
### PREPARE DATA FOR REGRESSION MODELLING

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

f_get.items = function (upc = "00-01-18200-53218") {
  
  setwd(paste(pth, "HEC/Data/iri category subsets",sep=""))  
  fc.items = readRDS("beer.top.upc.fc.items.rds")  
  fc.items
}


f_ts.regression.build.formula = function(model.id=0, model.name="", Lhs = "", Rhs = "")
{
  
  if (model.id > 0 | model.name != "" ) { 
    models.to.run <- data.table(read.xlsx(paste(pth, "HEC/Code/exp1/models to run.xlsx", sep=""), 1 , stringsAsFactors=F))
    if (model.id > 0) x = models.to.run[model_id == model.id,]
    if (model.name != "") x = models.to.run[model_name==model.name,]
    Rhs = paste(names(x)[x == 1],sep="",collapse=" + ")
    Lhs = "UNITS"
    this.formula = formula(paste(Lhs, " ~ ",Rhs,sep=""))
  } else {
    Rhs = paste(Rhs,sep="",collapse=" + ")
  }
  this.formula = paste(Lhs, " ~ ",Rhs,sep="")
  this.formula
}

f.ts.regression.cf.sig = function(cf) {
  cf.sig = cf[p.val <=0.05 & variable != "(Intercept)",c("variable","p.val"), with = FALSE]
  cf.sig.names = cf.sig[,c("variable"),with=FALSE]$variable
}


f_ts.diag.coef.effects = function(my.ec, opt.plot = FALSE) {
  
  # function to plot the relative effects of the explanatory variables
  # uses the coefficients (my.ec) from the regression model and the design matrix (dat.all)
  
  # requires cleaning for hard coded periods (313) and WEEK ********#
  
  estimates = my.ec[,list(variable,estimate)]
  mx.est = as.matrix(estimates$estimate)
  dat.ev = dat.all[,estimates$variable[-1], with=FALSE]
  dat.ev = data.table(cbind("(Intercept)" = rep(x=1,times=313),dat.ev))
  mx.ev = as.matrix(dat.ev)
  mx.est = do.call("rbind", rep(list(t(mx.est)), 313))
  mx.pred = mx.ev * mx.est
  head(mx.pred)
  
  week.end.dates = data.table(WEEK = 1:313,week_end_date = as.Date(calendar.weekly$week_end_date[1:313]))
  #mx.pred = cbind(as.Date(calendar.weekly$week_end_date[1:313]),mx.pred)
  mx.pred = merge(week.end.dates,data.table(WEEK = 1:313, mx.pred), by="WEEK")
  pred.melt = melt(mx.pred,id.vars=c("WEEK", "week_end_date"))  ;  names(pred.melt) = c("WEEK", "week.end.date","variable", "value")
  pred.melt = data.table(merge(pred.melt, variables))
  
  par(mfrow = c(1,1)) 
  ####### aggregate
  # TOP LEVEL: grp2
  pred.melt.group2 = pred.melt[,list(value = sum(value)),by=c("WEEK","week.end.date","grp2")]
  p1 = ggplot(data = pred.melt.group2, aes(x=week.end.date, y = value, colour = grp2)) + geom_line() + geom_point () +
    facet_wrap(~grp2) + theme_bw() +
    ggtitle("Contribution to units sold by component (top level)\n")
  if (opt.plot == TRUE) print(p1)
  p2 = ggplot(data = pred.melt.group2, aes(x=week.end.date, y = value, colour = grp2)) + 
    geom_line() + geom_point () + #scale_colour_brewer(type="qual", palette="Set1") +
    theme_bw() +
    ggtitle("Contribution to units sold by component (top level)\n")
  if (opt.plot == TRUE) print(p2)
  
  # MIDDLE LEVEL: grp1
  pred.melt.group1 = pred.melt[,list(value = sum(value)),by=c("WEEK","week.end.date","grp1")]
  p3 = ggplot(data = pred.melt.group1, aes(x=week.end.date, y = value, colour = grp1)) + geom_line() + geom_point () +
    facet_wrap(~grp1) + theme_bw() +
    ggtitle("Contribution to units sold by component (middle level)\n")
  if (opt.plot == TRUE) print(p3)
  p4 = ggplot(data = pred.melt.group1, aes(x=week.end.date, y = value, colour = grp1)) + 
    geom_line() + geom_point () + #scale_colour_brewer(type="qual", palette="Set1") +
    theme_bw() + theme_bw() +
    ggtitle("Contribution to units sold by component (middle level)\n")
  if (opt.plot == TRUE) print(p4)
  
  # VARIABLE LEVEL: display.name
  p5 = ggplot(data = pred.melt, aes(x=week.end.date, y = value)) + geom_line() + geom_point () +
    facet_wrap(~display.name) + theme_bw() +
    ggtitle("Contribution to units sold by explanatory variable\n")
  if (opt.plot == TRUE) print(p5)
  
  pred.melt.group2 = pred.melt.group2[, list(mean = mean(value, na.rm=TRUE), 
                                             sd = sd(value,na.rm=TRUE), 
                                             median = median(value,na.rm=TRUE),
                                             max = max(value,na.rm=TRUE),
                                             min = min(value,na.rm=TRUE)), 
                                      by = "grp2"]
  pred.melt.group1 = pred.melt.group1[, list(mean = mean(value, na.rm=TRUE), 
                                             sd = sd(value,na.rm=TRUE), 
                                             median = median(value,na.rm=TRUE),
                                             max = max(value,na.rm=TRUE),
                                             min = min(value,na.rm=TRUE)), 
                                      by = "grp1"]
  
  pred.melt = pred.melt[, list(mean = mean(value, na.rm=TRUE), 
                                             sd = sd(value,na.rm=TRUE), 
                                             median = median(value,na.rm=TRUE),
                                             max = max(value,na.rm=TRUE),
                                             min = min(value,na.rm=TRUE)), 
                                      by = c("grp1", "grp1", "variable", "display.name")]
  
  list(p1 = p1,p2 = p2, p3 = p3, p4 = p4, p5 = p5, pred.melt.group2, pred.melt.group1, pred.melt)
}

f_ts.properties.multivariate = function()
{
  ## using dat.all we need to look at the following stats for the following variables
  #vars.hols = grep("hol_", names(dat.all),value = TRUE)
  vars.feat = grep("FEAT_", names(dat.all),value = TRUE)
  vars.disp = grep("DISP_", names(dat.all),value = TRUE)
  properties.vars = c("UNITS","PRICE", "PRICE_DIFF",
                      vars.feat, vars.disp)
  
  ts.properties = 
  dat.all[, ldply(.SD, function(x) {
    n= length(x)
    na.count = sum(is.na(x))
    mean = mean(x,na.rm=TRUE)
    median = median(x, na.rm=TRUE)
    min = min(x, na.rm= TRUE)
    max = max(x, na.rm= TRUE)
    range = max - min
    sd = sd(x, na.rm=TRUE)
    cov = sd/mean
    data.table(fc.item, freq, na.count, n, mean, median, min, max, range, sd, cov)
  }), .SDcols = properties.vars ]
  setnames(ts.properties,".id","variable")
  ts.properties
}

f_ts.regression.elast = function (y.hat, variable.name, coef) {
  
  # needs to be generic enough to handle any variable
  # needs to be able to calculate individual elasticities per period?
  # needs to handle missing values
  
  Beta = coef[variable == variable.name,estimate, with = TRUE][1]
  x = dat.all[, eval(variable.name), with = FALSE][[variable.name]]  
  x.bar = mean(x,na.rm=TRUE)  #[(314 - length(mm$fitted.values)):313]
  y.bar = mean(y.hat)
  elast = Beta * (x.bar/y.bar)
  elast
}

f_ts.regression.elast.all = function(y.hat, coef)
{
  coef = coef[,list(variable.name, estimate), with = TRUE]
  to.match = c("PRICE", "FEAT", "DISP")
  #coef = coef[]
  grep(paste(to.match,collapse="|"), coef$variable.name, value=TRUE)
  coef[, with = TRUE][1]
  if (include.AR.terms == TRUE ) to.match = c("FEAT","DISP","UNITS_LAG")   else  to.match = c("FEAT","DISP")
  explanatory.variables <- unique (grep(paste(to.match,collapse="|"), names(dat.all), value=TRUE))
  
}




########  DIAGNOSTICS AND TESTING

f_ts.model.diag = function(my.model, dat.in=NULL, 
                           opt.plotModel = TRUE, opt.plot.Acf = TRUE, opt.plot.residual.hist = TRUE)
{
  #print(summary(my.model))
  #print(summary(aov(my.model)))  
  dat.in = dat.reg
  if (opt.plotModel == TRUE) {
    f_ts.plotModel(dat.in$UNITS,my.model, freq)
    par(mfrow = c(2,2),mar=c(4,4,4,4),oma=c(0,0,0,0))  
    #print(plot(my.model))
  }  
  if (opt.plot.Acf == TRUE) {
    par(mfrow = c(2,1),mar=c(3,4,3,3),oma=c(0,0,0,0))  
    Acf(my.model$residuals,lag.max=150)  
    pacf(my.model$residuals,lag.max=150) 
  }
  if (opt.plot.residual.hist == TRUE) {
    plot.new()
    par(mfrow = c(1,1))  
    x = data.frame(residuals = as.numeric(my.model$residuals))
    p = ggplot(data = x, aes(x=residuals)) +
      geom_histogram(aes(y=..density..), fill="red", alpha = 0.2, colour="black")+
      geom_density(colour="black") +
      ggtitle("Distribution of Residuals") + theme_bw() 
    print(p)  
  }
  
}

f_ts.model.regression = function(model = NULL,  #frm = 0,
                                 model.id = 0, model.name = "", k.optimal = NA,                                  
                                 opt.print.summary = TRUE, opt.print.aov = TRUE, opt.print.diag = TRUE,
                                 opt.print.stats = TRUE, opt.print.coef = TRUE,
                                 include.AR.terms = FALSE) {  

  
  # this function can take a model or a formula or a model name and will generate the stats and record the
  # coefficients for it.  Will also output the relevant information for the user.
  
  if (frm == 0 & is.null(model)) frm <<- f_ts.reg.build.formula (model.id, model.name)
  if (!is.null(model)) frm <<- model$call$formula   #print("\n===MODEL FORMULA===\n")  
  
  if (is.null(model)) my.model = lm(frm, data = dat.reg)    else     my.model = model  
  dv = all.vars(formula(frm))[1]
  y = dat.reg[,names(dat.reg) == dv, with = FALSE][[1]]
  
  
  ### RECORD THE ACCURACY STATS
  stats = f_ts.eval.accuracy.ext(model.lm = my.model, y = y)
  stats$average = mean(dat.reg$UNITS)
  stats = cbind(fc.item = fc.item,freq = freq,  #, chain = chain, store = store, 
                AR.terms = include.AR.terms,
                k = k.optimal,
                frm = as.character.formula(frm),
                stats)
  stats = data.table(stats)
  
  ### STORE THE COEFFICIENTS
  coef = f_ts.diag.coef.table (my.model = my.model)
  coef = cbind(fc.item = fc.item, freq = freq,  # chain = chain, store = store,
               AR.terms = include.AR.terms,
               frm = as.character.formula(frm),
               coef)
  coef = data.table(coef)
  
  ## STORE THE ELASTICITIES
  stats$elast = f_ts.regression.elast(y.hat=my.model$fitted.values,variable.name="PRICE",coef)
  
  
  ### PRINTING OPTIONS
  
  if (opt.print.summary == TRUE) { print("") ; print("===MODEL SUMMARY===")
                                   print("") ; print(paste("Include AR terms in the model:  ",include.AR.terms))
                                   print("") ; 
                                   print("") ; print(summary(my.model)) ; print("")  }
  if (opt.print.aov == TRUE) { print("\n===MODEL ANOVA TABLE===\n")
                               print(summary(aov(my.model$model))) }  
  
  if (opt.print.stats == TRUE) { print("") ; print("===MODEL STATS===")
                                 print("") ; print(stats[,names(stats)[-5],with=FALSE]) }  
  
  if (opt.print.coef == TRUE) { print("") ; print("\n===MODEL COEFFICIENTS===\n")
                                print("") ; print(coef[,names(coef)[-4], with = FALSE]) }
  
  list(model = my.model, coef = coef, stats = stats)
}


f_ts.regression.auto.formulae.get = function (k.optimal = 0, include.AR.terms = FALSE, 
                                              log.model=FALSE, price.terms = "PRICE") {

  ## This function will build the base and upper formulae for a regression model, based on the requested variable inclusions
  ## INTERCEPT INCLUSION?
  #if (intercept==TRUE) Rhs.intercept = "1" else Rhs.intercept = ""  

  # the BASE MODEL model consists of the Lhs plus the intercept plus and Fourier terms chosen
  if (log.model == TRUE) Lhs = "UNITS_LOG" else Lhs = "UNITS"
  if (k.optimal > 0) Rhs.fourier = f_ts.fourier.terms.for.formula(1:k.optimal)  else Rhs.fourier = ""
  if (k.optimal > 0) frm.base = paste (Lhs, "~ 1 +",Rhs.fourier) else  frm.base = paste (Lhs, " ~ 1")
  
  ## now get the EXPLANATORY VARIABLES to consider in the stepwise procedure as part of the UPPER MODEL
  ## match any of the terms in the variable names to warrant inclusion  
  if (include.AR.terms == TRUE ) to.match = c("FEAT","DISP","UNITS_LAG")   else  to.match = c("FEAT","DISP")
  explanatory.variables <- unique (grep(paste(to.match,collapse="|"), names(dat.all), value=TRUE))
  
  # this could be further improved to feed in defined subsets of variables into the model
  # perhaps from Excel or even define exclusion criteria instead
  # remove FEAT_NONE AND DISP_0 then remove the PRICE_DIFF terms (interaction variables only)
  explanatory.variables = explanatory.variables[explanatory.variables != "DISP_0" & explanatory.variables != "FEAT_NONE"]
  
  explanatory.variables = grep("PRICE_DIFF",explanatory.variables, value=TRUE, invert=TRUE)
  
  price.variables =
    switch(price.terms,
           PRICE_LAG = c("PRICE", "PRICE_LAG1", "PRICE_LAG2", "PRICE_LAG3"),
           PRICE = c("PRICE"),
           PRICE_DIFF = "PRICE_DIFF",# "PRICE_DIFF_LAG1", "PRICE_DIFF_LAG2", "PRICE_DIFF_LAG3"),
           PRICE_LOG = c("PRICE_LOG"),
           PRICE_LOG_LAG = c("PRICE_LOG", "PRICE_LOG_LAG1", "PRICE_LOG_LAG2", "PRICE_LOG_LAG3"))
  #if (price.variables == "PRICE") price.variables <- unique (grep("PRICE_DIFF", names(dat.all), value=TRUE))
  if (price.variables == "PRICE_DIFF") price.variables <- unique (grep("PRICE_DIFF", names(dat.all), value=TRUE))
  
  # add lags here to the price terms is required
  explanatory.variables = c(explanatory.variables, price.variables)
  Rhs.explanatory.variables = paste(explanatory.variables,sep="",collapse=" + ")    
  
  
  ## all HOLIDAY terms for formula
  Rhs.hols.names = names(dat.all)[grep("hol_",names(dat.all))]
  Rhs.hols = paste(Rhs.hols.names,sep="",collapse=" + ")
  
  ## merge the RHS terms into a single Rhs string with + signs to seperate terms
  frm.upper = paste(frm.base, Rhs.hols, Rhs.explanatory.variables,sep = " + ")
  print(frm.base);print(frm.upper)
  list(frm.base = frm.base, frm.upper = frm.upper)
}

f_ts.regression.auto = function(k.optimal, trace = 0, intercept = TRUE, 
                                       include.AR.terms = FALSE,
                                       log.model = FALSE,
                                       price.terms = "PRICE") {
  
  # get the lower and upper bounds of the formulae based on the variable inclusion parameters
  step.frm = f_ts.regression.auto.formulae.get(k.optimal,include.AR.terms = include.AR.terms,
                                               log.model = log.model,
                                               price.terms = price.terms)
  
  ## build the regression data set with all names variables in the RHS including
  ## thos in the formula to be employed
  dat.reg <<- f_ts.regression.data.reduce.formula (frm = step.frm$frm.upper, 
                                                   dat = dat.all) 
  dat.reg <<- na.omit(dat.reg)
  #if (Rhs.fourier != "")   base.model = lm(frm.base, dat.reg)  else base.model = lm(paste ("UNITS ~ ."), dat.reg) 
  base.model = lm(step.frm$frm.base, dat.reg)
  upper.model = lm(step.frm$frm.upper, dat.reg)
  
  #print(names(dat.reg)) ; print(nrow(dat.reg))
  ## use STEPWISE procedure stepAIC to get the model with the lowest AIC value
  model.stepAIC = stepAIC(object=base.model,trace=0, scope=list(lower=base.model, upper=upper.model))
  resReg = f_ts.model.regression(model = model.stepAIC,k.optimal=k.optimal,
                                 opt.print.aov=FALSE,opt.print.coef=FALSE, include.AR.terms = include.AR.terms)
  resReg
}

#=============================================
#              FUNCTIONS
#=============================================

f_ts.lagpad <- function(x, k) { c(rep(NA, k), x)[1 : length(x)]  }

f_ts.undifference = function() {}

f_ts.regression.data.reduce.formula = function(frm, dat) {
  model.vars = c(all.vars(formula(frm)))
  if (class(dat)[1]=="data.frame") dat = data.table(dat)
  dat[,c(model.vars),with = FALSE]  
}


f_ts.model.RegARIMA = function(dat.in, 
                               model.id=3, model.name="", frm = 0)
{
  model.name = "test_RegARIMA"
  if (frm == 0) frm = f_ts.reg.build.formula (model.id, model.name)
  as.character.formula(frm)
  xreg = unlist(strsplit(as.character(rhs(frm))," \\+ "))
  xreg = dat.in[,xreg[-1],with=F]
  names(xreg)
  #dat.in[,c("PRICE_DIFF"), with = TRUE] 
  fit <- auto.arima(dat.in$UNITS, xreg=xreg)
  fit1 <- Arima(dat.in$UNITS,order=c(2,0,0),xreg=xreg)  #  seasonal=list(order=c(1,0,0), period=52),
  sqrt(fit1$sigma2)
  tsdisplay(arima.errors(fit1), main="ARIMA errors")
  fit1
}


stepwise <- function(full.model, initial.model, dat.in, 
                     alpha.to.enter = 0.1, alpha.to.leave = 0.05) {
  
  # full.model is the model containing all possible terms
  # initial.model is the first model to consider
  # alpha.to.enter is the significance level above which a variable may enter the model
  # alpha.to.leave is the significance level below which a variable may be deleted from the model
  # (Useful things for someone to add: specification of a data frame; a list of variables that must be included)
  full <- lm(full.model, dat.in);  # fit the full model
  msef <- (summary(full)$sigma)^2;  # MSE of full model
  n <- length(full$residuals);  # sample size
  allvars <- attr(full$terms, "predvars");  # this gets a list of all predictor variables
  current <- lm(initial.model, dat.in);  # this is the current model
  while (TRUE) {  # process each model until we break out of the loop
    temp <- summary(current);  # summary output for the current model
    rnames <- rownames(temp$coefficients);  # list of terms in the current model
    print(temp$coefficients);  # write the model description
    p <- dim(temp$coefficients)[1];  # current model's size
    mse <- (temp$sigma)^2;  # MSE for current model
    cp <- (n-p)*mse/msef - (n-2*p);  # Mallow's cp
    fit <- sprintf("\nS = %f, R-sq = %f, R-sq(adj) = %f, C-p = %f",
                   temp$sigma, temp$r.squared, temp$adj.r.squared, cp);
    write(fit, file="");  # show the fit
    write("=====", file="");  # print a separator
    if (p > 1) {  # don't try to drop a term if only one is left
      d <- drop1(current, test="F");  # looks for significance of terms based on F tests
      pmax <- max(d[-1,6]);  # maximum p-value of any term (have to skip the intercept to avoid an NA value)
      if (pmax > alpha.to.leave) {
        # we have a candidate for deletion
        var <- rownames(d)[d[,6] == pmax];  # name of variable to delete
        if (length(var) > 1) {
          # if an intercept is present, it will be the first name in the list
          # there also could be ties for worst p-value
          # taking the second entry if there is more than one is a safe solution to both issues
          var <- var[2];
        }
        write(paste("--- Dropping", var, "\n"), file="");  # print out the variable to be dropped
        f <- formula(current);  # current formula
        f <- as.formula(paste(f[2], "~", paste(f[3], var, sep=" - ")));  # modify the formula to drop the chosen variable (by subtracting it)
        current <- lm(f, dat.in);  # fit the modified model
        next;  # return to the top of the loop
      }
    }
    # if we get here, we failed to drop a term; try adding one
    # note: add1 throws an error if nothing can be added (current == full), which we trap with tryCatch
    a <- tryCatch(add1(current, scope=full, test="F"), error=function(e) NULL);  # looks for significance of possible additions based on F tests
    if (is.null(a)) {
      break;  # there are no unused variables (or something went splat), so we bail out
    }
    pmin <- min(a[-1,6]);  # minimum p-value of any term (skipping the intercept again)
    if (pmin < alpha.to.enter) {
      # we have a candidate for addition to the model
      var <- rownames(a)[a[,6] == pmin];  # name of variable to add
      if (length(var) > 1) {
        # same issue with ties, intercept as above
        var <- var[2];
      }
      write(paste("+++ Adding", var, "\n"), file="");  # print the variable being added
      f <- formula(current);  # current formula
      f <- as.formula(paste(f[2], "~", paste(f[3], var, sep=" + ")));  # modify the formula to add the chosen variable
      current <- lm(f, dat.in);  # fit the modified model
      next;  # return to the top of the loop
    }
    # if we get here, we failed to make any changes to the model; time to punt
    break;
  } 
}



f_ts.model.regression.leaps = function() 
{  
  library(leaps)
  
  leaps<-regsubsets(y~x1+x2+x3+x4,data=mydata,nbest=10)
  # view results
  summary(leaps)
  # plot a table of models showing variables in each model.
  # models are ordered by the selection statistic.
  plot(leaps,scale="r2")
  # plot statistic by subset size
  library(car)
  subsets(leaps, statistic="rsq") 
}

f_ts.model.step = function (model_id=2) {
  #dat.in
  models.to.run <- data.table(read.xlsx(paste(pth, "HEC/Code/exp1/models to run.xlsx", sep=""), 1 , stringsAsFactors=F))
  x = models.to.run[model_id,]
  frm = f_ts.reg.build.formula(x)
  initial.model = lm(frm,data = dat.in)
  #slm = step(object=initial.model,direction = "both")
  
}
 


## OBTAIN THE COS AND SIN VALUES AS A MATRIX

zf_ts.fourier.terms.for.formula = function(cf.sig.periods)
{
  Rhs = paste("SIN_", cf.sig.periods,sep="")
  Rhs = c(Rhs,paste("COS_", cf.sig.periods,sep=""))
  Rhs = paste(Rhs,sep="",collapse=" + ")
  Rhs  
}



zaccuracy.ext = function (f, x, test = "all") 
{
  if (!missing(x)) 
    return(forecasterrors(f, x, test))
  if (!is.list(f)) 
    stop("If no x argument, then f must be a forecast object or a time series model object.")
  f$x <- getResponse(f)
  ff <- f$x
  fits <- fitted(f)
  res <- ff - fits
  if (is.numeric(test)) {
    res <- res[test]
    ff <- ff[test]
  }
  pe <- res/ff * 100
  out <- c(mean(res, na.rm = TRUE), sqrt(mean(res^2, na.rm = TRUE)), 
           mean(abs(res), na.rm = TRUE), mean(pe, na.rm = TRUE), 
           mean(abs(pe), na.rm = TRUE))
  names(out) <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  if (!is.null(tsp(f$x))) {
    scale <- mean(abs(diff(f$x)), na.rm = TRUE)
    out <- c(out, mean(abs(res/scale), na.rm = TRUE))
    names(out)[6] <- "MASE"
  }
  return(out)
}



f_ts.regression.diag.accuracy.stats = function(my.model)
#  in-sample (fit) stats for a model of lm class
{
  ss = summary(my.model)
  #acc = accuracy(my.model$fitted.values, dat.reg$UNITS)  
  acc = accuracy(my.model)
  stats = data.table(matrix(unlist(acc),nrow=1,dimnames=list(NULL,colnames(acc))))
  
  stats$aic = AIC(my.model)    # -2*logLik(my.model)+2*(length(my.model$coef)+1)  
  stats$r.squared = ss$r.squared
  stats$adj.r.squared = ss$adj.r.squared
  stats$sigma = ss$sigma
  
  stats
}




# 
# naive.res = (c(NA,diff(y)))      #naive.res = naive(y)$residuals
# #snaive.res = snaive(ts(y,frequency=seas.period))$residuals
# mean(abs(naive.res), na.rm=TRUE)
# mean(abs(snaive.res))
# MASE.dt = data.table(cbind(y,res = abs(res),naive = abs(naive.res)))
# dt = MASE.dt
# dt$rel = dt$res/dt$naive
# mean(dt$rel,na.rm=TRUE)
# 

#   if (is.numeric(test)) {
#     res <- res[test]
#     ff <- ff[test]
#   }


#   if (!missing(x)) 
#     return(forecasterrors(f, x, test))
#   if (!is.list(f)) 
#     stop("If no x argument, then f must be a forecast object or a time series model object.")
#   
#   f$x <- getResponse(f)
#   


#names(acc) <- c("ME", "RMSE", "MAE", "MPE", "MAPE")

#   if (!is.null(tsp(f$x))) {
#     scale <- mean(abs(diff(f$x)), na.rm = TRUE)
#     out <- c(out, mean(abs(res/scale), na.rm = TRUE))
#     names(acc)[6] <- "MASE"
#   }



# library(forecast)
# 
# fcast <- snaive(window(USAccDeaths,end=1977.99))
# 
# accuracy(fcast$mean,USAccDeaths)
# fcast$fitted
# fcast$x
# 
# trainingdata <- window(Canada, end=c(1998,4))
# testdata <- window(Canada, start=c(1999,1))
# v <- VAR(trainingdata, p=2)
# p <- predict(v, n.ahead=8)
# res <- residuals(v)
# fits <- fitted(v)
# for(i in 1:4)
# {
#   fc <- structure(list(mean=p$fcst[[i]][,"fcst"], x=trainingdata[,i],
#                        fitted=c(NA,NA,fits[,i])),class="forecast")
#   print(accuracy(fc,testdata[,i]))
# }



# 
# ## first get the EXPLANATORY VARIABLES to consider in the stepwise procedure
# ## match any of the terms in the variable names to warrant inclusion
# if (include.AR.terms == TRUE ) to.match = c("FEAT","DISP","PRICE", "UNITS_LAG")   else  to.match = c("FEAT","DISP","PRICE")
# explanatory.variables <- unique (grep(paste(to.match,collapse="|"), 
#                                       names(dat.all), value=TRUE))
# # this could be further improved to feed in defined subsets of variables into the model
# # perhaps from Excel or even define exclusion criteria instead
# explanatory.variables = explanatory.variables[explanatory.variables != "DISP_0" & explanatory.variables != "FEAT_NONE"]
# Rhs.explanatory.variables = paste(c("weeks.in.period",explanatory.variables),sep="",collapse=" + ")    
# 
# if (log.model == TRUE) Lhs = "UNITS_LOG" else Lhs = "UNITS"
# 
# # INTERCEPT INCLUSION?
# #if (intercept==TRUE) Rhs.intercept = "1" else Rhs.intercept = ""  
# ## optimal FOURIER terms for model
# if (k.optimal > 0) Rhs.fourier = f_ts.fourier.terms.for.formula(1:k.optimal)  else Rhs.fourier = ""
# if (k.optimal > 0) frm.base = paste (Lhs, " ~ 1 +",Rhs.fourier) else  frm.base = paste (Lhs, " ~ 1")
# 
# ## all HOLIDAY terms for formula
# Rhs.hols.names = names(dat.all)[grep("hol_",names(dat.all))]
# Rhs.hols = paste(Rhs.hols.names,sep="",collapse=" + ")
# 
# ## merge the RHS terms into a single Rhs string with + signs to seperate terms
# frm.upper = paste(frm.base, Rhs.hols, Rhs.explanatory.variables,sep = " + ")
# 
# ## build the regression data set with all names variables in the RHS including
# ## thos in the formula to be employed
# dat.reg <<- f_ts.regression.data.reduce.formula (frm.upper, dat.all) 
# dat.reg <<- na.omit(dat.reg)
# #if (Rhs.fourier != "")   base.model = lm(frm.base, dat.reg)  else base.model = lm(paste ("UNITS ~ ."), dat.reg) 
# base.model = lm(frm.base, dat.reg)
# upper.model = lm(frm.upper, dat.reg)




#COS_SIN = f_ts.seas.harmonics ()

  
  #
  # This is an R function to perform stepwise regression based on a "nested model" F test for inclusion/exclusion
  # of a predictor.  To keep it simple, I made no provision for forcing certain variables to be included in
  # all models, did not allow for specification of a data frame, and skipped some consistency checks (such as whether
  # the initial model is a subset of the full model).
  #
  # One other note: since the code uses R's drop1 and add1 functions, it respects hierarchy in models. That is,
  # regardless of p values, it will not attempt to drop a term while retaining a higher order interaction
  # involving that term, nor will it add an interaction term if the lower order components are not all present.
  # (You can of course defeat this by putting interactions into new variables and feeding it what looks like
# a first-order model.)
#
# Consider this to be "beta" code (and feel free to improve it).  I've done very limited testing on it.
#
# Author: Paul A. Rubin (rubin@msu.edu)
#



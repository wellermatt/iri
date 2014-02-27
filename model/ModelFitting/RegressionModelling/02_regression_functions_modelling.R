library(MASS)


## ============================== SEASONALITY & FOURIER TERMS ===================================

f_ts.fourier.terms.for.formula = function(k)
# this will build the SIN_ and COS_ terms for the specified number of terms (k) and seperate them with +
# should check/test whether including solely significant individual values would be better 
#
#  
{
	rhs = paste(c(paste("SIN", 1:k, sep = "_"), paste("COS", 1:k, sep = "_")), collapse = "+") 
	rhs
}

f_ts.regression.fourier.k.test = function(k, dt, p.include = 0.05) 
  
  # tests a specific data set containing harmonic variables using a formula 
  # containing k SIN/COS terms for significance of the last term
  # enhance to accept only the y-values
{  
	rhs = f_ts.fourier.terms.for.formula(k)
  frm = paste("UNITS ~ ", rhs)  
        
	# extract the significant coefficents and identify whether the last term added is significant
	coef(lm(frm, data = dt))
  coef = f_ts.diag.coef.table(lm(frm, data = dt))
	
  sig.fourier = coef[p.val < p.include & variable !="(Intercept)"]$variable
	cf.sig.periods = length(as.numeric(unique(sapply(strsplit(sig.fourier, "_"), "[[", 2))))
	significant.term = FALSE
	if (k == cf.sig.periods) significant.term = TRUE
	significant.term
}


f_ts.regression.fourier.k.optimise = function(dt) 
#  determine the optimal number of harmonic terms for a specific dataset 
#  with the harmonic terms already included
{
	max.k = max(as.integer(gsub("SIN_", "", grep("SIN_", names(dt), value = TRUE))))
	for (k in 1:max.k) {
		significant.term = f_ts.regression.fourier.k.test(k=k, dt = dt)
		if (significant.term == FALSE) break
	}
	k.opt = k - 1
	k.opt
}

## ========================== VARIABLE SELECTION (model input)  ===========================


f_ts.regression.auto.formulae.scope = function (dt, k.optimal)
  
  
  # formulae.options = list(k.optimal = 0, intercept = TRUE, include.AR.terms = FALSE, log.model=FALSE, price.terms = "PRICE")) 
## This function will build the base and upper formulae for a regression model, based on the requested variable inclusions
{
  	## INTERCEPT INCLUSION?
	## if (intercept==TRUE) Rhs.intercept = "1" else Rhs.intercept = ""  
	#for(i in names(formulae.options)) assign(i,formulae.options[[i]])
	#print(formulae.options)
	
	# the BASE MODEL model consists of the Lhs plus the intercept plus and Fourier terms chosen
	if (log.model == TRUE) Lhs = "UNITS_LOG" else Lhs = "UNITS"
	if (k.optimal > 0) Rhs.fourier = f_ts.fourier.terms.for.formula(k.optimal)  else Rhs.fourier = ""
	if (k.optimal > 0) frm.base = paste (Lhs, "~ 1 +",Rhs.fourier) else  frm.base = paste (Lhs, " ~ 1")

	## now get the EXPLANATORY VARIABLES to consider in the stepwise procedure as part of the UPPER MODEL
	## match any of the terms in the variable names to warrant inclusion  
	if (include.AR.terms == TRUE ) to.match = c("FEAT","DISP","UNITS_LAG")   else  to.match = c("FEAT","DISP")
	explanatory.variables <- unique (grep(paste(to.match,collapse="|"), names(dt), value=TRUE))

	# this could be further improved to feed in defined subsets of variables into the model
	# perhaps from Excel or even define exclusion criteria instead
	# remove FEAT_NONE AND DISP_0 then remove the PRICE_DIFF terms (interaction variables only)
	explanatory.variables = explanatory.variables[explanatory.variables != "DISP_ANY" & explanatory.variables != "FEAT_ANY"]

	explanatory.variables = grep("PRICE_DIFF",explanatory.variables, value=TRUE, invert=TRUE)

	price.variables =
	switch(price.terms,
		   PRICE_LAG = c("PRICE", "PRICE_LAG1", "PRICE_LAG2"),# "PRICE_LAG3"),
		   PRICE = c("PRICE"),
		   PRICE_DIFF = unique (grep("PRICE_DIFF", names(dt), value=TRUE)),  #"PRICE_DIFF",# "PRICE_DIFF_LAG1", "PRICE_DIFF_LAG2", "PRICE_DIFF_LAG3"),
		   PRICE_LOG = c("PRICE_LOG"),
		   PRICE_LOG_LAG = c("PRICE_LOG", "PRICE_LOG_LAG1", "PRICE_LOG_LAG2"))#, "PRICE_LOG_LAG3"))
	#if (price.variables == "PRICE") price.variables <- unique (grep("PRICE_DIFF", names(dat.all), value=TRUE))
	#if (price.variables == "PRICE_DIFF") price.variables <- unique (grep("PRICE_DIFF", names(dt), value=TRUE))
	price.variables = grep("PRICE_DIFF_LAG3|PRICE_DIFF_LAG4",price.variables, value = TRUE, invert = TRUE)
	
	cat(paste("Price Vars ",price.variables,"\n")) 	
	cat(paste("Explanatory Vars ",explanatory.variables,"\n")) 
	
	# add lags here to the price terms is required
	explanatory.variables = c(explanatory.variables, price.variables)
	Rhs.explanatory.variables = paste(explanatory.variables,sep="",collapse=" + ")    

	## all HOLIDAY terms for formula
	Rhs.hols.names = names(dt)[grep("hol_",names(dt))]
	Rhs.hols = paste(Rhs.hols.names,sep="",collapse=" + ")

	## merge the RHS terms into a single Rhs string with + signs to seperate terms
	frm.upper = paste(frm.base, Rhs.hols, Rhs.explanatory.variables,sep = " + ")
	#print(frm.base);print(frm.upper)
	list(frm.base = as.formula(frm.base), frm.upper = as.formula(frm.upper))
}

f_ts.regression.data.reduce.formula = function(dt, frm) 
# this function is probably not required given that we already have the necessary fields in dt for formulae between which to step
{
  model.vars = c(all.vars(frm))    ## allow for period_id ****
  #if (class(dt)[1]=="data.frame") dat = data.table(dat)
  dt[,c(model.vars),with = FALSE]  
}

## ======================== RUN THE stepAIC =========================

# model.opts:
#  intercept = TRUE
#  include.AR.terms = FALSE
#  log.model = FALSE ... if TRU then which variables to log? 
#  price.terms = "PRICE", "PRICE_DIFF", "PRICE_LOG"?, "PRICE_REF", "DISC_PERC"
#  lags of explanatory variables?
#  combinations of explanatory variables: e.g. HOLS x FEAT x DISP x DISCOUNT
#  princomp of promotional variables/holiday variables (pooling variables/reducing dimensionality)

f_ts.regression.auto.stepAIC = function(dt, print.details = 0,
             model.opts = list(intercept = TRUE, include.AR.terms = FALSE, 
             log.model = FALSE, price.terms = "PRICE"))   
  
  ## this is the stepwise function which will accept a data.table in the format:
{
  
	# get the lower and upper bounds of the formulae based on the variable inclusion parameters
	k.optimal = f_ts.regression.fourier.k.optimise(dt) 
	step.frm = f_ts.regression.auto.formulae.scope(dt, k.optimal)
	
	## build the regression data set with all names variables in the RHS including including those in the formula to be employed
	dt.reg <<- f_ts.regression.data.reduce.formula (dt, frm = step.frm$frm.upper) 
	dt.reg <<- na.omit(dt.reg) ## this can be an issue with time series forecasting!!
	
  
	base.model = lm(step.frm$frm.base, dt.reg)
	upper.model = lm(step.frm$frm.upper, dt.reg)

  
	## use STEPWISE procedure stepAIC to get the model with the lowest AIC value
	out.model = stepAIC(object = base.model, trace = 0,
                      scope = list(lower = base.model, upper = upper.model))
  
  
  # maybe extend here a little?
  # p-values to enter/leave, GLS to estimate model (due to autocorrelations)
  # choose alternative to AIC
  # optimising on unseen data
  if (print.details == 1)  f_ts.regression.model.summarise(out.model)
    
  out.model
}


f_ts.regression.model.summarise = 
    function(my.model = NULL, include.AR.terms = FALSE,
             print.options = list(opt.print.summary = TRUE, opt.print.aov = TRUE,
                                  opt.print.diag = TRUE, opt.print.stats = TRUE, 
                                  opt.print.coef = TRUE) )
  
# this function can take a model and will calculate accuracy stats, coefficients, elasticities for it.  
# Will also output the relevant information for the user.
      
{    
  ### RECORD THE ACCURACY STATS FROM FIT SAMPLE
      # may need rework
  stats = f_ts.eval.accuracy.lm(model.lm = my.model) #, y = y)
  stats$average = mean(df$UNITS)
  stats = cbind(freq = freq,  #, chain = chain, store = store, 
                AR.terms = include.AR.terms,
                k = k.optimal,
                frm = as.character.formula(frm),
                stats)
  stats = data.table(stats)
  
  ### STORE THE COEFFICIENTS FROM FIT 
  # consider reviewing the trace from stepAIC
  coef =  f_ts.diag.coef.table(my.model = my.model)
  coef = cbind(freq = freq,  # chain = chain, store = store,
               AR.terms = include.AR.terms,
               frm = as.character.formula(frm),
               coef)
  coef = data.table(coef)
  
  ## STORE THE ELASTICITIES FOR INITIAL PARAMTER ESTIMATES
  stats$elast = f_ts.regression.elast(y.hat = my.model$fitted.values, variable.name="PRICE", coef)
    
  ### PRINTING OPTIONS
  
  if (print.options$opt.print.summary == TRUE) { print("") ; print("===MODEL SUMMARY===")
                                   print("") ; print(paste("Include AR terms in the model:  ",include.AR.terms))
                                   print("") ; 
                                   print("") ; print(summary(my.model)) ; print("")  }
  if (print.options$opt.print.aov == TRUE) { print("\n===MODEL ANOVA TABLE===\n")
                               print(summary(aov(my.model$model))) }  
  
  if (print.options$opt.print.stats == TRUE) { print("") ; print("===MODEL STATS===")
                                 print("") ; print(stats[,names(stats)[-5],with=FALSE]) }  
  
  if (print.options$opt.print.coef == TRUE) { print("") ; print("\n===MODEL COEFFICIENTS===\n")
                                print("") ; print(coef[,names(coef)[-4], with = FALSE]) }
  
  list(model = my.model, coef = coef, stats = stats)
}

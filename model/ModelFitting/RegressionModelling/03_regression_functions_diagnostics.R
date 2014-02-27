library(forecast)  ; library(plyr)

f_ts.eval.accuracy.lm = function (model.lm = NULL, y = NULL, y.hat = NULL, res = NULL, test = "all", snaive.lag = 1) 

# calculate accuracy stats for a model of type lm
{
  if (is.null(y.hat)) y.hat = model.lm$fitted
  if (is.null(y)) y = model.lm$model[[1]]   # hope this works!!!
  
  #if (seas.period > 1) y = ts(y,seas.period)
  
  res <- (y - y.hat)
  pe <- res/y * 100    
  
  scale <- mean(abs(diff(y)), na.rm = TRUE)
  MASE = mean(abs(res/scale), na.rm = TRUE)
  #mean(abs(res)/abs(naive.res), na.rm=TRUE)
  acc <- data.table(ME = mean(res, na.rm = TRUE), 
                    RMSE = sqrt(mean(res^2, na.rm = TRUE)), 
                    MAE = mean(abs(res), na.rm = TRUE), 
                    MPE = mean(pe, na.rm = TRUE), 
                    MAPE = mean(abs(pe), na.rm = TRUE),
                    MASE = MASE)
  if (!is.null(model.lm)) {
    acc$aic = AIC(model.lm)    # -2*logLik(my.model)+2*(length(my.model$coef)+1)  
    ss = summary(model.lm)
    acc$r.squared = ss$r.squared
    acc$adj.r.squared = ss$adj.r.squared
    acc$sigma = ss$sigma  
  }
  #if (!is.null(model.tbats)) {
    #acc$aic = model.tbats$AIC    # -2*logLik(my.model)+2*(length(my.model$coef)+1)  
    ##acc$r.squared = ss$r.squared
    ##acc$adj.r.squared = ss$adj.r.squared
    #acc$variance = model.tbats$variance
  #}  
  acc
}


f_ts.diag.coef.table = function(my.model, opt.elasticity = TRUE, fc.item = "NA", id = 0) 

# builds a data.table object of the coefficients in the final model, 
# including p-values, estimates, t-values, significance and standard errors
{  
	coef = summary(my.model)$coefficients
	coef = data.table(fc.item = fc.item, id = id, variable = rownames(coef), coef)
	setnames(coef,c("fc.item", "exp.id", "variable", "estimate", "std.err", "t.val", "p.val"))    #)names(coef), 
	coef$signif = symnum(coef$p.val, 
                          corr = FALSE,
                          cutpoints = c(0,  .001,.01,.05, .1, 1),
                          symbols = c("***","**","*","."," "))
	# ELASTICITY CALCS
	if (opt.elasticity == TRUE) {
		y.bar = mean(my.model$fitted.values, na.rm = TRUE)
		my.model.data = data.table(my.model$model)
		coef$elasticity = as.numeric(NA)							# necessary to add the column and set it to NA first, in case model doesn't contain any of the below
		coef[grepl("PRICE|DISP|FEAT", variable) , 
			elasticity := f_ts.regression.elast(y.bar = y.bar, my.model.data, variable,Beta = estimate)]
	}
	coef
}

f_ts.regression.elast = function (y.bar, model.data, variable, Beta) 
  
  # needs to be generic enough to handle any variable
  # needs to be able to calculate individual elasticities per period?
  # needs to handle missing values
{  
  x = model.data[, eval(variable), with = FALSE][[1]]
  x.bar = mean(x ,na.rm=TRUE) 
  elast = Beta * (x.bar/y.bar)
  elast
}

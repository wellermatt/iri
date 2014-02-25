rm(list=ls())


f_weekly.split.matrix = function() {

    library(foreach)
    cal.periods = rep(x=c(4,4,5), times = 4)
    tm = matrix(0, length(cal.periods),sum(cal.periods))
    
    foreach (pd = 1:length(cal.periods)) %do%
    {
        end = sum(cal.periods[1:pd])
        start = end - cal.periods[pd] + 1
        
        tm[pd, start:end] = 1/cal.periods[pd]
    }
    tm
}


fcast.445 = as.integer(rnorm(n=12, mean=1000, sd=250))
ts.plot(ts(fcast.445))

tm = f_weekly.split.matrix()
weekly.fcast = fcast.445 %*% tm

ts.plot(ts(as.vector(weekly.fcast)))
#rm(list=ls())


f_weekly.split.matrix = function(o, h) {

    library(foreach)
    
    # requires the origin o to be at the end of a 445 period
    # data by 445 period starting at week following o for n periods        
    cal.periods = calendar.445$weeks.in.period[(o+1):(o+h)]
    
    tm = matrix(0, length(cal.periods),sum(cal.periods))
    
    foreach (pd = 1:length(cal.periods)) %do%
    {
        end = sum(cal.periods[1:pd])
        start = end - cal.periods[pd] + 1
        tm[pd, start:end] = 1/cal.periods[pd]
    }
    tm
}

#  testing split from 445 to weekly
# get a random series in 445 mode
x445 = as.integer(rnorm(n=3, mean=1000, sd=250))  ; ts.plot(ts(x445))

# make the split matrix
tm = f_weekly.split.matrix(o = 48, h = 3)

# split out the 445 to weekly via the transition (disagg) matrix
weekly.fcast = x445 %*% tm


ts.plot(ts(as.vector(weekly.fcast)))

# check against original values via reversed/inversed? transition mtrix (agg)
tm.agg = (1*tm>0)
check.445 = weekly.fcast %*% t(tm.agg)

identical(as.numeric(check.445), as.numeric(x445))

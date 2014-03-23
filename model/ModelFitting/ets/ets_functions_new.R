

# need generic functions to handle time series of varying lengths now...
# this procedure is very much lined up for monthly forecasting at present
# 


f_ets.rolling.multicore = function(sp, par.category, freq = 12, h.max=3, opt.dopar = "do", cores=6, Trace = TRUE)
{
    # function will take an input set of data (sp) for multiple forecast items and save/return a set of forecasts
    sp[,fc.item := factor(fc.item)]   ;   setkeyv(sp, c("fc.item"))  #,"period_id"))
    print(paste(length(levels(sp$fc.item)), "forecast items to process"))
    
    export.list = c("f_ets.run.item","f_ets.roll.fc.item", "f_load.calendar", "pth.dropbox.data", "dtcomb", "isplitDT", "h.max", "freq")
    
    
    library(doParallel)
    #if (opt.dopar =="dopar") registerDoParallel(cores)
    library(doSNOW)
    cl <- makeCluster(cores, outfile="")
    registerDoSNOW(cl)
    
    multi.item.results =
        foreach(dt.sub = isplitDT(sp, levels(sp$fc.item)),
                .combine='dtcomb', .multicombine=TRUE,
                .export = export.list,
                .packages=c("data.table", "forecast", "reshape2", "foreach")) %dopar% {
                    fc.item = dt.sub$key[1]
                    print(fc.item)
                    this.roll = f_ets.run.item(dt.sub$value, freq = freq, h.max = h.max,Trace=TRUE)
                    this.roll$fc.item = fc.item
                    this.roll
                }
    print(head(multi.item.results,10))
    
    # create a file name and save the results
    setwd(pth.dropbox.data)         
    fil = paste0("./output/errors/ets_", freq, "_", par.category, ".rds")
    saveRDS(object = multi.item.results, file = fil)
    
    multi.item.results
}



f_ets.roll.fc.item = function(y, h.max, PRINT = FALSE, reoptimise = FALSE, forecast.cycle = "monthly") 
{    
    # runs rolling origin multi-step forecasting for a single time series
    # rules around length of series (n), horizon (h), splitting rules
    freq = tsp(y)[3]
    n <- length(y)
        
    if (freq == 12) o1 <- 48             # minimum data length for fitting a model
    if (freq == 52) o1 <- 208             # minimum data length for fitting a model
    
    # fit model to the test set: use HoltWinters for weekly and ets for monthly
    st <- tsp(y)[1] + (o1-1)/freq                # st = start period (as of) in ts terms
    yhist <- window(y, end = st)
    fit.original <- if (freq == 12) {
        ets(yhist, model="ZZZ")  # model="MMM",
    } else {
        HoltWinters(yhist)
    }
    
    # origins are the time points where forecasts are to be generated and should be in the correct scale (weeks or months)
    # by default we assume the cycle is monthly/445 and the frequency = monthly
    f_load.calendar()
    origins = calendar.445[period_id >= o1 & period_id < n, c(period_id)]
    
    if (forecast.cycle == "weekly") origins = calendar.weekly[WEEK >= o1 & WEEK < n, WEEK]
    if (forecast.cycle == "monthly" & freq==52) origins = calendar.445[elapsed_weeks >= o1 & elapsed_weeks < n,  elapsed_weeks]
    
    roll.ets = foreach(o = origins, .combine=dtcomb) %do%
    {
        #print(o)
        yhist <- window(y, end = st + (o-o1)/freq)
        h = min(h.max, n - o)
        yfuture <- window(y, start = st + (o-o1+1)/freq , end = st + (o-o1+h)/freq)
        
        # use ets or  HoltWinters to re-fit the model.
        if (reoptimise != TRUE) {
            fit <- if(freq == 12) {
                ets(yhist, model=fit.original)
            } else {
                HoltWinters(yhist)
            }
        } else {
            fit <- if(freq == 12) {
                fit <- ets(yhist, model="ZZZ")
            } else {
                HoltWinters(yhist)
            }
        }
        # do the forecasts,      
        fcast <- forecast(fit, h = h)
        
        # optional printing
        if (PRINT == TRUE) {
            print(fit)
            #print(yhist) 
            #print(yfuture)
            #print(fcast)
        }
        data.table(o, h = 1:length(yfuture), y = yfuture, yhat = fcast[['mean']])
    }
    
    return(roll.ets)   # may also want to return some details from the model e.g. alpha, beta, gamma, phi)
}


f_ets.run.item = function(ss, freq, h.max, Trace = FALSE)  #fc.item = "00-01-18200-53030", pth = NULL)
{    
    y = ts(ss$UNITS, start=c(2001,1), frequency = freq)
    roll = f_ets.roll.fc.item(y, h.max = h.max)
    roll$fc.item = unique(ss$fc.item)
    
    if (Trace == TRUE) { print(roll$fit) ; print(roll$Err) }
    roll
}



f_ets.test.single = function(sp, freq = 12, h.max=3, Trace = TRUE) {
    i = 1
    items = unique(sp$fc.item)
    this.item = items[i]
    ss = sp[fc.item == this.item]
    #print(ss)
    this.roll = f_ets.run.item(ss=ss, freq = freq, h.max=h.max,Trace=TRUE)
    this.roll
}




f_ets.test.multi = function(sp, freq = 12, h.max=3, Trace = TRUE) {
    items = unique(sp$fc.item)
    multi.item.results = rbindlist(
        lapply(1:10,#length(items),
               function(i) { this.item = items[i]
                             print(this.item)
                             ss = sp[fc.item == items[i]]
                             this.roll = f_ets.run.item(ss, freq = 12, h.max = 3,Trace=TRUE)
                             Err = this.roll$Err
                             Err$fc.item = this.item
                             Err
               }))
    setwd(pth.dropbox.data)
    saveRDS(object = multi.item.results, file="./output/errors/ets_445_fast_all.rds")
    
    multi.item.results
}



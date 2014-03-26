

# need generic functions to handle time series of varying lengths now...
# this procedure is very much lined up for monthly forecasting at present
# 


f_ets.rolling.multicore = function(sp, par.category, 
                                   freq = 12, freq.cycle = 12, h.max=3,
                                   cores=1, parMethod = "doSNOW", Trace = FALSE)
{
    # function will take an input set of data (sp) for multiple forecast items and save/return a set of forecasts
    sp[,fc.item := factor(fc.item)]   ;   setkeyv(sp, c("fc.item"))  #,"period_id"))
    print(paste(length(levels(sp$fc.item)), "forecast items to process"))
    
    f_load.calendar()
    export.list = c("f_ets.run.item","f_ets.roll.fc.item", "calendar.weekly", "calendar.445", "pth.dropbox.data", 
                    "dtcomb", "isplitDT", "h.max", "freq", "freq.cycle")
        
#     if (cores>1) {
#         # library(doParallel)  ;  registerDoParallel(cores)
#         library(doSNOW)
#         cl <- makeCluster(cores, outfile="")
#         registerDoSNOW(cl)
#     }
    library(doParallel)
    #if (opt.dopar =="dopar") registerDoParallel(cores)
    #library(doSNOW)
    cl <- makeCluster(cores, outfile="")
    registerDoParallel(cl)
    print(paste("**** Cluster started with", cores, "cores"))
    
    multi.item.results =
        foreach(dt.sub = isplitDT(sp, levels(sp$fc.item)),
                .combine='dtcomb', .multicombine=TRUE,
                .verbose = FALSE, .errorhandling = "remove",
                .export = export.list,
                .packages=c("data.table", "forecast", "reshape2", "foreach")) %dopar% {
                    fc.item = dt.sub$key[1]
                    print(fc.item)
                    this.roll = f_ets.run.item(dt.sub$value, freq = freq, freq.cycle = freq.cycle, h.max = h.max, Trace=Trace)
                    this.roll = data.table(fc.item, this.roll)
                    this.roll
                }
    stopCluster(cl)
    
    print("=== END MULTICORE LOOP ====")
    print(multi.item.results)

    multi.item.results
}



f_ets.run.item = function(sp1, freq, h.max, freq.cycle, Trace = FALSE)  #fc.item = "00-01-18200-53030", pth = NULL)
{    
    y = ts(sp1$UNITS, start=c(2001,1), frequency = freq)
    
    if (sum(is.na(y))>0) y = ts(na.approx(y, na.rm=FALSE), start = c(2001,1), freq=freq)
    if (sum(is.na(y))>0) y= ts(na.locf(na.locf(y,fromLast=TRUE, na.rm=FALSE),na.rm=FALSE),start = c(2001,1), freq=freq)
    
    # print(y)
    
    roll = f_ets.roll.fc.item(y, h.max = h.max, freq.cycle=freq.cycle)
    #roll$fc.item = unique(ss$fc.item)
    
    if (Trace == TRUE) { print(roll$fit) ; print(roll$Err) }
    roll
}


f_ets.roll.fc.item = function(y, freq.cycle, h.max, PRINT = FALSE, reoptimise = FALSE) 
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
    #f_load.calendar()
    
    # set the origins based on the forecasting cycle (freq.cycle)
    if (freq == 12) { 
        origins = calendar.445[period_id >= o1 & period_id < n, c(period_id)]
    } else {
        if (freq.cycle == 52)  {
            origins = calendar.weekly[WEEK >= o1 & WEEK < n, WEEK]
        } else {
            origins = calendar.445[elapsed_weeks >= o1 & elapsed_weeks < n,  elapsed_weeks]
        }        
    }
    #origins = f_origins.get()
    
    roll.ets = foreach(o = origins, .combine=dtcomb, .verbose=FALSE, .errorhandling = "stop") %do%
    {
        print(o)
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
                ets(yhist, model="ZZZ")
            } else {
                HoltWinters(yhist)
            }
        }
        # do the forecasts,      
        fcast <- as.numeric(forecast(fit, h = h)$mean)
        
        # get the in-sample (rolling) 1-step naive erros (or 1 cycle ahead errors)
        mae.naive = mean(abs(diff(yhist[1:o], 1)), na.rm = TRUE)
        mae.snaive = mean(abs(diff(yhist[1:o], freq)), na.rm = TRUE)
        
        # do we need to test for missing values here as we did with the regression??
        data.table(o, 
                   h = 1:length(yfuture), 
                   t = o + 1:length(yfuture), 
                   fc = fcast, 
                   act = as.numeric(yfuture),
                   fc.naive = rep(yhist[o], h),
                   fc.snaive = yhist[(o-freq+1):(o-freq+h)],
                   mae.naive = mae.naive,
                   mae.snaive = mae.snaive)
    }
    
    # optional printing
    if (TRUE == TRUE) {
        print(fit)
        print(accuracy(fit))
    }
    
    return(roll.ets)   # may also want to return some details from the model e.g. alpha, beta, gamma, phi)
}


##====== EOF: redundant functions I believe

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



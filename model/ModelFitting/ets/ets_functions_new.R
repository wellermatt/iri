

# need generic functions to handle time series of varying lengths now...
# this procedure is very much lined up for monthly forecasting at present
# 

f_ets.roll.fc.item = function(y, h.max, PRINT = FALSE, ets.reopt = FALSE) 
{    
    # rules around length of series (n), horizon (h), splitting rules
    m = tsp(y)[3]               # m is seasonal period
    n <- length(y)              # n is series length
    
    if (m == 12) k <- 48             # minimum data length for fitting a model
    if (m == 52) k <- 208             # minimum data length for fitting a model
    
    # fit model to the test set
    st <- tsp(y)[1]+(k-1)/m                 # st = start period (as of) in ts terms
    yhist <- window(y, end = st)
    fit.original <- ets(yhist, model="ZZZ")  # model="MMM",
    
    # define the matrices for collecting the data
    err     <- matrix(NA, n-k, h.max) 
    yhat    <- matrix(NA, n-k, h.max)
    act     <- matrix(NA, n-k, h.max)    
    
    for(i in 1:(n-k))
    {
        yhist <- window(y, end = st + (i-1)/m)
        yfuture <- window(y, start = st + i/m , end = st + (i+h.max-1)/m)
        
        # use ets to re-fit the model.
        if (ets.reopt != TRUE) {
            fit <- ets(yhist, model=fit.original)
        } else {
            fit <- ets(yhist, model="ZZZ")
        }
        
        # do the forecasts,      
        fcast <- forecast(fit, h = min(n-k-i+1, h.max))
        
        # optional printing
        if (PRINT == TRUE) {
            print(fit)
            #print(yhist) 
            #print(yfuture)
            #print(fcast)
        }
        
        # record the data
        err[i,1:length(yfuture)] <- fcast[['mean']]-yfuture
        act[i,1:length(yfuture)] <- yfuture
        yhat[i,1:length(yfuture)] <- fcast[['mean']]
    }
    
    start.origin = k
    
    # prepare the accuracy stats
    act.melt = data.table(melt(act,value.name="y", varnames=c("t","k")),key="t,k") [!is.na(y)]
    yhat.melt = data.table(melt(yhat,value.name="yhat", varnames=c("t","k")),key="t,k") [!is.na(yhat)]
    Err = act.melt[yhat.melt]
    Err[,origin:=start.origin+t-1]
    Err[,fc.period:=start.origin+t+k-1]
    Err[,`:=` (e = yhat - y, ae = abs(yhat-y), 
               re = (yhat - y)/y, rae = abs((yhat - y)/y))]
    
    list(Err = Err, fit = fit)
}


f_ets.run.item = function(ss, freq, h.max, Trace = FALSE)  #fc.item = "00-01-18200-53030", pth = NULL)
{    
    y = ts(ss$UNITS, start=c(2001,1), frequency = freq)
    roll = f_ets.roll.fc.item(y, h.max = h.max)
    
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
    Err = this.roll$Err
    Err$fc.item = this.item
    Err
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



f_ets.test.multicore = function(sp, par.category, freq = 12, h.max=3, 
                                Trace = TRUE, opt.dopar = TRUE, i=10)
{
    library(doParallel)
    if (opt.dopar =="dopar") registerDoParallel(8)
    export.list = c("f_ets.run.item","f_ets.roll.fc.item")
    sp[,fc.item := factor(fc.item)]
    setkeyv(sp, c("fc.item"))  #,"period_id"))
    multi.item.results =
        foreach(dt.sub = isplitDT(sp, levels(sp$fc.item)),
                .combine='dtcomb', .multicombine=TRUE,
                .export = export.list,
                .packages=c("data.table", "forecast", "reshape2")) %dopar% {
                    fc.item = dt.sub$key[1]
                    print(fc.item)
                    #ss = spm[fc.item == items[i]]
                    this.roll = f_ets.run.item(dt.sub$value, freq = 12, h.max = 3,Trace=TRUE)
                    Err = this.roll$Err
                    Err$fc.item = fc.item
                    Err
                }
    setwd(pth.dropbox.data)
    #print(multi.item.results)
	fil=paste0("./output/errors/ets_445_fast_all_1_", par.category, ".rds")
    saveRDS(object=multi.item.results, file = fil)
    multi.item.results
}

library(reshape2)

## FUNCTIONS FOR RERIEVING & FORMATTING RESULTS

f_results.load = function(fil=NULL)
{
    # reads a single results output file and adds all the relevant point forecast errors
    library("stringr")
    res = readRDS(fil)
    res = f_errors.calculate(res)
    res[,`:=`(fc.item = factor(fc.item),
              lvl = factor(str_count( fc.item, "/") + 1)) ]
    
    res[,Level:=factor(lvl, levels = c(1,2,3),labels = c("ITEM", "CHAIN", "STORE"),ordered=TRUE)]    
    setkeyv(res,cols=c("lvl","fc.item","o","h"))
}

f_errors.calculate = function(dt)
{
    # receives a data.table with forecast and actuals and adds the errors in various guises, including:
    dt[,e:=fc-act]                                          # e = error
    dt[,ae:=abs(e)]                                         # ae = absolute error
    dt[,`:=`(ape = ae/act,                                  # ape = absolute percentage error
             sape = (2*ae)/(act+fc),                        # sape = smoothed absolute percentage error          
             rae.snaive = abs(fc-act)/abs(fc.snaive-act),   # rae.snaive = relative absolute error compared to snaive
             rae.naive = abs(fc-act)/abs(fc.naive-act),     # rae.naive = relative absolute error compared to naive
             ase.naive = ae/mae.naive,                      # ase.naive = absolute error scaled to mae of naive
             ase.snaive = ae/mae.snaive)]                   # ase.snaive = absolute error scaled to mae of snaive
}


f_results.summarise = function(res) {
    
    # mdAPE, MAPE, sMAPE, MASE
    # dcast(data=res, lvl+fc.item+h~.,fun.aggregate=median,na.rm=TRUE,value.var="ase.snaive")
    
    res.summary = res[, j = list(mape = mean(ape, na.rm=TRUE),
                                 mdape = median(ape, na.rm=T),
                                 smape = mean(sape, na.rm=TRUE),
                                 mdrae.n = median(rae.naive),
                                 mdrae.sn = median(rae.snaive),
                                 mase.n = mean(ase.naive, na.rm=TRUE),
                                 mase.sn = mean(ase.snaive, na.rm=TRUE)),
                      by=list(Level,fc.item, lvl, method, freq, freq.cycle)]

}

f_consolidate.errors = function(upc = "00-01-18200-53030", opt.save = FALSE)
{
    # will read in multiple results files (different methods/periodicities) and consolidate them into a single results table
    # currently hard-coded to read specific named files for either a single item or multiple items from a specified location
    
    # get the files to read
    if (!is.null(upc))
    {
        res.files = c("E:/data/errors/ets_12_12_3_beer_00-01-18200-53030.rds",
                      "E:/data/errors/ets_52_12_3_beer_00-01-18200-53030.rds",
                      "E:/data/errors/ets_52_52_3_beer_00-01-18200-53030.rds",
                      "E:/data/errors/reg_12_12_3_beer_00-01-18200-53030.rds",
                      "E:/data/errors/reg_52_12_3_beer_00-01-18200-53030.rds",
                      "E:/data/errors/reg_52_52_3_beer_00-01-18200-53030.rds")
    } else {   
        res.files = c("E:/data/errors/ets_12_12_3_beer_.rds",
                      "E:/data/errors/ets_52_12_3_beer_.rds",
                      "E:/data/errors/ets_52_52_3_beer_.rds",
                      "E:/data/errors/reg_12_12_3_beer_.rds",
                      "E:/data/errors/reg_52_12_3_beer_.rds",
                      "E:/data/errors/reg_52_52_3_beer_.rds")
    }
    
    # this can be parallelised ****
    # read each file and tidy it, adding the factors to describe
    results = rbindlist(lapply(res.files,function(x) {
        vars = strsplit(strsplit(x,"/")[[1]][4],"_")[[1]][1:3]
        data.table(method = vars[1], freq = vars[2], freq.cycle = vars[3],f_results.load(x))})) # filter the results??? -->> [!(freq.cycle!=freq)]
    results[,`:=`(freq = factor(freq, levels = c(12,52),labels=c("MONTH", "WEEK")),
                  freq.cycle = factor(freq.cycle,levels = c(12,52),labels=c("MONTH", "WEEK")))]
    
    if (opt.save == TRUE) saveRDS(results, "E:/data/errors/all.rds")
    return(results)
}

#stop()
#results = f_consolidate.errors(upc=NULL)
# now need to make sure we have only the records that are in both sets of errors (i.e.exclude NAs)


## load the consolidated results
#results = readRDS("E:/data/errors/all.rds")


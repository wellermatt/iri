library(reshape2)
## FUNCTIONS FOR RERIEVING & FORMATTING RESULTS


f_results.load = function(fil=NULL)
{
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
    # receives a data.table and adds the errors in various guises
    dt[,e:=fc-act]
    dt[,ae:=abs(e)]
    dt[,`:=`(ape = ae/act,
             sape = (2*ae)/(act+fc),            
             rae.snaive = abs(fc-act)/abs(fc.snaive-act),
             rae.naive = abs(fc-act)/abs(fc.naive-act),
             ase.naive = ae/mae.naive,
             ase.snaive = ae/mae.snaive)]
}


f_results.summarise = function(res) {
    
    # mdAPE, MAPE, sMAPE, MASE
    #dcast(data=res, lvl+fc.item+h~.,fun.aggregate=median,na.rm=TRUE,value.var="ase.snaive")
    
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


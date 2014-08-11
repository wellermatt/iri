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

f_errors.rank = function(dt, value.var = "ae", frm = NULL, par.melt = FALSE, par.recast = FALSE)
{
    # receives a data.table with forecast and actuals and aall the error measures per period/sku combination
    # purpose to rank the METHODS on a single error metric and summarise the analysis
    
    ## which field to use as the value.var? ae? sape?
    
    #if (is.null(frm)) frm = as.formula("freq+Level+lvl+fc.item+o+h~method")
    err = data.table(dcast(dt, freq+Level+lvl+fc.item+o+h~method,fun.aggregate=sum,na.rm=TRUE,value.var=value.var,fill=NA_real_))
    
    ### IMPROVE WITH DATA TABLE
    err$reg = abs((err$reg<err$ets)-2)
    err$ets = 3 - err$reg
    #err$ets_rank = abs((err$ets<err$reg)-2)
    #err$reg_rank = abs((err$reg<err$ets)-2)
    #err$reg_diff_val = (err$reg-err$ets)
    #err$reg_diff_perc = err$reg_diff_val/err$ets    
    if (par.melt == TRUE) err = melt(err, id.vars = c("freq","Level","lvl","fc.item","o","h"),variable.name="method" )
    if (par.recast == TRUE) err = dcast(err,freq+Level+lvl+fc.item+method~.,fun.aggregate=mean,na.rm=TRUE)
    names(err)[length(err)] = "value"
    data.table(err)
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

f_consolidate.errors = function(upc = "00-01-18200-53030", par.category, opt.save = FALSE)
{
    # will read in multiple results files (different methods/periodicities) and consolidate them into a single results table
    # currently hard-coded to read specific named files for either a single item or multiple items from a specified location
    
    print(upc)
    # get the files to read
    if (is.null(upc)) upc = ""
    res.files = c(paste0(pth.data.local, "output/errors/ets_12_12_3_", par.category, "_", upc, ".rds"), #                      "E:/data/errors/ets_52_12_3_beer_.rds",
                  paste0(pth.data.local, "output/errors/ets_52_52_3_", par.category, "_", upc, ".rds"),
                  paste0(pth.data.local, "output/errors/reg_12_12_3_", par.category, "_", upc, ".rds"), #                      "E:/data/errors/reg_52_12_3_beer_.rds",
                  paste0(pth.data.local, "output/errors/reg_52_52_3_", par.category, "_", upc, ".rds"))
    print(res.files)
    # this can be parallelised ****
    # read each file and tidy it, adding the factors to describe
    results = rbindlist(lapply(res.files,function(x) {
        vars = strsplit(strsplit(x,"/")[[1]][5],"_")[[1]][1:3]
        data.table(method = vars[1], freq = vars[2], freq.cycle = vars[3],f_results.load(x))})) # filter the results??? -->> [!(freq.cycle!=freq)]
    results[,`:=`(freq = factor(freq, levels = c(12,52),labels=c("MONTH", "WEEK")),
                  freq.cycle = factor(freq.cycle,levels = c(12,52),labels=c("MONTH", "WEEK")))]
    
    if (opt.save == TRUE) {
        fil = paste0(pth.data.local , "output/errors/", par.category, "_", upc, ".rds")
        saveRDS(results, fil)
    }
    
    return(results)
}

#stop()
#results = f_consolidate.errors(upc=NULL)
# now need to make sure we have only the records that are in both sets of errors (i.e.exclude NAs)


## load the consolidated results
#results = readRDS("E:/data/errors/all.rds")


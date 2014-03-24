
#======= ROLL CONTROLLER ===========

f_reg.roll.multiCORE = function(sp, par.category = "beer", par.periodicity = "weekly", 
                                opt.print.results = FALSE, opt.save.results =TRUE, h.max, cores = 1)
{
    # multicore implementation of the rolling regression
    # sp can contain multiple products
    sp[,fc.item := factor(fc.item)]
    setkeyv(sp, c("fc.item"))  #,"period_id"))
    
    
    library(doParallel)
    #if (opt.dopar =="dopar") registerDoParallel(cores)
    library(doSNOW)
    cl <- makeCluster(cores, outfile="")
    registerDoSNOW(cl)
    
    export.functions =  c("f_reg.roll_fc.item","f_ts.regression.auto.stepAIC",
                          "f_ts.regression.fourier.k.optimise","f_ts.regression.fourier.k.test","f_ts.fourier.terms.for.formula","f_ts.regression.data.reduce.formula",
                          "f_ts.regression.elast", "f_ts.regression.auto.formulae.scope", "f_ts.regression.model.summary", "f_ts.diag.coef.table", "f_ts.eval.accuracy.lm",
                          "log.model", "include.AR.terms", "price.terms",
                          "dtcomb", "isplitDT")
    
    multi.item.results =
        foreach(dt.sub = isplitDT(sp, levels(sp$fc.item)),
                .combine='dtcomb', .multicombine=TRUE,
                .errorhandling = "stop",
                .export = export.functions,
                .packages=c("data.table", "forecast", "reshape2","MASS","foreach")) %dopar%
        {
            fc.item = dt.sub$key[1]
            print(paste0("\n--> ",fc.item))
            reg.roll = f_reg.roll_fc.item(sp1 = dt.sub$value, freq = 52, h.max=h.max)             
            results = data.table(fc.item = fc.item, periodicity = par.periodicity, reg.roll)    
            results
        }
    print(head(multi.item.results,10))
    
    opt.add.errors = FALSE
    if (opt.add.errors == TRUE) {
        multi.item.results = f_errors.calculate(multi.item.results)
    }
        
        
    setwd(pth.dropbox.data)
    if (opt.print.results == TRUE) print(multi.item.results)
    if (opt.save.results == TRUE) {
        setwd(pth.dropbox.data)
        saveRDS(multi.item.results, paste0("./output/errors/reg_", par.periodicity, "_", par.category, ".rds"))
    }
    return(multi.item.results)
}




#======= ROLL FUNCTIONS ==============

f_reg.roll_fc.item = function(sp1, freq = 52, h.max = 13, model.pars = NULL)
{
    # receives the data (ssw) to fit the model for a single item using additional parameters
    #require(fomulatools)

    # 1 needs correction to accept weekly or monthly data with varying start/end dates
    # 2 needs to handle xreg NAs and output a restricted set
    
    
    if (freq == 12) o1 <- 48             # minimum data length for fitting a model
    if (freq == 52) o1 <- 208             # minimum data length for fitting a model
    
    end.period = max(sp1$period)
    
    # initial fit window and fit STEPWISE model
    fit.model.original = f_ts.regression.auto.stepAIC(sp1[1:o1],print.details=0)  # window.t = 1:198
    frm.original = fit.model.original$call$formula
    frm.text = paste(as.character(frm.original)[c(2,1,3)],collapse=" ")
    model.vars = gsub("^\\s+|\\s+$", "", unlist(strsplit(frm.text, split = "\\+|\\~")))
    
    
    reg.roll = 
        foreach (o = o1:(end.period-1),     
                 .combine='dtcomb', .multicombine=TRUE) %do%
        {                  
            # what is the extent of the horizon for this origin
            h = min(h.max, end.period-o)
            #print(h)
            # build the xreg variables, based on the variables in formula!!        
            xregnew = sp1[period %in% (o+1):(o+h), eval(model.vars), with = F]
            missing.periods = is.null(xregnew$UNITS)
            
            # update or re-optimise the model??
            # fit.model = f_ts.regression.auto.stepAIC(ssw[1:o])  # window.t = 1:198
            revised.model = lm(formula = frm.text, data=sp1[1:o])
    
            # make the predictions
            fc = predict.lm(object=revised.model,newdata=xregnew)
            fc
            #             fc = forecast(revised.model,
            #                           h=min(h, end.week-o), 
            #                           newdata = data.frame(xregnew))
            act = sp1[period %in% (o+1):(o+h),UNITS]
            fc.comparison = data.table(o,
                                       h = (1:h)[!missing.periods],
                                       t = o+1:h[!missing.periods],
                                       fc = as.numeric(fc),
                                       fc.naive = rep(sp1[o,UNITS], h)[!missing.periods],   #sp1[1:o1]
                                       fc.snaive = sp1[(o-freq+1):(o-freq+h),UNITS][!missing.periods],    #sp1[1:o1]
                                       act = as.numeric(act)[!missing.periods])
            fc.comparison
        }

    return(reg.roll)
}






#     results = data.table(fc.item = this.item, periodicity = "weekly", reg.roll$roll.stats)
#     results
#     results.all = rbindlist(results.all)
#     results.all[,`:=`(e = fc-act, ae=abs(fc-act), 
#                       re = (fc-act)/act, 
#                       rae = abs(fc-act)/act,
#                       srae = abs(fc-act)/(0.5 * (act + fc)))]   
#     



#reg.roll = f_reg.roll.item(ssw=ssw, o1=207,h=13)    

# mm = reg.roll$final.model  # do we need the final model or not?
# coefficients/elasticities



f_reg.roll.m= function(sp, imax = 1)
{
    # Function to take a full regression dataset for multiple items and run the regression roll procedure for each item
    # a multi-core version is also available
    
    # option to test lapply here instead of foreach for loopong
    
    
    items = unique(sp$fc.item)
    
    results.all = foreach (i = 1:imax) %do%  #length(items)
    {
        this.item = as.character(items[i]) # as.character(spw$fc.item[[i]])
        sp1 = sp[fc.item == this.item]  #,-1, with=F
        
        print(this.item)
        reg.roll = f_reg.roll_fc.item(sp1=sp1, o1=207,h=13)    
        
        # mm = reg.roll$final.model  # do we need the final model or not?
        # coefficients/elasticities
        # currently ONLY returning the stats: fc and actuals, NOT the model details
        
        results = data.table(fc.item = this.item, periodicity = "weekly", reg.roll)    
        
        results
        
    }
    
    results.all = rbindlist(results.all)
    results.all[,`:=`(e = fc-act, ae=abs(fc-act), 
                    re = (fc-act)/act, 
                    rae = abs(fc-act)/act,
                    srae = abs(fc-act)/(0.5 * (act + fc)))]   

}


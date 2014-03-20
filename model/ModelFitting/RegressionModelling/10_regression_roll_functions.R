
#======= ROLL CONTROLLER ===========

f_reg.roll.multiCORE = function(sp, opt.print.results = FALSE, opt.save.results =TRUE, imax = 1, periodicity = "weekly", ncores = 0)
{
    # multicore implementation of the rolling regression
    # sp can contain multiple products
    library(doParallel)
    registerDoParallel(cores=ncores)
    sp[,fc.item := factor(fc.item)]
    setkeyv(sp, c("fc.item"))  #,"period_id"))
    export.functions =  c("f_reg.roll_fc.item","f_ts.regression.auto.stepAIC",
                          "f_ts.regression.fourier.k.optimise","f_ts.regression.fourier.k.test","f_ts.fourier.terms.for.formula","f_ts.regression.data.reduce.formula",
                          "f_ts.regression.elast", "f_ts.regression.auto.formulae.scope", "f_ts.regression.model.summary", "f_ts.diag.coef.table", "f_ts.eval.accuracy.lm",
                          "log.model", "include.AR.terms", "price.terms",
                          "dtcomb", "isplitDT")
    
    multi.item.results =
        foreach(dt.sub = isplitDT(sp, levels(sp$fc.item)),
                .combine='dtcomb', .multicombine=TRUE,
                .export = export.functions,
                .packages=c("data.table", "forecast", "reshape2","MASS","foreach")) %dopar%
        {
            fc.item = dt.sub$key[1]
            print(fc.item)
            reg.roll = f_reg.roll_fc.item(sp1 = dt.sub$value, o1=208, h=13)       #f_ets.run.item(dt.sub$value, frequency = 12, h.max = 3)
            
            results = data.table(fc.item = fc.item, periodicity = periodicity, reg.roll)    
            
            results
        }
    setwd(pth.dropbox.data)
    if (opt.print.results == TRUE) print(multi.item.results)
    if (opt.save.results == TRUE) saveRDS(object=multi.item.results,file="./output/errors/reg_week_test.rds")
    return(multi.item.results)
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



#======= ROLL FUNCTIONS ==============

f_reg.roll_fc.item = function(sp1, o1 = 208, h = 13, model.pars = NULL)
{
    # receives the data (ssw) to fit the model for a single item using additional parameters
    #require(fomulatools)
    # needs correction to accept weekly or monthly data with varying start/end dates
    
    end.week = max(sp1$WEEK)
  
    # initial fit window and fit STEPWISE model
    fit.model.original = f_ts.regression.auto.stepAIC(sp1[1:o1])  # window.t = 1:198
    frm.original = fit.model.original$call$formula
    frm.text = paste(as.character(frm.original)[c(2,1,3)],collapse=" ")
    model.vars = gsub("^\\s+|\\s+$", "", unlist(strsplit(frm.text, split = "\\+|\\~")))
    
    
    reg.roll = 
      foreach (o = o1:(end.week-1),     
               .combine='dtcomb', .multicombine=TRUE) %do%         
      {                  
          # build the xreg variables, based on the variables in formula!!        
          xregnew = sp1[WEEK %in% (o+1):min(o+h,end.week), eval(model.vars), with = F]
          
          # update or re-optimise the model??
          # fit.model = f_ts.regression.auto.stepAIC(ssw[1:o])  # window.t = 1:198
          revised.model = lm(formula = frm.text, data=sp1[1:o])
          
          # make the predictions
          fc = predict.lm(object=revised.model,newdata=xregnew)
          
          #             fc = forecast(revised.model,
          #                           h=min(h, end.week-o), 
          #                           newdata = data.frame(xregnew))
          
          act = sp1[WEEK %in% (o+1):min(o+h,end.week),UNITS]
          
          fc.comparison = data.table(t = o, 
                                     k = 1:min(h, end.week-o),
                                     fc = fc,
                                     act = act)
          fc.comparison
      }
    
    reg.roll
}

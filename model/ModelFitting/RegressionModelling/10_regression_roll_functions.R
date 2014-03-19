
#======= ROLL CONTROLLER ===========

f_reg.roll.multiCORE = function(imax = 1)
{
    # multicore implementation of the rolling regression
    
    if (test.multicore == TRUE) {
        library(doParallel)
        registerDoParallel(8)
        spw[,fc.item := factor(fc.item)]
        setkeyv(spw, c("fc.item"))  #,"period_id"))
        multi.item.results =
            foreach(dt.sub = isplitDT(spw, levels(spw$fc.item)),
                    .combine='dtcomb', .multicombine=TRUE,
                    .packages=c("data.table", "forecast", "reshape2","MASS")) %do%
            {
                fc.item = dt.sub$key[1]
                print(fc.item)
                #ss = spm[fc.item == items[i]]
                this.roll = f_reg.roll.item(ssw=dt.sub$value, o1=207,h=13)       #f_ets.run.item(dt.sub$value, frequency = 12, h.max = 3)
                
                # mm = reg.roll$final.model  # do we need the final model or not?
                # coefficients/elasticities
                
                Err = this.roll$Err
                Err$fc.item = fc.item
                Err
            }
        setwd(pth.dropbox.data)
        print(multi.item.results)
        saveRDS(object=rbindlist(multi.item.results),file="./output/errors/ets_445_fast.rds")
    }

    
    
    
    
    reg.roll = f_reg.roll.item(ssw=ssw, o1=207,h=13)    
    
    # mm = reg.roll$final.model  # do we need the final model or not?
    # coefficients/elasticities
    
results = data.table(fc.item = this.item, periodicity = "weekly", reg.roll$roll.stats)
results
results.all = rbindlist(results.all)
results.all[,`:=`(e = fc-act, ae=abs(fc-act), 
                  re = (fc-act)/act, 
                  rae = abs(fc-act)/act,
                  srae = abs(fc-act)/(0.5 * (act + fc)))]   

}

f_reg.roll.multi = function(sp, imax = 1)
{
  # this is probably the function that requires the multicore processing
  
  results.all = 
    foreach (i = 1:imax) %do%  #length(items)
  {
    this.item = as.character(items[i]) # as.character(spw$fc.item[[i]])
    ss = sp[fc.item == this.item]  #,-1, with=F
    
    print(this.item)
    reg.roll = f_reg.roll.item(ssw=ss, o1=207,h=13)    
    
    # mm = reg.roll$final.model  # do we need the final model or not?
    # coefficients/elasticities
    
    results = data.table(fc.item = this.item, periodicity = "weekly", reg.roll$roll.stats)
    results
  }
  
  results.all = rbindlist(results.all)
  results.all[,`:=`(e = fc-act, ae=abs(fc-act), 
                    re = (fc-act)/act, 
                    rae = abs(fc-act)/act,
                    srae = abs(fc-act)/(0.5 * (act + fc)))]   
  
}



#======= ROLL FUNCTIONS ==============

f_reg.roll.item = function(ssw, o1 = 207, h = 13, model.pars = NULL)
{
    # receives the data (ssw) to fit the model for a single item using additional parameters
    #require(fomulatools)
    
    end.week = max(ssw$WEEK)
  
    # initial fit window and fit STEPWISE model
    fit.model.original = f_ts.regression.auto.stepAIC(ssw[1:o1])  # window.t = 1:198
    frm.original = fit.model.original$call$formula
    frm.text = paste(as.character(frm.original)[c(2,1,3)],collapse=" ")
    model.vars = gsub("^\\s+|\\s+$", "", unlist(strsplit(frm.text, split = "\\+|\\~")))
    
    
    #print(accuracy(fit.model.original))
    #hist(resid(fit.model.original))
    
    
    
    reg.roll = 
      foreach (o = o1:(end.week-1),     
               .combine='dtcomb', .multicombine=TRUE) %do%         
      {                  
          # build the xreg variables, based on the variables in formula!!        
          xregnew = ssw[WEEK %in% (o+1):min(o+h,end.week), 
                        eval(model.vars), with = F]
          
          # update or re-optimise the model??
          # fit.model = f_ts.regression.auto.stepAIC(ssw[1:o])  # window.t = 1:198
          revised.model = lm(formula = frm.text, data=ssw[1:o])
          
          # make the predictions
          fc = predict.lm(object=revised.model,newdata=xregnew)
          
          #             fc = forecast(revised.model,
          #                           h=min(h, end.week-o), 
          #                           newdata = data.frame(xregnew))
          
          act = ssw[WEEK %in% (o+1):min(o+h,end.week),UNITS]
          
          fc.comparison = data.table(t = o, 
                                     k = 1:min(h, end.week-o),
                                     fc = fc,
                                     act = act)
          fc.comparison
      }
    list(roll.stats = reg.roll, final.model = revised.model)
    
}





#======= ROLL FUNCTIONS ==============

f_reg.roll = function(ssw, o1 = 207, h = 13)
{
    # receives the data to fit the model to and additional paramters
    #require(fomulatools)
    
    end.week = max(ssw$WEEK)  
  
    # initial fit window and fit STEPWISE model
    fit.model.original = f_ts.regression.auto.stepAIC(ssw[1:o1])  # window.t = 1:198
    frm.original = fit.model.original$call$formula
    frm.text = paste(as.character(frm.original)[c(2,1,3)],collapse=" ")
    model.vars = gsub("^\\s+|\\s+$", "", unlist(strsplit(frm.text, split = "\\+|\\~")))
    
    
    print(accuracy(fit.model.original))
    hist(resid(fit.model.original))
    
    
    
    reg.roll = 
    foreach (o = o1:(end.week-1),   #(o1+1)   #(fit.week.end)  #(end.week-1)
             .combine='dtcomb', .multicombine=TRUE) %do%         
    {          
        #print(o)
        
        # build the xreg variables, based on the variables in formula!!        
        xregnew = ssw[WEEK %in% (o+1):min(o+h,end.week), 
                      eval(model.vars), with = F]
        
        #fit.model = f_ts.regression.auto.stepAIC(ssw[1:o])  # window.t = 1:198
        revised.model = lm(formula = frm.text, data=ssw[1:o])
        
        fc = predict.lm(object=revised.model,newdata=xregnew)
        #             fc = forecast(revised.model,
        #                           h=min(h, end.week-o), 
        #                           newdata = data.frame(xregnew))
        
        act = ssw[WEEK %in% (o+1):min(o+h,end.week),UNITS]
        
        fc.comparison = data.table(t = o, 
                                   k = 1:min(h, end.week-o),
                                   fc = fc, act)
        fc.comparison[,`:=`(e=fc-act, re = (fc-act)/act)]   
        #print(fc.comparison)
        fc.comparison
    }
    list(roll.stats = reg.roll, final.model = revised.model)
    
}

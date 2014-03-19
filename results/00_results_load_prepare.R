# Contains functions to prepare the results data (forecast accuracy from various methods at various levels) for presentation


library("ggplot2");library("reshape2");library("stringr")




# generate some results and comparisons of the different forecasting methods at various levels of aggregations:

# Dimensions:
# item - chain- store
# months - weeks

# Accuracy measures/residuals diagnostics
# detailed: e, ae, re, rae, srae, se, ase (abs scaled error)
# summary: mean, median, trimmed mean, geometric mean, etc.

# Experimental Factors:
# time series frequency
# forecast frequency
# evaluation frequency
# aggregation/disaggregation logic
# forecasting method + variables
# forecast item - category, brand, etc.



# need to start examining the errors
# get some errors ** requires consistency here in input spec

#source("./.Rprofile")
lk.week.to.month = function (o.week) min(which(calendar.445$elapsed_weeks >= o.week))
lk.month.to.week = function (o.month, start.end = "end") calendar.445$elapsed_weeks[o.month]

f_read.fcast.values = function(periodicity = "monthly", h.eval=3, 
                               melt.data = TRUE, this.item =  "00-01-18200-53030")
{
  setwd(pth.dropbox.data)
  
  # read the fcast data from file for both methods
  if (periodicity == "weekly") {
    fc1 = readRDS( "./output/errors/bk/ets_445_weekly.rds")[,list(method = "ets_445", periodicity="weekly", fc.item, o = origin.week, k, fc, act)]
    origin.subset.445 = unique(fc1$o)
    fc2 = readRDS( "./output/errors/bk/errors_reg2.rds")[t %in% origin.subset.445,
                                                      list(method = "regression_weekly", periodicity="weekly", fc.item, o = t, k, fc, act)]    
    fcast = rbindlist(list(fc1, fc2))[,fc.period:=o+k]
    #fcast[,o.month := lk.week.to.month(o)]
    fcast$o.month = unlist(lapply(fcast$o,function(o)lk.week.to.month(o)))
    fcast$fc.month = unlist(lapply(fcast$fc.period,function(o)lk.week.to.month(o)))
    fcast[,m:=fc.month-o.month]   
  } else {
    fc1 = readRDS( "./output/errors/bk/ets_445.rds")[,list(method = "ets_445", periodicity="monthly", fc.item, o = origin, k, fc=yhat)]
    origin.subset.445 = unique(fc1$o)
    fc2 = readRDS("./output/errors/bk/regression_weekly_445.rds")[origin %in% origin.subset.445,
                                                                list(method = "regression_weekly", periodicity="monthly", fc.item, o = origin, k, fc)]
    fcast = rbindlist(list(fc1, fc2))[,fc.period:=o+k] 
    fcast[,o.month := o]
    fcast[,fc.month := o+k]
    fcast[,m:=fc.month-o.month]
  }
  
  
  if (melt.data == TRUE) {
    
    # filter to the relevant horizon (months and weeks?) and item (optional?)
    fcast = fcast[m == h.eval &  fc.item == this.item] 
    
    fcast.melt = data.table(melt(fcast, id.vars=c("fc.item", "method", "periodicity", "fc.period"),measure.vars="fc"))[,variable:=NULL]
    setnames(fcast.melt,"fc.period","t")
    key.fields = fcast.melt[1,list(fc.item,method="Actuals",periodicity)] 
    actuals = f_get_actuals(periodicity=periodicity)
    n.actuals = length(actuals)
    act = data.table(key.fields,t= 1:n.actuals, value = actuals)
    out = rbindlist(list(fcast.melt, act ) )
    out
  } else  fcast
  
}
#f_get_actuals(periodicity="monthly")


f_load.res.ets.all = function(par.category)
{
    require(stringr);require(ggplot2)
    setwd(pth.dropbox.data)
    fil = paste0("./output/errors/ets_445_fast_all_123_", par.category, ".rds")
    res =readRDS(fil)[,list(category = par.category, method = "ets_445", periodicity="monthly", fc.item, o = origin, k, fc=yhat, act=y, ape = 100*rae)]
    res[,lvl := str_count( fc.item, "/") + 1 ]
    res[,Level:=factor(lvl,labels = c("1 ITEM", "2 CHAIN", "3 STORE")[1:length(unique(lvl))],ordered=TRUE)]
    res[,lvl:=NULL]
    
    res[,km:= cut(k, breaks=c(0,1,2,3),right=TRUE,labels=c("M1","M2","M3"))]
    
    
    #print(ggplot(data=res, aes(x= km, y = ape, colour = Level, alpha = 0.5)) +  geom_jitter() + facet_wrap( ~Level, ncol=2, scales="free") + geom_boxplot()) #+ coord_flip())
    #print(ggplot(data=res[Level=="1 ITEM"], aes(x= fc.item, y = ape, colour = Level, alpha = 0.5)) +  geom_jitter() + facet_wrap( ~km, ncol=1) + geom_boxplot() + coord_flip())
    #print(ggplot(data=res[Level=="2 CHAIN"], aes(y = ape, x = Level, alpha = 0.5)) +  geom_jitter() + facet_wrap( ~km, ncol=1) + geom_boxplot() + coord_flip())
    res
}
categories = c("beer", "carbbev","milk")
res = rbindlist(lapply(categories,f_load.res.ets.all))
dcast(res,formula=category~Level,fun.aggregate=median,value.var="ape")
dcast(res[Level=="1 ITEM"],formula=category+fc.item~km,fun.aggregate=median,value.var="ape")

f_load.res.ets.all(par.category = "carbbev")

f_fcast.plot = function(plot.data, periodicity = "weekly", h.eval = 3)
{
    # plot.data is a melt of the time series, forecast and various forecasts
    plot.data = f_read.fcast.values(periodicity = periodicity, h.eval = h.eval)
    
    #plot.data = f_read.fcast.values(periodicity = periodicity, 
    #                                  h.eval=h.eval, melt.data=TRUE)  
  
  # get dates from plot.data
  tt = unique(plot.data$t)
  # add dates into plot.data
  
  if (periodicity == "weekly") t.start = 200 else t.start = 40
  
  plot.title = paste(h.eval, " month ahead Forecast comparison", periodicity, plot.data$fc.item[1], "\n")
  #tasks:
    # add dates to x axis, zero y-axis, titles, legend
    # colours: actuals are BLACK, benchmark is green, our model is RED
  
  p = ggplot(data=plot.data[t>=t.start], aes(x=t, y=value, colour = method)) + 
    geom_line(size=1) + geom_point(size=3) + 
    theme_bw() + ggtitle(plot.title) + ylab("Units Sold\n") + xlab("\nTime")
  p
}
#f_fcast.plot()

f_get.fcast.comp.dat.weekly = function()
{
  # this will load the errors from the ets 445 weekly split and regression weekly forecasts
  setwd(pth.dropbox.data)
  
  rae1 = readRDS( "./output/errors/bk/ets_445_weekly.rds")[,list(method = "ets_445", periodicity="weekly", fc.item, o = origin.week, k, rae)]
  origin.subset.445 = unique(rae1$o)
  rae2 = readRDS( "./output/errors/bk/errors_reg2.rds")[t %in% origin.subset.445,
                                                    list(method = "regression_weekly", periodicity="weekly", fc.item, o = t, k, rae)]
  
  fcast.comp.dat = rbindlist(list(rae1, rae2))
  fcast.comp.dat[,lvl := str_count( fc.item, "/") + 1 ]
  fcast.comp.dat[,Level:=factor(lvl,labels = c("1 ITEM", "2 CHAIN", "3 STORE"))]
  fcast.comp.dat[,lvl:=NULL]
  fcast.comp.dat[,km:= cut(k, breaks=c(0,4,8,13),right=TRUE,labels=c("M1","M2","M3"))]
  fcast.comp.dat
}

f_get.fcast.comp.dat.monthly = function()
{
  # this will load the errors from file for ets 445 and aggregated weekly regression forecasts
  setwd(pth.dropbox.data)
  rae1 = readRDS( "./output/errors/bk/ets_445.rds")[,list(method = "ets_445", periodicity="monthly", fc.item, o = origin, k, rae)]
  origin.subset.445 = unique(rae1$o)
  rae2 = readRDS("./output/errors/bk/regression_weekly_445.rds")[origin %in% origin.subset.445,
                                                             list(method = "regression_weekly", periodicity="monthly", fc.item, o = origin, k, rae)]
  
  fcast.comp.dat = rbindlist(list(rae1,rae2))
  fcast.comp.dat[,lvl := str_count( fc.item, "/") + 1 ]
  fcast.comp.dat[,Level:=factor(lvl,labels = c("1 ITEM", "2 CHAIN", "3 STORE"),ordered=TRUE)]
  fcast.comp.dat[,lvl:=NULL]
  
  fcast.comp.dat[,km:= cut(k, breaks=c(0,1,2,3),right=TRUE,labels=c("M1","M2","M3"))]
  fcast.comp.dat
  
  
}




f_results.test1 = function(fcast.comp.dat)
{
  # some basic reports and levels of comparison for the resultts
  print(fcast.comp.dat[,median(rae, na.rm=TRUE),by=list(k,method)])
  print(dcast(fcast.comp.dat,formula=Level+method~k,median,na.rm=TRUE,value.var="rae"))
  print(dcast(fcast.comp.dat,formula=method~lvl,median,na.rm=TRUE,value.var="rae"))
  
  p = ggplot(fcast.comp.dat[k<14], aes(x=factor(k),y = rae, colour = method, shape = method, size=1)) +  stat_summary(fun.y = median,na.rm=TRUE,geom="point") + 
    theme_bw() +facet_wrap(~lvl,ncol=1)+scale_y_continuous(limits = c(0,0.5))+coord_flip()
  print(p)
  p = ggplot(fcast.comp.dat[k<14], aes(x=method, y = rae, colour = method)) + geom_boxplot() +
    theme_bw() +facet_wrap(~lvl,ncol=1)+scale_y_continuous(limits = c(0,2))+coord_flip()
  print(p)
  
}

f_fcast.compare = function()
{
  # now compare the error for each point forecast between the 2 methods and rank them/flag which is best
  point.comp = data.table(dcast(fcast.comp.dat,fc.item+o+k~method,fun.aggregate=sum,value.var="rae"))
  point.comp[,`:=`(improved = as.integer((ets_445>regression_weekly)), gain = (ets_445-regression_weekly))]
  
  head(point.comp)
  point.comp.summary = point.comp[,list(median_gain=median(gain,na.rm=TRUE), proportion_improved=mean(improved,na.rm=TRUE)),by=fc.item]
  head(point.comp.summary)             
  
}


# consistent data format required:
# 
#names(fc2)

# two types of comparison. like-for-like with rankings or summary measures
# like-for like requires ranking within group (i.e. same origin & horizon)

#err.comp = 



  
  
  
  
  
  
  
#   
# 
# 
# # functions to get summary stats
# acc.summary.week = data.table(cbind(dcast(data=fcast.weekly, formula=fc.item+k~., fun.aggregate=median,value.var="rae"),
#                                     dcast(data=fcast.weekly, formula=fc.item+k~., fun.aggregate=mean,value.var="rae")[-1:-2]))
# 
# names(acc.summary.week)[-1:-2] = c("mdrae","mrae")
# 
# 
# if (TRUE == FALSE) 
# {
#   library(ggplot2)
#   p = ggplot(data = fcast.weekly, aes(x=rae)) + geom_histogram() + ggtitle(this.fc.item)  ; print(p)
#   p = ggplot(data = fcast.weekly, aes(x = as.factor(k), y = rae)) + geom_boxplot() + ggtitle(this.fc.item) ; print (p)
#   
#   fcast.weekly[,list(mdrae = median(rae), mrae = mean(rae)),by="k"]
#   
#   with(fcast.weekly,boxplot(rae~k))
# }     
# 
# 
# 
# # pick fields of interest
# # add other fields e.g. chain/store/level/category/importance......
# acc.summary.week[,lvl := str_count( fc.item, "/") + 1 ]
# acc.summary.week
# 
# 
# 
# library(ggplot2)
# qplot(data=acc.summary.week[k!=14],x=factor(k),y=mdrae, geom = "boxplot") + coord_flip() + facet_wrap(~lvl,ncol=1)

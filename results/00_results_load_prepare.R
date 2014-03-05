library("ggplot2");library("reshape2");library(stringr)


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
f_get.fcast.comp.dat.weekly = function()
{
  # this will load the errors from the ets 445 weekly split and regression weekly forecasts
  setwd(pth.dropbox.data)
  
  fc1 = readRDS( "./output/errors/ets_445_weekly.rds")[,list(method = "ets_445", periodicity="weekly", fc.item, o = origin.week, k, rae)]
  origin.subset.445 = unique(fc1$o)
  fc2 = readRDS( "./output/errors/errors_reg2.rds")[t %in% origin.subset.445,
                                                    list(method = "regression_weekly", periodicity="weekly", fc.item, o = t, k, rae)]
  
  fcast.comp.dat = rbindlist(list(fc1,fc2))
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
  fc1 = readRDS( "./output/errors/ets_445.rds")[,list(method = "ets_445", periodicity="monthly", fc.item, o = origin, k, rae)]
  origin.subset.445 = unique(fc1$o)
  fc2 = readRDS("./output/errors/regression_weekly_445.rds")[origin %in% origin.subset.445,
                                                             list(method = "regression_weekly", periodicity="monthly", fc.item, o = origin, k, rae)]
  
  fcast.comp.dat = rbindlist(list(fc1,fc2))
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

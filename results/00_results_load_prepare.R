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
setwd(pth.dropbox.data)
fc1 = readRDS( "./output/errors/ets_445_weekly.rds")[,list(method = "ets 445 split", periodicity="weekly", fc.item, o = origin.week, k, rae)]
origin.subset.445 = unique(fc1$o)
fc2 = readRDS( "./output/errors/errors_reg2.rds")[t %in% origin.subset.445,list(method = "regression weekly", periodicity="weekly", fc.item, o = t, k, rae)]

err.comp.dat = rbindlist(list(fc1,fc2))
err.comp.dat[,lvl := str_count( fc.item, "/") + 1 ]

# some basic reports and levels of comparison for the resultts
err.comp.dat[,median(rae, na.rm=TRUE),by=list(k,method)]
dcast(err.comp.dat,formula=lvl+method~k,median,na.rm=TRUE,value.var="rae")
dcast(err.comp.dat,formula=method~lvl,median,na.rm=TRUE,value.var="rae")

ggplot(err.comp.dat[k<14], aes(x=factor(k),y = rae, colour = method, shape = method, size=1)) +  stat_summary(fun.y = median,na.rm=TRUE,geom="point") + 
  theme_bw() +facet_wrap(~lvl,ncol=1)+scale_y_continuous(limits = c(0,0.5))+coord_flip()

ggplot(err.comp.dat[k<14], aes(x=method, y = rae, colour = method)) + geom_boxplot() +
  theme_bw() +facet_wrap(~lvl,ncol=1)+scale_y_continuous(limits = c(0,2))+coord_flip()

# now compare the error for each point forecast between the 2 methods and rank them/flag which is best
point.comp = data.table(dcast(err.comp.dat,fc.item+o+k~method,fun.aggregate=sum,value.var="rae"))
point.comp

# consistent data format required:
# 
names(fc2)

# two types of comparison. like-for-like with rankings or summary measures
# like-for like requires ranking within group (i.e. same origin & horizon)

err.comp = 



# functions to get summary stats
acc.summary.week = data.table(cbind(dcast(data=fcast.weekly, formula=fc.item+k~., fun.aggregate=median,value.var="rae"),
                                    dcast(data=fcast.weekly, formula=fc.item+k~., fun.aggregate=mean,value.var="rae")[-1:-2]))

names(acc.summary.week)[-1:-2] = c("mdrae","mrae")


if (TRUE == FALSE) 
{
  library(ggplot2)
  p = ggplot(data = fcast.weekly, aes(x=rae)) + geom_histogram() + ggtitle(this.fc.item)  ; print(p)
  p = ggplot(data = fcast.weekly, aes(x = as.factor(k), y = rae)) + geom_boxplot() + ggtitle(this.fc.item) ; print (p)
  
  fcast.weekly[,list(mdrae = median(rae), mrae = mean(rae)),by="k"]
  
  with(fcast.weekly,boxplot(rae~k))
}     



# pick fields of interest
# add other fields e.g. chain/store/level/category/importance......
acc.summary.week[,lvl := str_count( fc.item, "/") + 1 ]
acc.summary.week



library(ggplot2)
qplot(data=acc.summary.week[k!=14],x=factor(k),y=mdrae, geom = "boxplot") + coord_flip() + facet_wrap(~lvl,ncol=1)

## FUNCTIONS FOR RERIEVING & FORMATTING RESULTS

library(reshape2)
setwd(pth.dropbox.code) ; source("./results/results_prepare_functions.R")

results = f_consolidate.errors()

# prepare single product results to compare the 6 levels of aggregation for each forecasting method
res2 = results[freq == freq.cycle]
dcast(res2, 
      lvl+Level~freq+method, 
      fun.aggregate = mean, na.rm = TRUE,
      value.var = "ape")



# some simple stats to show the number of errors calculated for each method/frequency/cycle
res.counts = results[,.N,by=list(method,freq,freq.cycle, Level)]
dcast(res.counts, freq+freq.cycle+Level~method, fun.aggregate=sum,value.var="N")


dcast(results, Level+fc.item~freq+freq.cycle+method, fun.aggregate= median,na.rm=TRUE, value.var="ape")
dcast(results, Level+freq+freq.cycle~method, fun.aggregate= median,na.rm=TRUE, value.var="ape")

res.summary = f_results.summarise(results)


library(ggplot2)
ggplot(data = res.summary[lvl==1], aes(x = method, y = mdape)) + geom_point() +  coord_flip() + facet_wrap(~freq)+
    ylim(0,0.5)


ggplot(data=results[lvl==1], aes(x= fc.item, y=ase.naive, colour = method)) + geom_boxplot() + facet_wrap(~freq) +coord_flip()
ggplot(data=results[lvl==1], aes(x= fc.item, y=rae.naive)) + geom_boxplot() + coord_flip()


#res=f_results.load("E:/data/errors/reg_52_52_beer_3_.rds")
#res.summary = f_results.summarise(res)



x = readRDS("E:/data/errors/ets_52_12_3_beer_00-01-18200-53030.rds")
max(x$o) ;x

x[,max(o),by=fc.item]
tail(x,100)


>>>>>>> fd254f4ffc3e07bab225158f9aa0250bf0e5611c

results = f_consolidate.errors()[freq.cycle=="MONTH"]
res.counts = results[,.N,by=list(method,freq,freq.cycle, Level)]

<<<<<<< HEAD
# some summary output
dcast(res.counts, freq+freq.cycle+Level~method, fun.aggregate=sum,value.var="N")
dcast(results, Level+fc.item~freq+freq.cycle+method, fun.aggregate= median,na.rm=TRUE, value.var="ape")
dcast(results, Level+freq+freq.cycle~method, fun.aggregate= median,na.rm=TRUE, value.var="ape")
=======
#res = readRDS("E:/data/errors/all.rds")
>>>>>>> fd254f4ffc3e07bab225158f9aa0250bf0e5611c


res.summary = f_results.summarise(results)



## ploting results

library(ggplot2)
ggplot(data = res.summary[lvl==3], aes(x=fc.item, y = mdape)) + 
    geom_point(aes(colour = method, shape=method, size = 6)) +  coord_flip() + facet_wrap(~freq, ncol=1) +
    ylim(0,0.5) + theme_bw()


ggplot(data=results[lvl==1], aes(x= fc.item, y=ase.naive, colour = method)) + 
    geom_boxplot() + facet_wrap(~freq,ncol=1) +coord_flip()

ggplot(data=results[lvl==2], aes(x= fc.item, y=ase.naive, colour = method)) + 
    geom_boxplot() + facet_wrap(~freq,ncol=1) +coord_flip()

ggplot(data=results[lvl==2], aes(x= fc.item, y=rae.naive, colour =method)) + geom_boxplot() + coord_flip()




#res=f_results.load("E:/data/errors/reg_52_52_beer_3_.rds")
#res.summary = f_results.summarise(res)

x = readRDS("E:/data/errors/ets_52_12_3_beer_00-01-18200-53030.rds")
max(x$o) ;x

x[,max(o),by=fc.item]
tail(x,100)









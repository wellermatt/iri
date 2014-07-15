## FUNCTIONS FOR RERIEVING & FORMATTING RESULTS


library(ggplot2)  ;  library(reshape2)

setwd(pth.dropbox.code) ; source("./results/results_prepare_functions.R")

# prepare single product results to compare the 6 levels of aggregation for each forecasting method
results = f_consolidate.errors()

#results = f_consolidate.errors(upc = NULL) # this should be loaded from file

# taking data only for freq = freq.cycle
res2 = results[freq == freq.cycle]  ; results = NULL

# overall summary of accuracy MAPE
dcast(res2, 
      lvl+Level~freq+method, 
      fun.aggregate = mean, na.rm = TRUE,
      value.var = "sape")

# item level forecast accuracy MAPE for multiple items
dcast(res2[lvl==1], 
      lvl+Level+fc.item~freq+method, 
      fun.aggregate = mean, na.rm = TRUE,
      value.var = "sape")

# prepare data for boxplots: MAE per item per method per periodicity/level
plot.data = dcast(res2,lvl+Level+fc.item+freq+method~., fun.aggregate = mean, na.rm = TRUE, value.var= "sape")
names(plot.data)[6] = "MdAPE"
p = ggplot(data=plot.data, aes(y=MdAPE,colour=method, x=method)) + geom_boxplot() + facet_grid(lvl+Level~freq,scales="free")
p + ggtitle("Comparison of Accuracy (MdAPE) at each level of aggregation:\nTop 10 items in beer category")


# counts of results at each level
res.counts = res2[,.N, by = list(method,freq, freq.cycle, Level)]
dcast(res.counts, freq + Level ~ method, fun.aggregate = sum,value.var="N")

# tabular summary of MdAPE for each level
dcast(res2, Level + fc.item ~ freq + method, fun.aggregate = mean,na.rm=TRUE, value.var="sape")
dcast(res2, Level + freq ~ method, fun.aggregate = mean,na.rm=TRUE, value.var="sape")

# summarise results to calculate the key error measures for each forecast item
res.summary.fc.item = f_results.summarise(res2)

ggplot(data = res.summary.fc.item, aes(x = smape, y = fc.item, colour = method)) + geom_point(size=2.5) + facet_grid(lvl~freq,scales="free",space="free_y") + theme_bw()

ggplot(data = res.summary[lvl==1], aes(x = method, y = mdape)) + geom_point() +  coord_flip() + facet_wrap(~freq)+
    ylim(0,0.5)


ggplot(data=res2, aes(x= fc.item, y=ase.naive, colour = method)) + geom_boxplot() + facet_grid(Level~freq) +coord_flip()
ggplot(data=results[lvl==1], aes(x= fc.item, y=rae.naive)) + geom_boxplot() + coord_flip()


#res=f_results.load("E:/data/errors/reg_52_52_beer_3_.rds")
#res.summary = f_results.summarise(res)



x = readRDS("E:/data/errors/ets_52_12_3_beer_00-01-18200-53030.rds")
max(x$o) ;x

x[,max(o),by=fc.item]
tail(x,100)


<<<<<<< HEAD
=======






>>>>>>> 2f4c7aa04c41bde72bde5fad350d138224cae07a
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









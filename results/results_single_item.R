

library(reshape2)


## FUNCTIONS FOR RERIEVING & FORMATTING RESULTS

setwd(pth.dropbox.code) ; source("./results/results_prepare_functions.R")


results = f_consolidate.errors()[freq.cycle=="MONTH"]
res.counts = results[,.N,by=list(method,freq,freq.cycle, Level)]

# some summary output
dcast(res.counts, freq+freq.cycle+Level~method, fun.aggregate=sum,value.var="N")
dcast(results, Level+fc.item~freq+freq.cycle+method, fun.aggregate= median,na.rm=TRUE, value.var="ape")
dcast(results, Level+freq+freq.cycle~method, fun.aggregate= median,na.rm=TRUE, value.var="ape")


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









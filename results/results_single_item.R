library(reshape2)
## FUNCTIONS FOR RERIEVING & FORMATTING RESULTS

setwd(pth.dropbox.code) ; source("./results/results_prepare_functions.R")

results = f_consolidate.errors()

## load the consolidated results

res = readRDS("E:/data/errors/all.rds")

res[,`:=`(freq = factor(freq, levels = c(12,52),labels=c("MONTH", "WEEK")),
          freq.cycle = factor(freq.cycle,levels = c(12,52),labels=c("MONTH", "WEEK")))]

res.counts = res[,.N,by=list(method,freq,freq.cycle, Level)]

dcast(res.counts, freq+freq.cycle+Level~method, fun.aggregate=sum,value.var="N")


 24*13*2700
dcast(res, Level+fc.item~method+freq+freq.cycle, fun.aggregate= median,na.rm=TRUE, value.var="ape")

dcast(res, freq+Level~method, fun.aggregate= median,na.rm=TRUE, value.var="ape")
res.summary = f_results.summarise(res)


library(ggplot2)
ggplot(data = res.summary[lvl==1], aes(x = method, y = mdape)) + geom_point() +  coord_flip() + facet_wrap(~freq)+
    ylim(0,0.5)


ggplot(data=res[lvl==1], aes(x= fc.item, y=ase.naive)) + geom_boxplot() + coord_flip()
ggplot(data=res[lvl==1], aes(x= fc.item, y=rae.naive)) + geom_boxplot() + coord_flip()


#res=f_results.load("E:/data/errors/reg_52_52_beer_3_.rds")
#res.summary = f_results.summarise(res)



x = readRDS("E:/data/errors/reg_52_12_3_beer_.rds")
max(x$o) ;x

x[,max(o),by=fc.item]
tail(x,1000)

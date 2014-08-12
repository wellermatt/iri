## FUNCTIONS FOR RETRIEVING & CALCULTATING ERROS, FORMATTING & DISPLAYING RESULTS

library(ggplot2)  ;  library(reshape2)
setwd(pth.dropbox.code) ; source("./results/results_prepare_functions.R")

par.category = "beer"
par.upc = "00-01-18200-53030"
par.load.file = FALSE

# prepare single product results to compare the 6 levels of aggregation for each forecasting method
if (par.load.file == FALSE) {
    results = f_consolidate.errors(upc = par.upc, par.category = par.category, opt.save = TRUE ) # this should be loaded from file    
    # taking data only for freq = freq.cycle
    res2 = results[freq == freq.cycle]
} else {
    res2 = readRDS(paste0("E:/data/output/errors/", par.category, "_.rds"))
}
results = NULL
names(res2)[c(1,2,4,22:23,17)]
res2=res2[lvl==1 & o==48]
dt = res2


# for rankings
#res3 = dcast(data=res2,formula=lvl+Level+fc.item+freq+o+h~method,fun.aggregate=mean,na.rm=TRUE,value.var="sape")
err.rank = f_errors.rank(res2,par.melt=FALSE,par.recast=FALSE)

ggplot(err.rank,aes(y=value,x = method,fill=method))+geom_boxplot() +
    scale_y_continuous(limits=c(1,2)) + facet_grid(Level~freq) +
    ggtitle("Rankings across items") + coord_flip()


# summary of the variable of interest
summary(res2$sape)

# overall summary of accuracy sMAPE
dcast(res2, 
      lvl+Level~freq+method, 
      fun.aggregate = mean, na.rm = TRUE,
      value.var = "sape")

# item level forecast accuracy sMAPE for multiple items
dcast(res2, 
      lvl+Level+fc.item+freq+o+h~method, 
      fun.aggregate = mean, na.rm = TRUE,
      value.var = "sape")


##### prepare data for boxplots: sMAPE per item per method per periodicity/level
# data step
plot.data = dcast(res2,lvl+Level+fc.item+freq+method~., fun.aggregate = mean, na.rm = TRUE, value.var= "sape")
names(plot.data)[6] = "sMAPE"
# plotting steps
p = ggplot(data=plot.data, aes(y=sMAPE,colour=method, x=method)) + geom_boxplot() + facet_grid(freq+lvl+Level~.) + 
    coord_flip() +xlab("") + theme_bw()
p + ggtitle("Comparison of Accuracy (sMAPE) at each level of aggregation:\nTop 10 items in beer category")




# counts of results at each level
res.counts = res2[,.N, by = list(method,freq, freq.cycle, Level)]
dcast(res.counts, freq + Level ~ method, fun.aggregate = sum,value.var="N")

# tabular summary of sMAPE for each level
dcast(res2, Level + fc.item ~ freq + method, fun.aggregate = mean,na.rm=TRUE, value.var="sape")
dcast(res2, Level + freq ~ method, fun.aggregate = mean,na.rm=TRUE, value.var="sape")

# summarise results to calculate the key error measures for each forecast item
res.summary.fc.item = f_results.summarise(res2)

ggplot(data = res.summary.fc.item, aes(x = smape, y = fc.item, colour = method)) + geom_point(size=2.5) + facet_grid(lvl~freq,scales="free",space="free_y") + theme_bw()

ggplot(data = res.summary[lvl==1], aes(x = method, y = mdape)) + geom_point() +  coord_flip() + facet_wrap(~freq)+
    ylim(0,0.5)


ggplot(data=res2, aes(x= fc.item, y=ase.naive, colour = method)) + geom_boxplot() + facet_grid(Level~freq) +coord_flip()
ggplot(data=results[lvl==1], aes(x= fc.item, y=rae.naive)) + geom_boxplot() + coord_flip()


library(ggplot2)
ggplot(data = res.summary[lvl==3], aes(x=fc.item, y = mdape)) + 
    geom_point(aes(colour = method, shape=method, size = 6)) +  coord_flip() + facet_wrap(~freq, ncol=1) +
    ylim(0,0.5) + theme_bw()


ggplot(data=results[lvl==1], aes(x= fc.item, y=ase.naive, colour = method)) + 
    geom_boxplot() + facet_wrap(~freq,ncol=1) +coord_flip()

ggplot(data=results[lvl==2], aes(x= fc.item, y=ase.naive, colour = method)) + 
    geom_boxplot() + facet_wrap(~freq,ncol=1) +coord_flip()

ggplot(data=results[lvl==2], aes(x= fc.item, y=rae.naive, colour =method)) + geom_boxplot() + coord_flip()




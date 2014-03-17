
rm(list=ls())
getwd()
#setwd()
source('D:/Dropbox/Dropbox/HEC/Code/exp1.1/.Rprofile')


library("forecast") ; library("xts") ; library("data.table") ; library("reshape2")
library("ggplot2")

setwd(pth.dropbox.code) ; source("./DataAdaptor/00_data_adaptor_test.R")
setwd(pth.dropbox.code) ; source("./ModelFitting/ets/ets_functions.R")

#============== DATA LOADING =====================
# get the necessary data for a specific item
spw = f_da.reg.cat.test(par.category="beer", par.periodicity="weekly")
spm = f_da.reg.cat.test(par.category="beer", par.periodicity="445")

items = spw[!is.na(IRI_KEY),as.character(unique(fc.item))]
items = spm[,as.character(unique(fc.item))]

#=============== TESTING =================
test.single = FALSE
test.multi = FALSE

if (test.single == TRUE) {
    item.id=2
    ssm = spm[fc.item == items[item.id]]
    this.roll = f_run.item(ssm, h=3)
    Err = this.roll$Err
}

if (test.multi == TRUE) {
    multi.item.results =
        lapply(1:2,   #length(items)
               function(i) { print(items[i])
                             ssm = spm[fc.item == items[i]]
                             this.roll = f_run.item(ssm, h = 3)
                             Err = this.roll$Err
                             Err[,fc.item := items[i]]
               })
    
    #saveRDS(object=rbindlist(multi.item.results),file="errors.rds")
}

library(stringr)
setwd(pth.dropbox.data) ; Err2 = readRDS("errors.rds")

Err3 = data.table(dcast(data=Err2,formula=fc.item~k,fun.aggregate=median,value.var="rae"))
Err3[,lvl := str_count( fc.item, "/")+1 ]
Err3.melt = data.table(melt(Err3,variable.name = "k", id=c("fc.item","lvl")))

qplot(data = Err3.melt,y=value, x=factor(lvl), geom="boxplot") + geom_jitter()
qplot(data = Err3.melt[lvl>1],x=value, colour=factor(lvl), geom="density") 
qplot(data = Err3.melt,x=value, geom="histogram") + facet_wrap(facets=~lvl, ncol=1)

#f_summary.plots(Err)
#Err

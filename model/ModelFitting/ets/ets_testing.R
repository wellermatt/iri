
#rm(list=ls())
getwd()
setwd(pth.dropbox.code)
source('./.Rprofile')


library("forecast") ; library("xts") ; library("data.table") ; library("reshape2")
library("ggplot2")

setwd(pth.dropbox.code) ; source("./data/DataAdaptor/00_data_adaptor_test.R")
setwd(pth.dropbox.code) ; source("./model/ModelFitting/ets/ets_functions.R")

#============== DATA LOADING =====================
# get the necessary data for a specific item
spw = f_da.reg.cat.test(par.category="beer", par.periodicity="weekly")
spm = f_da.reg.cat.test(par.category="beer", par.periodicity="445")

items = spw[!is.na(IRI_KEY),as.character(unique(fc.item))]
items = spm[,as.character(unique(fc.item))]

#=============== TESTING =================
test.single = TRUE
test.multi = FALSE

if (test.single == TRUE) {
    item.id=1
    ss = spm[fc.item == items[item.id]]
    this.roll = f_ets.run.item(ss=ss, frequency = 12, h.max=3)
    Err = this.roll$Err
}

if (test.multi == TRUE) {
    multi.item.results =
        lapply(1:length(items),
               function(i) { print(items[i])
                             ss = spm[fc.item == items[i]]
                             this.roll = f_ets.run.item(ss, frequency = 12, h.max = 3)
                             Err = this.roll$Err
                             Err[,fc.item := items[i]]
               })
    setwd(pth.dropbox.data)
    saveRDS(object=rbindlist(multi.item.results),file="./output/errors/ets_445_fast.rds")
}

library(stringr)
setwd(pth.dropbox.data) ; Err2 = readRDS("./output/errors/ets_445_fast.rds")

Err3 = data.table(dcast(data=Err2,formula=fc.item~k,fun.aggregate=median,value.var="rae"))
Err3[,lvl := str_count( fc.item, "/")+1 ]
Err3.melt = data.table(melt(Err3,variable.name = "k", id=c("fc.item","lvl")))

qplot(data = Err3.melt,y=value, x=factor(lvl), geom="boxplot") + geom_jitter()
qplot(data = Err3.melt[lvl>1],x=value, colour=factor(lvl), geom="density") 
qplot(data = Err3.melt,x=value, geom="histogram") + facet_wrap(facets=~lvl, ncol=1)


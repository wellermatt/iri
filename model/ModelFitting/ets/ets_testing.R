
setwd(pth.dropbox.code)
rm(list=ls())
source('./.Rprofile')


library("forecast") ; library("data.table") ; library("reshape2")
library("ggplot2")

setwd(pth.dropbox.code) ; source("./data/DataAdaptor/00_data_adaptor_test.R")
setwd(pth.dropbox.code) ; source("./model/ModelFitting/ets/ets_functions.R")
setwd(pth.dropbox.code) ; source("./other/GenericRoutines/useful_functions.R")

#============== DATA LOADING =====================
# get the necessary data for a specific item
#spw = f_da.reg.cat.test(par.category="beer", par.periodicity="weekly")
#spm = f_da.reg.cat.test(par.category="beer", par.periodicity="445")   #     "00-01-18200-53030"
spm = f_da.reg.cat.all(par.category = "beer", par.periodicity = "445")  # "07-01-18200-53025"

#items = spw[!is.na(IRI_KEY),as.character(unique(fc.item))]
items = spm[,as.character(unique(fc.item))]

#spm = droplevels(spm[fc.item %in% items[1:8]])

#=============== TESTING =================
test.single = FALSE
test.multi = FALSE
test.multicore = TRUE


if (test.single == TRUE) {
    item.id=1
    ss = spm[fc.item == items[item.id]]
    this.roll = f_ets.run.item(ss=ss, frequency = 12, h.max=3)
    Err = this.roll$Err
}

if (test.multi == TRUE) {
    multi.item.results = rbindlist(
        lapply(1:30,#length(items),
               function(i) { this.item = items[i]
                             print(this.item)
                             ss = spm[fc.item == items[i]]
                             this.roll = f_ets.run.item(ss, frequency = 12, h.max = 3)
                             Err = this.roll$Err
                             Err$fc.item = this.item
                             Err
               }))
    setwd(pth.dropbox.data)
    saveRDS(object = multi.item.results, file="./output/errors/ets_445_fast_all.rds")
}

if (test.multicore == TRUE) {
    library(doParallel)
    registerDoParallel(3)
    spm[,fc.item := factor(fc.item)]
    setkeyv(spm, c("fc.item"))  #,"period_id"))
    multi.item.results =
        foreach(dt.sub = isplitDT(spm, levels(spm$fc.item)),
                .combine='dtcomb', .multicombine=TRUE,
                .packages=c("data.table", "forecast", "reshape2")) %dopar%
        {
            fc.item = dt.sub$key[1]
            print(fc.item)
            #ss = spm[fc.item == items[i]]
            this.roll = f_ets.run.item(dt.sub$value, frequency = 12, h.max = 3)
            Err = this.roll$Err
            Err$fc.item = fc.item
            Err
        }
    setwd(pth.dropbox.data)
    print(multi.item.results)
    saveRDS(object=rbindlist(multi.item.results),file="./output/errors/ets_445_fast_all.rds")
}


# 
# library(stringr)
# setwd(pth.dropbox.data) ; Err2 = readRDS("./output/errors/ets_445_fast.rds")
# 
# Err3 = data.table(dcast(data=Err2,formula=fc.item~k,fun.aggregate=median,value.var="rae"))
# Err3[,lvl := str_count( fc.item, "/")+1 ]
# Err3.melt = data.table(melt(Err3,variable.name = "k", id=c("fc.item","lvl")))
# 
# qplot(data = Err3.melt,y=value, x=factor(lvl), geom="boxplot") + geom_jitter()
# qplot(data = Err3.melt[lvl>1],x=value, colour=factor(lvl), geom="density") 
# qplot(data = Err3.melt,x=value, geom="histogram") + facet_wrap(facets=~lvl, ncol=1)


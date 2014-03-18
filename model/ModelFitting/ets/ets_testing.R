print(getwd())
source('D:/Git/iri/.Rprofile')
setwd(pth.dropbox.code)
#rm(list=ls())




library("forecast") ; library("data.table") ; library("reshape2")
library("ggplot2")

setwd(pth.dropbox.code) ; source("./data/DataAdaptor/00_data_adaptor_test.R")
setwd(pth.dropbox.code) ; source("./model/ModelFitting/ets/ets_functions_new.R")
setwd(pth.dropbox.code) ; source("./other/GenericRoutines/useful_functions.R")

print(ls())
#=============== TESTING =================

#============== DATA LOADING =====================
# get the necessary data for a specific item
#spw = f_da.reg.cat.test(par.category="beer", par.periodicity="weekly")
#spm = f_da.reg.cat.test(par.category="beer", par.periodicity="445")   #     "00-01-18200-53030"
spm = f_da.reg.cat.all(par.category = "beer", par.periodicity = "445")  #,par.item= "07-01-18200-53025")

#items = spw[!is.na(IRI_KEY),as.character(unique(fc.item))]
items = spm[,as.character(unique(fc.item))]



test.single = FALSE
test.multi = FALSE
test.multicore = FALSE

L12 = TRUE

if (L12 == TRUE) {
    items.L12 = items[which(unlist(lapply(strsplit(items,"/"),length))<3)]
    spm = droplevels(spm[fc.item %in% items.L12])
}

if (test.single == TRUE)    ets.Err = f_ets.test.single(sp = spm)
if (test.multi == TRUE)    ets.Err = f_ets.test.multi(sp = spm)
if (test.multicore == TRUE)    ets.Err = f_ets.test.multicore(sp = spm, opt.dopar=TRUE)



#system.time(f_ets.test.single(sp = spm))
#system.time(f_ets.test.multi(sp = spm))
system.time(f_ets.test.multicore(sp = spm, opt.dopar=TRUE, i=321))



# 
# library(stringr)
setwd(pth.dropbox.data) ; Err2 = readRDS("./output/errors/ets_445_fast_all.rds")
# 
# Err3 = data.table(dcast(data=Err2,formula=fc.item~k,fun.aggregate=median,value.var="rae"))
# Err3[,lvl := str_count( fc.item, "/")+1 ]
# Err3.melt = data.table(melt(Err3,variable.name = "k", id=c("fc.item","lvl")))
# 
# qplot(data = Err3.melt,y=value, x=factor(lvl), geom="boxplot") + geom_jitter()
# qplot(data = Err3.melt[lvl>1],x=value, colour=factor(lvl), geom="density") 
# qplot(data = Err3.melt,x=value, geom="histogram") + facet_wrap(facets=~lvl, ncol=1)







### BACKUP 


# f_ets.test.single = function(sp, freq = 12, h.max=3) {
#     i = 1
#     this.item = items[i]
#     ss = sp[fc.item == items[item.id]]
#     #print(ss)
#     this.roll = f_ets.run.item(ss=ss, freq = freq, h.max=h.max,Trace=TRUE)
#     Err = this.roll$Err
#     Err$fc.item = this.item
#     Err
# }
# 
# 
# f_ets.test.multi = function(sp, freq = 12, h.max=3) {
#     multi.item.results = rbindlist(
#         lapply(1:3,#length(items),
#                function(i) { this.item = items[i]
#                              print(this.item)
#                              ss = sp[fc.item == items[i]]
#                              this.roll = f_ets.run.item(ss, freq = 12, h.max = 3,Trace=TRUE)
#                              Err = this.roll$Err
#                              Err$fc.item = this.item
#                              Err
#                }))
#     setwd(pth.dropbox.data)
#     saveRDS(object = multi.item.results, file="./output/errors/ets_445_fast_all.rds")
#     
#     multi.item.results
# }
# 
# 
# f_ets.test.multicore = function(sp, freq = 12, h.max=3)
# {
#     library(doParallel)
#     registerDoParallel(3)
#     spm[,fc.item := factor(fc.item)]
#     setkeyv(spm, c("fc.item"))  #,"period_id"))
#     multi.item.results =
#         foreach(dt.sub = isplitDT(spm, levels(spm$fc.item)),
#                 .combine='dtcomb', .multicombine=TRUE,
#                 .packages=c("data.table", "forecast", "reshape2")) %dopar%
# {
#     fc.item = dt.sub$key[1]
#     print(fc.item)
#     #ss = spm[fc.item == items[i]]
#     this.roll = f_ets.run.item(dt.sub$value, freq = 12, h.max = 3,Trace=TRUE)
#     Err = this.roll$Err
#     Err$fc.item = fc.item
#     Err
# }
#     setwd(pth.dropbox.data)
#     print(multi.item.results)
#     saveRDS(object=rbindlist(multi.item.results),file="./output/errors/ets_445_fast_all.rds")
# }
# 
# 
# 

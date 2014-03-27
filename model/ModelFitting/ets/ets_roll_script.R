print(paste(">> Start directory =", getwd()))
if (!exists("machine")) {
    if (.Platform$OS.type == "unix") {
        source("~/projects/iri/.Rprofile")
    } else {
        source("E:/Git/iri/.Rprofile") #source("~/projects/iri/.Rprofile") #else source("E:/Git/iri/.Rprofile")
    }
}
print(paste("Machine =",machine))
print(paste("Platform = ", platform))

##==== parameters ========         categories = c("milk","beer","carbbev")

par.category = "beer"
freq = 52
freq.cycle = 12
h.max = if(freq == 52) 13 else 3
Level = 3
par.upc =    "00-01-18200-53030"      # NULL   # "07-01-18200-53025 
par.fc.item = NULL #"00-01-18200-53030/57"      #  NULL # 00-01-41383-09036/12#  NULL # "00-02-28000-24610/99"   #NULL #"00-01-18200-53030/104/228694" # NULL# "00-01-18200-53030/57" #"00-01-18200-53030/104/228694"
cores = 1
TRACE = 0

# replace with command line options
args <- commandArgs(trailingOnly = TRUE)
print (args)
if (length(args)>0)  {
    par.category = args[1]
    freq = as.integer(args[2])
    freq.cycle = as.integer(args[3])
    h.max = as.integer(args[4])
    Level = as.integer(args[5])
    par.upc = args[6] ; if(par.upc == "NULL") par.upc = NULL
    par.fc.item = args[7] ; if(par.fc.item == "NULL") par.fc.item = NULL
    cores = as.integer(args[8])
}

print(ls())

### ============ LIBRARIES & SOURCE CODE ==============

library("forecast") ; library("data.table") ; library("reshape2")
library("ggplot2")  ; library("microbenchmark") ; library("foreach")
library("snow") ; library("doSNOW")

## LOCAL CODE FILES
setwd(pth.dropbox.code) ; source("./data/DataAdaptor/00_data_adaptor_test.R")
setwd(pth.dropbox.code) ; source("./data/DataAdaptor/10_load_data_various.R")
setwd(pth.dropbox.code) ; source("./model/ModelFitting/ets/ets_functions.R")
setwd(pth.dropbox.code) ; source("./other/GenericRoutines/useful_functions.R")

#=============== TESTING =================

print(ls())
par.periodicity = if (freq == 52) "weekly" else "445"


sp = f_adaptor.reg.cat.all(par.category = par.category, par.periodicity, 
                           par.upc = par.upc, par.fc.item=par.fc.item, Level = Level, univariate = TRUE)
print(sp)

this.time = system.time(res.w <- 
                            f_ets.rolling.multicore(sp=sp, par.category=par.category,
                                             freq=freq, freq.cycle = freq.cycle, h.max=h.max,
                                             cores=cores, parMethod = NULL, TRACE))

test.stats = data.table(method = "ets", periodicity = par.periodicity, item_count = length(unique(sp$fc.item)), cores = cores, this.time = this.time[3])
print(test.stats)



# create a file name and save the results
setwd(paste0(pth.dropbox.data,"/output/errors/")  )
if (platform=="windows") setwd("E:/data/errors/")
fil = paste0(paste("ets", freq, freq.cycle, par.category, Level, par.upc,sep="_"), ".rds")
saveRDS(object = res.w, file = fil)



TEST = FALSE
if (TEST == TRUE) {
    
    #y = ts(rnorm(72,100,20),start = 2001, freq=12)
    #microbenchmark(f_ets.roll.fc.item(y, h.max=3,freq.cycle=12,reoptimise=FALSE), times=1)
    
    # low level testing for a single item
    #sp = f_adaptor.reg.cat.all(par.category = par.category, par.periodicity = "445", 
    #                      par.upc = "00-01-18200-53030", Level = 1,univariate = TRUE)
    #y = ts(sp$UNITS,start=c(2001,1),frequency=12)
    #f_ets.roll.fc.item(y, h.max=3, forecast.cycle="monthly")
    
}


# 
# 
# # 445 ets
# if (freq == 12) {
#     sp = f_adaptor.reg.cat.all(par.category = par.category, par.periodicity = "445", 
#                                par.upc = "00-01-18200-53030", Level = Level, univariate = TRUE)
#     system.time(res.m <- f_ets.rolling.multicore(sp=sp, par.category=par.category,
#                                                  freq=freq, freq.cycle = freq.cycle, h.max=h.max,
#                                                  cores=cores, parMethod = NULL))
#     


####=======
# serious need of a cleanup below here!!!

    
    #test.multicore = TRUE ; system.time(f_ets.test("milk","445"))
    #test.multicore = TRUE ; system.time(f_ets.test("beer","445"))
    #test.multicore = TRUE ; system.time(f_ets.test("carbbev","445"))
    
    
    #y=ts(rnorm(312,100,20),start = 2001, freq=52)
    #microbenchmark(f_ets.roll.fc.item(y,h.max=13,forecast.cycle="monthly"),times=10)
    #microbenchmark(f_ets.roll.fc.item(y,h.max=13,forecast.cycle="weekly"),times=10)    


#============== DATA LOADING =====================
# get the necessary data for a specific item
# sp for multiple categories

# sp.all = rbindlist(lapply(categories, function(x) data.table(category = x, f_load_data_sp(par.category=x))))[,c(1:3,7),with=F]

# write.csv(dcast(sp.all,period_id~category+fc.item,sum,value.var="UNITS"),file = "E:/items.ts.csv")
# write.csv(dcast(sp.all, category+fc.item~period_id,sum,value.var="UNITS"),file = "E:/items.ts.flip.csv")


test.single = FALSE
test.multi = FALSE
test.multicore = TRUE


#f_ets.test("beer","445")
#f_ets.test("carbbev","445")


#unique(sp$fc.item)


#========================
#system.time(f_ets.test.single(sp = sp))
#system.time(f_ets.test.multi(sp = sp))


#setwd(pth.dropbox.data)
#milk = readRDS("./output/errors/ets_12_milk.rds")
#beer12 = readRDS("./output/errors/ets_12_beer.rds")
#beer52 = readRDS("./output/errors/ets_52_beer.rds")
#carbbev = readRDS("./output/errors/ets_12_carbbev.rds")



# 
# library(stringr)
#setwd(pth.dropbox.data) ; Err2 = readRDS("./output/errors/ets_445_fast_all.rds")
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

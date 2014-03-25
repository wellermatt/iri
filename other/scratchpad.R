source("E:/Git/iri/.Rprofile")
library("stringr"); library(reshape2)

setwd(pth.dropbox.data)

f_errors.calculate = function(dt)
{
    # receives a data.table and adds the errors in various guises
    dt[,e:=fc-act]
    dt[,ae:=abs(e)]
    dt[,ape:=ae/act]
    dt[,sape:=(2*ae)/(act+fc)]
    #dt[,ase.snaive:=]
    dt[,rae.snaive:=abs(fc-act)/abs(fc.snaive-act)]    # check
    dt[,rae.naive:=abs(fc-act)/abs(fc.naive-act)]    # check
    dt[,lvl := str_count( fc.item, "/") + 1 ]
}

## BEER ETS ERROR PREP
beer.12.ets = data.table(category=strsplit("beer.12.ets","\\.")[[1]][1],
                         periodicity=strsplit("beer.12.ets","\\.")[[1]][2],
                         method = strsplit("beer.12.ets","\\.")[[1]][3],
                         readRDS("./output/errors/ets_12_beer.rds")) 
beer.52.ets = data.table(category=strsplit("beer.52.ets","\\.")[[1]][1],
                         periodicity=strsplit("beer.52.ets","\\.")[[1]][2],
                         method = strsplit("beer.52.ets","\\.")[[1]][3],
                         readRDS("./output/errors/ets_52_beer.rds")) 

beer.52.reg = data.table(category=strsplit("beer.52.reg","\\.")[[1]][1],
                         periodicity=strsplit("beer.52.reg","\\.")[[1]][2],
                         method = strsplit("beer.52.reg","\\.")[[1]][3],
                         readRDS("./output/errors/reg_52_beer_L123all.rds")[,-2,with=F]) 

res.comp = rbindlist(list(beer.12.ets,beer.52.ets, beer.52.reg) )

Err = f_errors.calculate(res.comp[method == "reg"])
Err = f_errors.calculate(beer.52.reg)

Err[]

dcast(data=Err,lvl+periodicity~method,fun.aggregate=median,na.rm=TRUE,value.var="sape")
dcast(data=Err,lvl+periodicity~method,fun.aggregate=median,na.rm=TRUE,value.var="rae.snaive")
dcast(data=Err,lvl+periodicity~method,fun.aggregate=median,na.rm=TRUE,value.var="rae.naive")

library(ggplot2)

ggplot(data=Err, aes(x=lvl, y=rae.snaive))  + geom_boxplot()
#==========================================================================
y = 1:10
y[4] = NA
is.na(y)
x = y[!is.na(y)]




### micro benchmark
freq=52; h.max = 13 ; o=52
y = ts(1:104, start = c(1,1), freq=freq)
h = h.max
# forecast package naive calcs
#yhat.naive = naive(window(y, start = 1, end = (1.9999)))$mean
#yhat.snaive = snaive(window(y, start = 1, end = (1.9999)))$mean
yhat.rwf = microbenchmark(rwf(window(y, start = 1, end = (1.9999)))$mean, times=100)
yhat.naive = rep(y[o], h)
yhat.snaive = y[(o-freq+1):(o-freq+h.max)]



library(microbenchmark)
microbenchmark(rwf(window(y, start = 1, end = (1.9999)),h=h,level=FALSE)$mean, times=1000, unit="ms")
microbenchmark(rep(y[o], h),times=1000, unit="ms")
yhat = y[o-freq]
plot(y)

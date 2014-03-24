
f_errors.calculate = function(dt)
{
    # receives a data.table and adds the errors in various guises
    dt[,e:=fc-act]
    dt[,ae:=abs(e)]
    dt[,ape:=ae/act]
    dt[,sape:=(2*ae)/(act+fc)]
}

beer12 = readRDS("./output/errors/ets_12_beer.rds")
beer52 = readRDS("./output/errors/ets_52_beer.rds")

beer52[,act:=as.numeric(act)]
beer52[,fc:=as.numeric(fc)]
beer12[,act:=as.numeric(act)]
beer12[,fc:=as.numeric(fc)]

f_errors.calculate(beer12)
f_errors.calculate(beer52)

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

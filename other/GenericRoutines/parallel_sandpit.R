
## sandpit for paralllel/multicore

f_par.select.paradigm = function () 
{
    
}



f_dt.test = function () {
    dt = data.table(i=rep(1:20,each=313), t=rep(1:313, times=20), y=rnorm(20*313,mean=100,sd=10))
    dt
}

dt = f_dt.test()
dt

library("forecast") ; library("parallel")

?parallel

getOption("mc.cores")

library(foreach)
library(parallel)

type = if (exists("mcfork",mode="function")) "FORK" else "PSOCK"
cores = 3
cl = makeCluster(cores, type = type)
clusterExport(cl, varlist="data.table", "forecast")

rbindlist(parLapply(cl,X=1:100, function(x) data.table(x,sqrt(x))))



#mcl = rbindlist(
system.time(mclapply(1:20, FUN=function(this.item) 
             {
             this.dt = dt[i==this.item]
             y = ts(this.dt$y, start=c(2001,1),frequency=12)
             mymodel = ets(y)
             forecast(mymodel,6)$mean
             }
))
#)
library(forecast)
dt = f_dt.test()
cl <- makeCluster(3)
clusterExport(cl,c("data.table","dt","forecast","ets"))

clusterApply(cl,x= 1:20,fun=function(x) 
{
    
    this.dt = dt[i==x]
    y = ts(this.dt$y, start=c(2001,1),frequency=12)
    mymodel = ets(y)
    forecast(mymodel,6)$mean
}
)


identical( dt, mcl)

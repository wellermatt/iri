
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

makeCluster(8)
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


cl <- makeCluster(getOption("cl.cores", 6))

clusterApply(cl, 1:20,fun=function(this.item) 
{
    this.dt = dt[i==this.item]
    y = ts(this.dt$y, start=c(2001,1),frequency=12)
    mymodel = ets(y)
    forecast(mymodel,6)$mean
}
)


identical( dt, mcl)

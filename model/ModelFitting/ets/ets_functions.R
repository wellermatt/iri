

# functions required:

# FIT: fit an ets model to 445/monthly/weekly data

f_ets.fit = function(y, fit.years = 4)
{
    y.fit = window(y,start=tsp(y)[1], end = tsp(y)[1] + fit.years - 0.0001)
    ets(y.fit)
}


# FORECAST


# ROLL


# SPLIT/AGGREGATE



# EVALUATE
#fit

f_diagnotics = function(y)
{
    seasonplot(y,col=rainbow(6))
    plot(stl(y,s.window="periodic"))
    plot(decompose(y))
    plot(HoltWinters(y))
    #
    fit = f_ets.fit(y, 4)
    plot(fit) ; 
    #fit.acc = accuracy(fit)
    #str(fit.acc)
    #class(fit.acc)
    #fit
    #fit$x
    
}

# need generic functions to handle time series of varying lengths now...
f_roll.fc.item = function(y, h=6, PRINT = FALSE) {

    # rules around length of series (n), horizon (h), splitting rules
    n <- length(y)
    k <- 48             # minimum data length for fitting a model
    err <- matrix(NA, n-k, h) 
    yhat <- matrix(NA, n-k, h)
    act <- matrix(NA, n-k, h)
    
    m = tsp(y)[3]               # m is seasonal period
    st <- tsp(y)[1]+(k-1)/m     # st = start period in ts terms
    
    for(i in 1:(n-k))
    {
        yhist <- window(y, end= st + (i-1)/m)
        yfuture <- window(y, start= st + i/m , end=st + (i+h-1)/m)
        #warnings()
        
        
        # select method
        #fit.tslm <-     tslm(xhist ~ trend + season, lambda=0)
        #fit.Arima <-    Arima(xshort, order=c(3,0,1), seasonal=list(order=c(0,1,1), period=12), 
        #                      include.drift=TRUE, lambda=0, method="ML")
        #fit.auto.arima <- auto.arima(xhist)
        #fit <- hw(yhist)$model
        
        # using ets presently
        fit <- ets(yhist, model="MMM", damped=TRUE)  # model="MMM",
        
        # do the forecasts      
        fcast <- forecast(fit, h=min(n-k-i+1, h))
        
        # optional printing
        if (PRINT == TRUE) {
            print(fit)
            print(yhist) 
            print(yfuture)
            print(fcast)
        }
        
        # record the data
        err[i,1:length(yfuture)] <- fcast[['mean']]-yfuture
        act[i,1:length(yfuture)] <- yfuture
        yhat[i,1:length(yfuture)] <- fcast[['mean']]
    }
    
    start.origin = k
    # prepare the accuracy stats
    act.melt = data.table(melt(act,value.name="y", varnames=c("t","k")),key="t,k") [!is.na(y)]
    yhat.melt = data.table(melt(yhat,value.name="yhat", varnames=c("t","k")),key="t,k") [!is.na(yhat)]
    Err = act.melt[yhat.melt]
    Err[,origin:=start.origin+t-1]
    Err[,fc.period:=start.origin+t+k-1]
    Err[,`:=` (e = yhat - y, ae = abs(yhat-y), 
               re = (yhat - y)/y, rae = abs((yhat - y)/y))]
    
    list(Err = Err, fit = fit)
}

f_plot.ts.fit = function(fit)
{
    require(ggplot2)
    
    # prepare data.table of insample errors & fitted values
    dt = data.table(y = as.numeric(fit$x), y.fit = as.numeric(fit$fitted))
    dt[,`:=`(E=y-y.fit, AE=abs(y-y.fit))]
    dt[,CE:=cumsum(E)]
    
    # make a plot with 
    p = ggplot(data = dt) + 
        
        # error blocks (residuals)
        geom_bar(aes(x=factor(1:71), y = E, geom = "bar", fill = "orange"), stat="identity") +
        
        # the actuals
        geom_line(aes(x=1:71, y = y), colour = "darkgreen") +
        geom_point(aes(x=1:71, y = y), colour = "darkgreen") +
        
        # fitted values
        geom_line(aes(x=1:71, y = y.fit), colour = "indianred2") +
        geom_point(aes(x=1:71, y = y.fit), colour = "indianred2") +
        
        # tracking signal
        #geom_point(aes(x=1:71, y = CE), alpha = 0.2) +
        #geom_line(aes(x=1:71, y = CE), alpha = 0.2) +
        
        # presentation
        theme_bw() + theme(legend.position = "none") +
        theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank()) +
        theme(axis.text.x  = element_text(angle=-90, vjust=0.5, size=10)) +
        ggtitle("Item-level plot for SKU\n") + xlab("\nTime Period (445 Periods)")
    
    #barplot(as.numeric(fit$x-fit$fitted), ylim = c(-3000,18000),col = "lightblue")
    #lines(as.numeric(fit$fitted), col = 4, type = "o") ; lines(as.numeric(fit$x), col = 2, type = "o")
    #hline(y=0)
    p
}



#a way to get separate plots for each plot
plot2 <- function(theplot, name, ...) {
    name <- paste0(name, ".png")
    png(filename=name,width=800,height=500)
    print(theplot)
    dev.off()
} #plotting function


f_run.item = function(ssm, h, fc.item = "00-01-18200-53030", pth = NULL)
{
    
    
    y = ts(ssm$UNITS, start=c(2001,1), frequency = 12)
    roll = f_roll.fc.item(y, h = h)
    
    #p = f_plot.ts.fit(roll$fit) 
    #plot2(theplot=p, name=paste0("D:/TEMP/IRI_PLOTS/",i))
    #ggsave(filename=fil, plot=p, width=8, height=8, units="cm")
    #list(roll,p)
    roll
}


f_summary.plots = function(Err) {
   
    ggplot(data=Err, aes(y=re, x = factor(k))) +geom_boxplot()
    ggplot(data=Err, aes(x=e, colour = factor(k))) + geom_density()+        theme_bw()
    ggplot(data=Err, aes(x=e)) + geom_histogram() +         facet_wrap(~k)
    
    Err[,list(n=length(y), me = mean(e), mae = mean(ae), mrae = mean(re)),by=list(k)]
}




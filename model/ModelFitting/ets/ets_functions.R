

# functions required:

# FIT: fit an ets model to 445/monthly/weekly data

ets2 = function (y, model = "ZZZ", damped = NULL, alpha = NULL, beta = NULL, 
                 gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL, 
                 lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999, 3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), 
                 nmse = 3, bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"), restrict = TRUE, ...) 
{
    opt.crit <- match.arg(opt.crit)
    bounds <- match.arg(bounds)
    ic <- match.arg(ic)
    if (any(class(y) %in% c("data.frame", "list", "matrix", "mts"))) 
        stop("y should be a univariate time series")
    y <- as.ts(y)
    ny <- length(y)
    y <- na.contiguous(y)
    if (ny != length(y)) 
        warning("Missing values encountered. Using longest contiguous portion of time series")
    orig.y <- y
    if (!is.null(lambda)) {
        y <- BoxCox(y, lambda)
        additive.only = TRUE
    }
    if (nmse < 1 | nmse > 10) 
        stop("nmse out of range")
    m <- frequency(y)
    if (sum((upper - lower) > 0) < 4) 
        stop("Lower limits must be less than upper limits")
    if (class(model) == "ets") {
        alpha = model$par["alpha"]
        beta = model$par["beta"]
        if (is.na(beta)) 
            beta <- NULL
        gamma = model$par["gamma"]
        if (is.na(gamma)) 
            gamma <- NULL
        phi = model$par["phi"]
        if (is.na(phi)) 
            phi <- NULL
        damped = (model$components[4] == "TRUE")
        model = paste(model$components[1], model$components[2], 
                      model$components[3], sep = "")
    }
    errortype <- substr(model, 1, 1)
    trendtype <- substr(model, 2, 2)
    seasontype <- substr(model, 3, 3)
    if (!is.element(errortype, c("M", "A", "Z"))) 
        stop("Invalid error type")
    if (!is.element(trendtype, c("N", "A", "M", "Z"))) 
        stop("Invalid trend type")
    if (!is.element(seasontype, c("N", "A", "M", "Z"))) 
        stop("Invalid season type")
    if (m < 1 | length(y) <= m) {
        seasontype <- "N"
    }
    if (m == 1) {
        if (seasontype == "A" | seasontype == "M") 
            stop("Nonseasonal data")
        else substr(model, 3, 3) <- seasontype <- "N"
    }
    if (m > 52) {
        if (is.element(seasontype, c("A", "M"))) 
            stop("Frequency too high")
        else if (seasontype == "Z") {
            warning("I can't handle data with frequency greater than 24. Seasonality will be ignored. Try stlf() if you need seasonal forecasts.")
            substr(model, 3, 3) <- seasontype <- "N"
        }
    }
    if (restrict) {
        if ((errortype == "A" & (trendtype == "M" | seasontype == 
                                     "M")) | (errortype == "M" & trendtype == "M" & seasontype == 
                                                  "A") | (additive.only & (errortype == "M" | trendtype == 
                                                                               "M" | seasontype == "M"))) 
            stop("Forbidden model combination")
    }
    data.positive <- (min(y) > 0)
    if (!data.positive & errortype == "M") 
        stop("Inappropriate model for data with negative or zero values")
    if (!is.null(damped)) {
        if (damped & trendtype == "N") 
            stop("Forbidden model combination")
    }
    n <- length(y)
    npars <- 2L
    if (trendtype == "A" | trendtype == "M") 
        npars <- npars + 2L
    if (seasontype == "A" | seasontype == "M") 
        npars <- npars + m
    if (!is.null(damped)) 
        npars <- npars + as.numeric(damped)
    if (n <= npars + 1) 
        stop("You've got to be joking. I need more data!")
    if (errortype == "Z") 
        errortype <- c("A", "M")
    if (trendtype == "Z") 
        trendtype <- c("N", "A", "M")
    if (seasontype == "Z") 
        seasontype <- c("N", "A", "M")
    if (is.null(damped)) 
        damped <- c(TRUE, FALSE)
    best.ic <- Inf
    for (i in 1:length(errortype)) {
        for (j in 1:length(trendtype)) {
            for (k in 1:length(seasontype)) {
                for (l in 1:length(damped)) {
                    if (trendtype[j] == "N" & damped[l]) 
                        next
                    if (restrict) {
                        if (errortype[i] == "A" & (trendtype[j] == 
                                                       "M" | seasontype[k] == "M")) 
                            next
                        if (errortype[i] == "M" & trendtype[j] == 
                                "M" & seasontype[k] == "A") 
                            next
                        if (additive.only & (errortype[i] == "M" | 
                                                 trendtype[j] == "M" | seasontype[k] == 
                                                 "M")) 
                            next
                    }
                    if (!data.positive & errortype[i] == "M") 
                        next
                    fit <- etsmodel(y, errortype[i], trendtype[j], 
                                    seasontype[k], damped[l], alpha, beta, gamma, 
                                    phi, lower = lower, upper = upper, opt.crit = opt.crit, 
                                    nmse = nmse, bounds = bounds, ...)
                    fit.ic <- switch(ic, aic = fit$aic, bic = fit$bic, 
                                     aicc = fit$aicc)
                    if (!is.na(fit.ic)) {
                        if (fit.ic < best.ic) {
                            model <- fit
                            best.ic <- fit.ic
                            best.e <- errortype[i]
                            best.t <- trendtype[j]
                            best.s <- seasontype[k]
                            best.d <- damped[l]
                        }
                    }
                }
            }
        }
    }
    if (best.ic == Inf) 
        stop("No model able to be fitted")
    model$m <- m
    model$method <- paste("ETS(", best.e, ",", best.t, ifelse(best.d, 
                                                              "d", ""), ",", best.s, ")", sep = "")
    model$components <- c(best.e, best.t, best.s, best.d)
    model$call <- match.call()
    model$initstate <- model$states[1, ]
    model$sigma2 <- mean(model$residuals^2, na.rm = TRUE)
    model$x <- orig.y
    model$lambda <- lambda
    if (!is.null(lambda)) {
        model$fitted <- InvBoxCox(model$fitted, lambda)
    }
    return(structure(model, class = "ets"))
}




f_ets.fit = function(y, fit.years = 4)
{
    y.fit = window(y,start=tsp(y)[1], end = tsp(y)[1] + fit.years - 0.0001)
    ets(y.fit)
}

# FORECAST


# ROLL
# need generic functions to handle time series of varying lengths now...
# this procedure is very much lined up for monthly forecasting at present
# 

f_ets.roll.fc.item = function(y, h.max, PRINT = FALSE) 
{    
    # rules around length of series (n), horizon (h), splitting rules
    m = tsp(y)[3]               # m is seasonal period
    n <- length(y)              # n is series length
    
    if (m == 12) k <- 48             # minimum data length for fitting a model
    if (m == 52) k <- 208             # minimum data length for fitting a model
    
    st <- tsp(y)[1]+(k-1)/m     # st = start period in ts terms
    
    yhist <- window(y, end = st)
    fit.original <- ets(yhist, model="ZZZ")  # model="MMM",
    
    # define the matrices for collecting the data
    err <- matrix(NA, n-k, h.max) 
    yhat <- matrix(NA, n-k, h.max)
    act <- matrix(NA, n-k, h.max)    
    
    for(i in 1:(n-k))
    {
        yhist <- window(y, end = st + (i-1)/m)
        yfuture <- window(y, start = st + i/m , end=st + (i+h.max-1)/m)
        
        # using ets presently
        fit <- ets(yhist, model=fit.original)  # model="MMM",
        
        # do the forecasts      
        fcast <- forecast(fit, h = min(n-k-i+1, h.max))
        
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


f_ets.run.item = function(ss, frequency, h.max)  #fc.item = "00-01-18200-53030", pth = NULL)
{    
    y = ts(ss$UNITS, start=c(2001,1), frequency = frequency)
    roll = f_ets.roll.fc.item(y, h.max = h.max)
    roll
}


# SPLIT/AGGREGATE



# EVALUATE
#   fit

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


#=================== PLOTTING ====================

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



f_summary.plots = function(Err) {
   
    ggplot(data=Err, aes(y=re, x = factor(k))) +geom_boxplot()
    ggplot(data=Err, aes(x=e, colour = factor(k))) + geom_density()+        theme_bw()
    ggplot(data=Err, aes(x=e)) + geom_histogram() +         facet_wrap(~k)
    
    Err[,list(n=length(y), me = mean(e), mae = mean(ae), mrae = mean(re)),by=list(k)]
}




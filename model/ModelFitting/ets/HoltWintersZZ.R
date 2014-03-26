
HoltWintersZZ <- function (x,
                           # smoothing parameters
                           alpha = NULL, # level
                           beta = NULL, # trend
                           gamma = NULL, # seasonal component
                           seasonal = c("additive", "multiplicative"),
                           exponential = FALSE, # exponential
                           phi = NULL # damp
)
{
    x <- as.ts(x)
    seasonal <- match.arg(seasonal)
    m <- frequency(x)
    lenx <- length(x)
    
    if(is.null(phi) || !is.numeric(phi))
        phi <- 1
    if(!is.null(alpha) && !is.numeric(alpha))
        stop ("cannot fit models without level ('alpha' must not be 0 or FALSE).")
    if(!all(is.null(c(alpha, beta, gamma))) &&
           any(c(alpha, beta, gamma) < 0 || c(alpha, beta, gamma) > 1))
        stop ("'alpha', 'beta' and 'gamma' must be within the unit interval.")
    if((is.null(gamma) || gamma > 0)) {
        if (seasonal == "multiplicative" && any(x <= 0))
            stop ("data must be positive for multiplicative Holt-Winters.")
    }
    
    if(m<=1)
        gamma <- FALSE
    
    ## initialise l0, b0, s0
    if(!is.null(gamma) && is.logical(gamma) && !gamma) {
        seasonal <- "none"
        l.start <- x[1L]
        s.start <- 0
        if(is.null(beta) || !is.logical(beta) || beta){
            if(!exponential)
                b.start <- x[2L] - x[1L]
            else
                b.start <- x[2L]/x[1L]
        }
    } else {
        ## seasonal Holt-Winters
        l.start <- mean(x[1:m])
        b.start <- (mean(x[m+(1:m)]) - l.start)/m
        if(seasonal=="additive")
            s.start <- x[1:m]-l.start
        else
            s.start <- x[1:m]/l.start
    }
    
    #initialise smoothing parameters
    #lower=c(rep(0.0001,3), 0.8)
    #upper=c(rep(0.9999,3),0.98)
    lower <- c(0,0,0,0)
    upper <- c(1,1,1,1)
    
    if(!is.null(beta) && is.logical(beta) && !beta)
        trendtype <- "N"
    else if(exponential)
        trendtype <- "M"
    else
        trendtype <- "A"
    
    if(seasonal=="none")
        seasontype <- "N"
    else if(seasonal=="multiplicative")
        seasontype <- "M"
    else
        seasontype <- "A"
    
    ## initialise smoothing parameter
    optim.start <- initparam(alpha = alpha, beta = beta, gamma=gamma, phi=1,
                             trendtype=trendtype, seasontype=seasontype, damped=FALSE, lower=lower, upper=upper, m=m)
    
    # if(!is.na(optim.start["alpha"]))
    # alpha2 <- optim.start["alpha"]
    # else
    # alpha2 <- alpha
    # if(!is.na(optim.start["beta"]))
    # beta2 <- optim.start["beta"]
    # else
    # beta2 <- beta
    # if(!is.na(optim.start["gamma"]))
    # gamma2 <- optim.start["gamma"]
    # else
    # gamma2 <- gamma
    
    # if(!check.param(alpha = alpha2,beta = beta2, gamma = gamma2,phi=1,lower,upper,bounds="haha",m=m))
    # {
    # print(paste("alpha=", alpha2, "beta=",beta2, "gamma=",gamma2))
    # stop("Parameters out of range")
    # }
    
    ###################################################################################
    #optimisation: alpha, beta, gamma, if any of them is null, then optimise them
    error <- function (p, select)
    {
        if(select[1]>0)
            alpha <- p[1L]
        if(select[2]>0)
            beta <- p[1L+select[1]]
        if(select[3]>0)
            gamma <- p[1L+select[1]+select[2]]
        
        zzhw(x,lenx=lenx, alpha = alpha, beta=beta, gamma=gamma, seasonal=seasonal, m=m,
             dotrend=(!is.logical(beta) || beta), doseasonal=(!is.logical(gamma) || gamma),
             exponential=exponential, phi=phi, l.start=l.start, b.start=b.start, s.start=s.start)$SSE
    }
    select <- as.numeric(c(is.null(alpha),is.null(beta),is.null(gamma)))
    
    if(sum(select)>0) # There are parameters to optimize
    {
        sol <- optim(optim.start, error, method = "L-BFGS-B", lower = lower[select], upper = upper[select], select=select)
        if(sol$convergence || any(sol$par < 0 | sol$par > 1)) {
            if (sol$convergence > 50) {
                warning(gettextf("optimization difficulties: %s", sol$message), domain = NA)
            } else stop("optimization failure")
        }
        if(select[1]>0)
            alpha <- sol$p[1L]
        if(select[2]>0)
            beta <- sol$p[1L+select[1]]
        if(select[3]>0)
            gamma <- sol$p[1L+select[1]+select[2]]
    }
    
    final.fit <- zzhw(x, lenx=lenx, alpha = alpha, beta=beta, gamma=gamma, seasonal=seasonal, m=m,
                      dotrend=(!is.logical(beta) || beta), doseasonal=(!is.logical(gamma) || gamma),
                      exponential=exponential, phi=phi, l.start=l.start, b.start=b.start, s.start=s.start)
    
    tspx <- tsp(x)
    fitted <- ts(final.fit$fitted,frequency=m,start=tspx[1])
    states <- matrix(final.fit$level,ncol=1)
    colnames(states) <- "l"
    if(trendtype!="N")
        states <- cbind(states,b=final.fit$trend)
    if(seasontype!="N")
    {
        nr <- nrow(states)
        nc <- ncol(states)
        for(i in 1:m)
            states <- cbind(states,final.fit$season[(m-i)+(1:nr)])
        colnames(states)[nc+(1:m)] <- paste("s",1:m,sep="")
    }
    states <- ts(states,frequency=m,start=tspx[1]-1/m)
    
    # Package output as HoltWinters class
    # structure(list(fitted = fitted,
    # x = x,
    # alpha = alpha,
    # beta = beta,
    # gamma = gamma,
    # coefficients = c(a = final.fit$level[lenx],
    # b = if (!is.logical(beta) || beta) final.fit$trend[lenx],
    # s = if (!is.logical(gamma) || gamma) final.fit$season[lenx - m + 1L:m]),
    # seasonal = seasonal,
    # exponential = exponential,
    # SSE = final.fit$SSE,
    # call = match.call(),
    # level = final.fit$level,
    # trend = final.fit$trend,
    # season = final.fit$season,
    # phi = phi
    # ),
    # class = "HoltWinters"
    # )
    # Package output as ets class
    damped <- (phi<1.0)
    if(seasonal=="additive") # This should not happen
        components <- c("A",trendtype,seasontype,damped)
    else if(seasonal=="multiplicative")
        components <- c("M",trendtype,seasontype, damped)
    else if(seasonal=="none" & exponential)
        components <- c("M",trendtype,seasontype,damped)
    else# if(seasonal=="none" & !exponential)
        components <- c("A",trendtype,seasontype, damped)
    
    initstate <- states[1,]
    param <- alpha
    names(param) <- "alpha"
    if(trendtype!="N")
    {
        param <- c(param,beta=beta)
        names(param)[length(param)] <- "beta"
    }
    if(seasontype!="N")
    {
        param <- c(param,gamma=gamma)
        names(param)[length(param)] <- "gamma"
    }
    if(damped)
    {
        param <- c(param,phi=phi)
        names(param)[length(param)] <- "phi"
    }
    
    if(components[1]=="A")
        sigma2 <- mean(final.fit$residuals^2)
    else
        sigma2 <- mean((final.fit$residuals/fitted)^2)
    structure(list(fitted = fitted,
                   residuals=final.fit$residuals,
                   components=components,
                   x = x,
                   par=c(param,initstate),
                   initstate=initstate,
                   states=states,
                   SSE = final.fit$SSE,
                   sigma2 = sigma2,
                   call = match.call(),
                   m = m
    ),
    class = "ets"
    )
}

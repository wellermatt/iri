##  FUNCTIONS RELATED TO AGGREGATING/DISAGGREGATING TIME SERIES VARIABLES

setwd(pth.dropbox.code) ; source("./data/DataAdaptor/00_data_adaptor_test.R")
f_load.calendar()


f_weekly.split.matrix = function(o, h) {
  
  # how to handle 6-week month??
    library(foreach)
    
    # requires the origin o to be at the end of a 445 period
    # data by 445 period starting at week following o for n periods        
    cal.periods = calendar.445$weeks.in.period[(o+1):(o+h)]
    
    tm = matrix(0, length(cal.periods),sum(cal.periods))
    
    foreach (pd = 1:length(cal.periods)) %do%
    {
        end = sum(cal.periods[1:pd])
        start = end - cal.periods[pd] + 1
        tm[pd, start:end] = 1/cal.periods[pd]
    }
    
    # return value is the transition matrix from this point in time
    # in future may be able to generate this only once and re-use it
    tm
}

f_item_split = function(yhat, tm = NULL,melt.results = FALSE)
{
  o.count = nrow(yhat)
  o.start = 48
  h.max = ncol(yhat)
  
  # foreach origin - this is the main loop through the origins to split 445 -> weeks
  weekly.fcast = foreach (o = 1:o.count) %do%    # .combine = rbind
  {
    
    h = min(h.max, o.count - o + 1)
    tm = f_weekly.split.matrix(o = o.start + o - 1, h = h)    
    yhat[o, 1:h] %*% tm
  }
  #weekly.fcast
  if (melt.results == TRUE) {
    
    wf = rbindlist(lapply(1:o.count, function(i) {
      dt = data.table(o = i, melt(weekly.fcast[[i]], varnames=c("t","k"), value.name=="yhat"))
      setnames(dt,"value", "yhat")
      dt$t = dt$o
      dt$o = NULL
      dt
    }))
    wf
  }
  else weekly.fcast
      
}
# need some functions to test with real data, rolling origin, many series (ets results)
# parameters:
#  445 dataset so forecasts for k-steps ahead to horizon h
#  calendar445 -> weekly
#  
#  


# input is a matrix of rolling origin forecasts
# rows = origins (t), columns = k-steps ahead
# deficiency with current input foormat - no need to calulate all the errors in the forecast data
# at this point, needs change to the ets simulation run code
library(reshape2) ; library(data.table)

TEST = TRUE

if (TEST == TRUE)
{
  #  testing split from 445 to weekly
  
  ## TESTING
  setwd(pth.dropbox.data)
  errs = readRDS("./output/errors/errors_ets.rds")
  head(errs)
  yhat = data.table(dcast(errs,formula=fc.item+t~k,fun.aggregate=mean,value.var="yhat"))
  yhat.test = as.matrix(yhat[fc.item =="00-01-18200-53030",-1:-2, with = FALSE])
  
  yhat = yhat.test
  
  f_item_split(yhat,melt.results = FALSE)
  f_item_split(yhat,melt.results = TRUE)
  
  #identical(as.numeric(check.445), as.numeric(x445))
  
}

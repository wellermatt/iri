# ** URGENT: remove all references to 313, should now be dynamic but at least change to 312


##  FUNCTIONS RELATED TO AGGREGATING/DISAGGREGATING TIME SERIES VARIABLES

#source("c:/Users/welle_000/My Documents/GitHub/iri/.Rprofile")
#setwd(pth.dropbox.code) ; source("./data/DataAdaptor/00_data_adaptor_test.R")
#f_load.calendar()

library(foreach)

f_full.tm = function(o, h)
{
  tm = matrix(0, 72, 312)
  f_weekly.split.matrix(0,h)
  
}
#tm.full = f_weekly.split.matrix(o=1,h=72, inverse = FALSE)
#tm.test = f_weekly.split.matrix(o=71,h=1, inverse = TRUE)
#tm = f_weekly.split.matrix(1,72, inverse=TRUE)

f_weekly.split.matrix = function(o = 0, h=72, inverse = FALSE) 
  
  # this will generate a split matrix from 445 periods to weeks from a certain start point and for a given horizon
  #  in effect this is the weightings matrix between weeks and months fo this calendar set up  
  {
  
  # how to handle 6-week month??
    library(foreach)
    
    # requires the origin o to be at the end of a 445 period
    # data by 445 period starting at week following o for n periods        
    cal.periods = calendar.445$weeks.in.period[(o+1):(o+h)]
    
    tm = matrix(0, length(cal.periods), sum(cal.periods))
    
    foreach (pd = 1:length(cal.periods)) %do%
    {
        end = sum(cal.periods[1:pd])
        start = end - cal.periods[pd] + 1
        tm[pd, start:end] = 1/cal.periods[pd]
    }
    if (ncol(tm) == 313) tm = tm[, 1:312]
    # return value is the transition matrix from this point in time
    # in future may be able to generate this only once and re-use it
    if (inverse == TRUE) t(tm>0) * 1 else tm
}


fcast.object = function()
{
  # fcast can be an object (list) with various slots for numbers, the periodicity 
  mx.time =  f_weekly.split.matrix()  
  apply(mx.time,1,function(x) sum(x>0))

  
}


f_agg.fcast = function(fcast) 
  
  # receive weekly forecasts made at monthly intervals and convert to monthly forecasts with actuals and error measures
  #  in effect this is the weightings matrix between weeks and months fo this calendar set up  
{
  # fcast.weekly = 
  
}


f_item_split = function(yhat.445, actuals, tm = NULL, melt.results = FALSE)
  
  # function to split the rolling forecasts for an item from 445 to weekly
  # calsulates error measures as required on a per line basis  
  {
  # parameters
  # yhat.445  matrix of multi-origin, multi-step forecasts 
  # actuals   vector of y values representing weekly observations
  # tm        optional matrix of transformations period -> week
  #
  o.count = nrow(yhat.445)
  o.start = 48
  h.max = ncol(yhat.445)
  
  # foreach origin - this is the main loop through the origins to split 445 -> weeks
  weekly.fcast = foreach (o = 1:o.count) %do%    # .combine = rbind
  {
    
    h = min(h.max, o.count - o + 1)
    tm = f_weekly.split.matrix(o = o.start + o - 1, h = h)    
    yhat.445[o, 1:h] %*% tm
  }
  
  # prepare output data from weekly.fcast
  if (melt.results == TRUE) {
    
    end.weeks = calendar.445.slim$end.week
    wf = rbindlist(lapply(1:o.count, function(i) {
      dt = data.table(o = i, melt(weekly.fcast[[i]], varnames=c("t","k"), value.name=="fc"))
      setnames(dt,"value", "fc")
      dt$t = dt$o
      dt$o = NULL
      dt[,`:=`(origin = o.start + t -1, origin.week = end.weeks[o.start + t -1])]
      dt[,`:=`(fc.week = origin.week + k)]
      dt[,act:=actuals[fc.week]]
    }))
    wf[,`:=`(e = fc-act, ae=abs(fc-act), 
             re = (fc-act)/act, 
             rae = abs(fc-act)/act,
             srae = abs(fc-act)/(0.5 * (act + fc)))]   
  
    
  }   else weekly.fcast
      
}

f_get_actuals = function(this.fc.item = "00-01-18200-53030", periodicity = "weekly")
  
  # will use the dataAdaptor functionality to get weekly/445 actuals for a fc.item  
  {
  if (periodicity=="monthly")  periodicity="445"
  sp = f_da.reg.cat.test(par.category="beer", par.periodicity=periodicity)   # spw is the regression dataset, all nodes
  ss = sp[fc.item == this.fc.item, UNITS, with = TRUE]
  ss
}

f_item_aggregate_temporal = function(yhat.weekly, actuals, tm = NULL, melt.results = TRUE)

  # function to perform temporal aggregation on the rolling origin multi-step weekly forecasts
  # currently missing  the last single month forecast and forecast at
  {
    # need a set of yhat weekly
    o.count = nrow(yhat.weekly)
    o.start = 48
    h.max = ncol(yhat.weekly)
    
    # foreach origin - this is the main loop through the origins to split 445 -> weeks
    fcast.445 = foreach (o = 1:(o.count)) %do%    # .combine = rbind
    {
      #print(o)
      h = min(h.max, o.count - o + 1)
      tm = f_weekly.split.matrix(o = o.start + o - 1, h = min(h,3), inverse = TRUE)
      tm = tm[1:min(nrow(tm),13),] 
      if (is.null(nrow(tm))) fc.week.count = length(tm) else fc.week.count = nrow(tm)
      yhat.weekly[o, 1:fc.week.count]  %*%  tm
    }   
    
  
  # prepare output data from fcast.445
  if (melt.results == TRUE) {
    
    end.weeks = calendar.445.slim$end.week
    wf = rbindlist(lapply(1:o.count, function(i) {
      dt = data.table(o = i, melt(fcast.445[[i]], varnames=c("t","k"), value.name=="fc"))
      setnames(dt,"value", "fc")
      dt$t = dt$o
      dt$o = NULL
      dt[,`:=`(origin = o.start + t -1, fc.period = o.start + t -1 + k)]
      #dt[,`:=`(fc.week = origin.week + k)]
      dt[,act:=actuals[fc.period]]
    }))
    wf[,`:=`(e = fc-act, ae=abs(fc-act), 
             re = (fc-act)/act, 
             rae = abs(fc-act)/act,
             srae = abs(fc-act)/(0.5 * (act + fc)))]   
    
    
  }   else fcast.445

  
}


# input is a matrix of rolling origin forecasts
# rows = origins (t), columns = k-steps ahead
# deficiency with current input foormat - no need to calulate all the errors in the forecast data
# at this point, needs change to the ets simulation run code

#stop()

###========================================================

library(reshape2) ; library(data.table)

TEST = FALSE

if (TEST == TRUE)
{
  #  testing split from 445 to weekly
  
  ## TESTING
  # get a multi-item dataset
  setwd(pth.dropbox.data)
  fcast.445 = readRDS("./output/errors/ets_445.rds")
  
  head(fcast.445,20)
  yhat.all = data.table(dcast(fcast.445,
                          formula = fc.item + t ~ k,
                          fun.aggregate=mean, value.var="yhat"))
  
  calendar.445.slim =  calendar.445[,list(period_id,weeks.in.period,elapsed_weeks, start_date,end_date)][1:72]
  calendar.445.slim[,`:=`(start.week = elapsed_weeks-weeks.in.period + 1,end.week = elapsed_weeks)]
  calendar.445.slim
  
  
  library(stringr)
  
  items = unique(yhat.all$fc.item)  
  fcast.weekly = foreach(this.fc.item = items) %do%
  {
    # main loop to split for an item and generate a weekly set of forecasts
    
    yhat.445 = as.matrix(yhat.all[fc.item == this.fc.item,-1:-2, with = FALSE])  
    actuals = f_get_actuals(this.fc.item =this.fc.item, periodicity="weekly")  
    
    fcast.weekly = f_item_split(yhat.445, actuals, melt.results = TRUE)
    fcast.weekly$fc.item = this.fc.item
    fcast.weekly
  }
  #acc.summary.week[,lvl := str_count( fc.item, "/") + 1 ]
  
  setwd(pth.dropbox.data)
  saveRDS(rbindlist(fcast.weekly), "./output/errors/ets_445_weekly.rds")
  
  #identical(as.numeric(check.445), as.numeric(x445))
  
}


TEST=FALSE

library(reshape2)

if (TEST == TRUE)
{
  #  testing aggregating from weekly to 445
  
  ## TESTING
  # get a multi-item dataset
  setwd(pth.dropbox.data)
  fcast.weekly = readRDS("./output/errors/errors_reg2.rds")[t>207]
  
  head(fcast.weekly,20)
  yhat.all = data.table(dcast(fcast.weekly,
                              formula = fc.item + t ~ k,
                              fun.aggregate=sum, value.var="fc"))
  
  
  calendar.445.slim =  calendar.445[,list(period_id,weeks.in.period,elapsed_weeks, start_date,end_date)][1:72]
  calendar.445.slim[,`:=`(start.week = elapsed_weeks-weeks.in.period + 1,end.week = elapsed_weeks)]
  calendar.445.slim
  
  # limit the yhat data to only the rows (origins) which correspond to the end of a 445 period
  yhat.all = yhat.all[t %in% calendar.445.slim$end.week]
  
  library(stringr)
  
  items = unique(yhat.all$fc.item)  
  fcast.445 = foreach(this.fc.item = items) %do%
  {
    # main loop to split for an item and generate a weekly set of forecasts
    print(this.fc.item)
    yhat.weekly = as.matrix(yhat.all[fc.item == this.fc.item,-1:-2, with = FALSE])  
    actuals = f_get_actuals(this.fc.item = this.fc.item, periodicity="445")  
    
    fcast.445 = f_item_aggregate_temporal  (yhat.weekly, actuals, melt.results = TRUE)
    fcast.445$fc.item = this.fc.item
    fcast.445
  }
  #acc.summary.week[,lvl := str_count( fc.item, "/") + 1 ]
  
  setwd(pth.dropbox.data)
  saveRDS(rbindlist(fcast.445), "./output/errors/regression_weekly_445.rds")
  
  #identical(as.numeric(check.445), as.numeric(x445))

}


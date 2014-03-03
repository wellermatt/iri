##  FUNCTIONS RELATED TO AGGREGATING/DISAGGREGATING TIME SERIES VARIABLES

source("c:/Users/welle_000/My Documents/GitHub/iri/.Rprofile")
setwd(pth.dropbox.code) ; source("./data/DataAdaptor/00_data_adaptor_test.R")
f_load.calendar()

library(foreach)

f_weekly.split.matrix = function(o, h) 
  
  # this will generate a split matrix from 445 periods to weeks from a certain start point and for a given horizon
  
  {
  
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

f_get_weekly_actuals = function(this.fc.item = "00-01-18200-53030")
  
  # will use the dataAdaptor functionality to get weekly actuals for a fc.item
  
  {
  
  spw = f_da.reg.cat.test(par.category="beer", par.periodicity="weekly")   # spw is the regression dataset, all nodes
  ssw = spw[fc.item == this.fc.item, UNITS, with = TRUE]
  ssw
}




# input is a matrix of rolling origin forecasts
# rows = origins (t), columns = k-steps ahead
# deficiency with current input foormat - no need to calulate all the errors in the forecast data
# at this point, needs change to the ets simulation run code



###========================================================

library(reshape2) ; library(data.table)

TEST = TRUE

if (TEST == TRUE)
{
  #  testing split from 445 to weekly
  
  ## TESTING
  # get a multi-item dataset
  setwd(pth.dropbox.data)
  fcast.445 = readRDS("./output/errors/errors_ets.rds")
  head(fcast.445,20)
  yhat.all = data.table(dcast(fcast.445,
                          formula = fc.item + t ~ k,
                          fun.aggregate=mean, value.var="yhat"))
  
  calendar.445.slim =  calendar.445[,list(period_id,weeks.in.period,elapsed_weeks, start_date,end_date)][1:72]
  calendar.445.slim[,`:=`(start.week = elapsed_weeks-weeks.in.period + 1,end.week = elapsed_weeks)]
  calendar.445.slim
  
  
  items = unique(yhat.all$fc.item)
  
  acc.summary.week = foreach(this.fc.item = items) %do%
  {
    library(stringr)
    # main loop to split for an item and generate a weekly set of forecasts
    
    yhat.445 = as.matrix(yhat.all[fc.item == this.fc.item,-1:-2, with = FALSE])  
    actuals = f_get_weekly_actuals(this.fc.item =this.fc.item)  
    fcast.weekly = f_item_split(yhat.445, actuals, melt.results = TRUE)
    
    acc.summary.week = cbind(dcast(data=fcast.weekly, formula=k~., fun.aggregate=median,value.var="rae"),
                             dcast(data=fcast.weekly, formula=k~., fun.aggregate=mean,value.var="rae")[-1])
    
    names(acc.summary.week)[-1] = c("mdrae","mrae")
    acc.summary.week$fc.item = this.fc.item
        
    if (TRUE == FALSE) 
    {
      library(ggplot2)
      p = ggplot(data = fcast.weekly, aes(x=rae)) + geom_histogram() + ggtitle(this.fc.item)  ; print(p)
      p = ggplot(data = fcast.weekly, aes(x = as.factor(k), y = rae)) + geom_boxplot() + ggtitle(this.fc.item) ; print (p)
      
      fcast.weekly[,list(mdrae = median(rae), mrae = mean(rae)),by="k"]
      
      with(fcast.weekly,boxplot(rae~k))
    } 
    
    
    acc.summary.week
  }
  acc.summary.week[,lvl := str_count( fc.item, "/") + 1 ]
  
  setwd(pth.dropbox.data)
  saveRDS(acc.summary.week, "./output/errors/errors_ets_weekly.rds")
  
  #identical(as.numeric(check.445), as.numeric(x445))
  
}




acc.summary.week = rbindlist(readRDS( "./output/errors/errors_ets_weekly.rds"))
acc.summary.week

library(ggplot2)
qplot(data=acc.summary.week[k!=14],x=factor(k),y=mdrae, geom = "boxplot") + coord_flip() + facet_wrap(~lvl,ncol=1)

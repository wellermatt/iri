# this file contains a range of functions related to the calendar and the aggregation and disaggregation 
# of time series through the temporal hierarchy of days, weeks, months

#

library("data.table") ; library("zoo") ; library("reshape2")
library("dummies") ; library("TimeProjection")

machine = (Sys.info()["nodename"])

pth.dropbox = "/home/users/wellerm/"
if (machine == "M11") pth.dropbox = "C:/Users/Matt/Dropbox/"
if (machine == "DESKTOP") pth.dropbox = "D:/Dropbox/Dropbox/"
if (machine == "IDEA-PC") pth.dropbox = "C:/Users/welle_000/Dropbox/"

pth.dropbox.data = paste(pth.dropbox, "HEC/IRI_DATA/", sep = "")
pth.dropbox.code = paste(pth.dropbox, "HEC/Code/exp1.1/", sep = "")
if (pth.dropbox == "/home/users/wellerm/") {
	pth.dropbox.data = paste(pth.dropbox, "IRI_DATA/", sep = "")
	pth.dropbox.code = paste(pth.dropbox, "projects/exp1.1/", sep = "")
}



# build the daily calendar, it needs the holidays added

hols.seq = c("NYE","Superbowl","Presidents","Easter","Memorial","Independence","Labor","Halloween","Thanksgiving","Xmas")
#hols.seq = paste("hol_", hols.seq, sep="")

f_cal.holidays.us.get = function()
{
	# function to read the holidays from file and store it in long format
	# assumes the working directory is already set to pth.dropbox.data	
	setwd(pth.dropbox.data)
	holidays.us = read.csv(file="./iri reference data/calendar/US Holidays.csv",header=TRUE,stringsAsFactors=FALSE)
	
	holidays.us[,1]=NULL	# the input file contains a long namme for the holiday which we don't need
	holidays.us[,2]=NULL	# not interested in holidays from 2000

	holidays.us$Hol_Short = factor(holidays.us$Hol_Short)
  
	hols.melt = data.table(melt(holidays.us, variable.name = "cal.year", value.name = "cal.date", id="Hol_Short"))
	hols.melt[, cal.date:= as.Date(cal.date,format = "%d-%b-%y" )]
	hols.melt[, cal.year := as.integer(sub("X", "", cal.year))]

    hols.melt[,list(Hol_Short,cal.date)]
}


f_calendar.daily_build = function(opt.holiday.dummies = TRUE, opt.date.attributes = FALSE)
{
	start.date = as.Date("2001-01-01")
	end.date = as.Date("2006-12-31")
	num.weeks = (end.date - start.date + 1)/7

	calendar.daily = data.table(
		cal.date = as.Date(seq(from = start.date, to = end.date, by = "day")), 
		WEEK = as.integer(rep(1:num.weeks,each=7)), key = "cal.date")
	
	
	if (opt.holiday.dummies == TRUE) {
		# now get the holidays and add them in as dummies
		hols.melt = f_cal.holidays.us.get()
		setnames(hols.melt,1, "hol_")
		hols.melt = data.table(dummy.data.frame(hols.melt), key = "cal.date")
		calendar.daily = merge(calendar.daily, hols.melt, all.x=TRUE)
		for (j in names(calendar.daily)) set(calendar.daily,which(is.na(calendar.daily[[j]])),j,0L)
	}
	if (opt.date.attributes == TRUE) {
		calendar.daily = cbind(calendar.daily, projectDate(calendar.daily$cal.date))
	}
	
	return(calendar.daily)
}
# TEST 
f_calendar.daily_build()


num.weeks = 10

dat.weekly = data.table(
	week.ending = seq(from = as.Date("2001-1-7"), by = "week", length.out = num.weeks),
	n1 = rnorm(num.weeks, 100,10), 
	n2 = rnorm(num.weeks, 100, 10), 
	d3 = sample(c(0,0,0,1),num.weeks,replace=TRUE)) 
dat.weekly

day.weights = matrix(c(rep(1/7,7),rep(1/7,7),rep(1L,7)),nrow=7)
day.weights

library("foreach")

# use foreach to apply the daily weights to split out the weekly values to days
# some weights have the effect of keeping the dummy identical across all 7 days
# other weights have the effect of splitting a sum (e.g. total revenue) over 7 days
dat.daily = foreach (i = 2:4, .combine = "cbind") %do%
{
	as.vector(outer(
		day.weights[,i-1],
		unlist(dat.weekly[,c(i),with=FALSE])))
}

dates = do.call("c", lapply(dat.weekly$week.ending,function(x)seq(as.Date(x)-6,length.out=7, by="day")))
dat.daily = data.table(dates, dat.daily)
setnames(dat.daily, c("cal.date",names(dat.weekly)[-1]))

dat.daily






y = ssw[fc.item=="00-01-18200-53030",UNITS]

plot(y)


f_ts.agg.weeks.to.days = function(y)
{
	# this assumes that y is a vector representing a weekly time series of 313 periods in length
	if (length(y) != 313)  return ("series must be 313 periods in length")
	z.weekly <- zoo(y/7, order.by=as.Date(calendar.weekly$week_end_date)[1:313]-6)
	z.daily = merge(
		z.weekly, 
		foo=zooreg(NA, order.by=seq(start(z.weekly), end(z.weekly)+6,"day")))[,1]
	z.daily = na.locf(z.daily, method = "constant", na.rm = FALSE)
	z.daily
}

f_ts.agg.days.to.months = function(z.daily)
{
	z.monthly = aggregate(z.daily, as.yearmon, sum, na.rm=TRUE, regular = TRUE)
	z.monthly
}

f_ts.agg.weeks.to.months = function(y)
{
	z.daily = f_ts.agg.weeks.to.days(y)
	z.monthly = f_ts.agg.days.to.months(z.daily)
	z.monthly	
}

ssw

y.mon = f_ts.agg.weeks.to.months(y)
library("forecast") ; library("tseries")

seasonplot(ts(y.mon,frequency=12),col=rainbow(6),year.labels=TRUE,xlim=c(0,16000))
monthplot(ts(y.mon,frequency=12))
plot(stl(y.mon,s.window="periodic"))
stl(y.mon,s.window="periodic")
adf.test(as.numeric(y.mon))
ets(y.mon)
tbats(ts(y.mon,frequency=12))

# load the weekly calendar
setwd(pth.dropbox.data)
calendar.iri = readRDS("./iri reference data/calendar/calendar.weekly.rds")







names(calendar.iri)



if (TRUE == FALSE) {
	
# testing some of the functions
library(TimeProjection)
dates = timeSequence(from = '2012-01-01', to = '2012-12-31', by = 'day')
plotCalendarHeatmap(as.Date(dates), 1:366)

dates = timeSequence(from = "2001-01-01", to = "2006-12-31", by = "day")
projectDate(as.Date(dates))

}








#============================================ OLD STUFF=====================================


## old functions used to create the calendars initially

require(xts) ; require(reshape2) 
library(data.table) ; library(ggplot2)
library(plyr)

f_calendar.weekly.holidays = function(hols.us)
{	
	dt = as.numeric(-difftime("2001-01-01", hols.us$cal.date))

	hols.us$week_decimal =  dt/7
	hols.us$week_number =  as.integer(hols.us$week_decimal) + 1
	hols.us$iri_week_number = as.factor(hols.us$week_number)
	hols.us$week_day_number =  7*((dt/7)%%1) + 1
	hols.us$week_day = weekdays(hols.us$cal.date)
	hols.us
}

#f_calendar.weekly.holidays(f_calendar.holidays.us.get())




f_calendar.holidays.prepare = function(calendar, opt.set.dummies.binary = TRUE)
{
  calendar$Hols_any_weekday = rowSums(calendar[,c(grep("hol_",names(calendar))),with=FALSE])
  calendar$Hols_any_binary = abs((calendar$Hols_any_weekday >0))
  #calendar$Hols_any_binary>0
  #calendar$Hols_any_binary =calendar$Hols_any_binary    #factor(calendar$Hols_any_binary)
  #calendar
  if (opt.set.dummies.binary == TRUE) {
    mysubset <- c(grep("hol_", names(calendar)))
    calendar[, (mysubset) := lapply(.SD, function(x) as.integer(as.logical(x))), .SDcols = mysubset]  
  }
  
  calendar$Hols_name = names(calendar)[max.col(calendar[,c(grep("hol_",names(calendar))),with=FALSE],ties.method="first")+5]
  calendar$Hols_name = gsub(pattern="hol_",replacement="",calendar$Hols_name)
  calendar$Hols_name[calendar$Hols_any_binary==0]= NA
  calendar$Hols_name = factor(calendar$Hols_name, levels = hols.seq)
  calendar
}

f_calendar.build.weekly = function(opt.set.dummies.binary = TRUE, opt.lead.lag.1 = FALSE)
{
  # load the base calendar from an Excel file and add the holiday information as dummies
  # additionally add the Hols_any, Hols_any_weekday and Hols_any_binary fields
  
  # build an initial calendar weekly with holidays
  iri.calendar = data.table(read.csv(file=paste(pth,"HEC/IRI_DATA/iri reference data/IRI Calendar.csv",sep=""),header=TRUE))
  iri.calendar$PERIOD = (iri.calendar$Calendar_Year * 100 + iri.calendar$Calendar_Period)
  holidays.us = f_calendar.holidays.us.get()
    
  calendar.weekly = merge(iri.calendar[,c("week_number","Calendar_Year","Calendar_Week"),with=FALSE],
                      holidays.us, by = "week_number")
  calendar.weekly.cast = cast(data=calendar.weekly, formula=week_number~Hol_Short,
                             value="week_day_number")
  calendar.weekly = merge(x=iri.calendar, y=calendar.weekly.cast,by="week_number",all.x=TRUE)
  calendar.weekly[is.na(calendar.weekly)] <- 0
  hols = calendar.weekly
  setnames(hols,1,"WEEK")
  #names(hols)[1] = "WEEK"
  hols.slim = hols[,c(1,4,6:9,11:20),with=FALSE]
  setnames(hols.slim,7:length(hols.slim), paste("hol_", names(hols.slim)[7:length(hols.slim)], sep=""))
  calendar.weekly = as.data.table(hols.slim)
  setkeyv(calendar.weekly,"WEEK")
  calendar.weekly = f_calendar.holidays.prepare(calendar.weekly, opt.set.dummies.binary)
  if (opt.lead.lag.1 == TRUE) calendar.weekly = f_calendar.lag.lead(calendar.weekly)
  calendar.weekly
}



# calendar.weekly = f_calendar.build.weekly(opt.set.dummies.binary=TRUE,opt.lead.lag.1=TRUE)
# calendar.445 = f_calendar.445(opt.lead.lag.1=TRUE)
# saveRDS(calendar.weekly)

f_calendar.445 = function(opt.lead.lag.1=FALSE)
{
  cal = f_calendar.build.weekly(opt.set.dummies.binary=TRUE, opt.lead.lag.1 = opt.lead.lag.1)
  mysubset <- names(cal)[c(grep("hol_", names(cal)))]  
  calendar.445 = cal[, lapply(.SD, sum), by=list(period_id), .SDcols=mysubset]
  
  weeks.in.period = cal[, list(weeks.in.period = max(WEEK)-min(WEEK)+1), by=list(period_id)]
  calendar.445 = merge(weeks.in.period, calendar.445)
  #calendar.445 = data.table(period.id = 1:nrow(calendar.445), calendar.445)
  
  #calendar.week.445.map = merge(calendar.445[,list(period, period.id)],
  #                              cal,
  #                              by="period")
  #list(calendar.445 = calendar.445, calendar.week.445.map = calendar.week.445.map)

  calendar.445
}



f_save.calendars = function() {
  saveRDS(f_calendar.build.weekly(),file= "D:/Dropbox/Dropbox/HEC/IRI_DATA/iri reference data/calendar/calendar.weekly.rds")
  saveRDS(f_calendar.build.weekly(opt.lead.lag.1=TRUE),file= "D:/Dropbox/Dropbox/HEC/IRI_DATA/iri reference data/calendar/calendar.weekly.lead.lag.rds")
  saveRDS(f_calendar.445(opt.lead.lag.1=TRUE),file= "D:/Dropbox/Dropbox/HEC/IRI_DATA/iri reference data/calendar/calendar.445.lead.lag.rds")
}

f_save.calendars()



f_calendar.lag.lead = function(calendar)
{
  names(calendar)
  mysubset <- c(grep("hol_", names(calendar)))
  calendar[, paste(names(calendar)[mysubset],"LEAD1",sep="_") := lapply(.SD, function(x) c(x[2:length(x)],NA)),
           .SDcols = mysubset]  
  calendar[, paste(names(calendar)[mysubset],"LAG1",sep="_") := lapply(.SD, function(x) c(NA, x[1:(length(x)-1)])),    #as.integer(lag(x,-1,na.pad=TRUE
           .SDcols = mysubset] 
  
  # assuming WEEKLY data need to reset LAG values from NA to 0/1 in WEEK 1
  mysubset <- grep("_LAG1",names(calendar))
  calendar[c(1), (mysubset) := lapply(.SD, function(x) x = 0), .SDcols = mysubset]  
  mysubset = mysubset[c(6,10)]
  calendar[c(1), (mysubset) := lapply(.SD, function(x) x = 1), .SDcols =  mysubset]  
  
  calendar
}




f_calendar.lag.lead.nl = function(calendar)
{
  names(calendar)
  mysubset <- c(grep("hol_", names(calendar)))
  calendar[, (mysubset) := lapply(.SD, function(x) as.integer(as.logical(x))), .SDcols = mysubset]
  calendar[, paste(names(calendar)[mysubset],"LEAD1",sep="_") := lapply(.SD, function(x) c(x[2:length(x)],NA)),
           .SDcols = mysubset]  
  calendar[, paste(names(calendar)[mysubset],"LAG1",sep="_") := lapply(.SD, function(x) c(NA, x[1:(length(x)-1)])),    #as.integer(lag(x,-1,na.pad=TRUE
           .SDcols = mysubset] 
}
#f_calendar.lag.lead(calendar)



##########


# calendar functions
f_calendar.daily.get = function()
{
  iri.calendar = read.csv(file=paste(pth,"HEC/Data/iri reference data/IRI Calendar.csv",sep=""),header=TRUE)
  calendar.daily = ddply(iri.calendar,.variable = c("week_number"),
                   .fun=function(x)
                     data.frame(dates = seq(from=as.Date(x$week_start_date),to=as.Date(x$week_end_date), by = "days"), 
                                week_number = x$week_number))
  calendar.daily$MONTH = month(calendar.daily$dates)
  calendar.daily$YEAR = year(calendar.daily$dates)
  day.weights = rep(1/7,7)
  calendar.daily$day.weight = 1/7
  calendar.daily$doy = as.integer(strftime(calendar.daily$dates, format = "%j"))
  calendar.daily$dom = as.POSIXlt(calendar.daily$dates)$mday 
  calendar.daily$dow = as.POSIXlt(calendar.daily$dates)$wday 
  calendar.daily$dow[calendar.daily$dow==0] = 7
  
  names(calendar.daily)[2] = "WEEK"
  calendar.daily = data.table(calendar.daily,key="dates")
  
  holidays.us = f_calendar.holidays.us.get()  
  names(holidays.us)[2] = "dates"  
  holidays.us = data.table(holidays.us,key="dates")[,c(1:2),with=FALSE]
  calendar.daily = merge(calendar.daily,holidays.us, by="dates", all.x=TRUE)
  calendar.daily$Hols_any = !is.na(calendar.daily$Hol_Short)
  names(calendar.daily)  
  
  calendar.daily$Hol_Short = factor(calendar.daily$Hol_Short, levels = hols.seq)
  dummies = cast(data=calendar.daily,dates~Hol_Short,sum,value="Hols_any")
  names(dummies)
  dummies = dummies[,c(1:(length(dummies)-1))]
  names(dummies)[2:length(dummies)]  =  paste("hol_",names(dummies)[2:length(dummies)],sep="")
  calendar.daily = merge(calendar.daily,dummies)
  #calendar.daily = calendar.daily
  calendar.daily  
}
calendar.daily = f_calendar.daily.get()
cc = calendar.daily
names(cc)

f_calendar.monthly.get=function()
{
  period.id = 1:(12*7)
  months = seq(as.Date("2001/1/1"), by="month", length=12*7)
  calendar.monthly = data.table(period.id, MONTH = rep(1:12,7),YEAR = year(months), start.date = months) 
  end.dates = (calendar.monthly$start.date -1)
  #,max(calendar.daily$dates)

  end.dates = end.dates[2:length(end.dates)]
  end.dates = c(end.dates,as.Date("2007-12-30"))
  calendar.monthly$end.date =  end.dates
  calendar.monthly$dim = as.POSIXlt(calendar.monthly$end.date)$mday 
  names(calendar.monthly)
  
  hols.us = f_calendar.holidays.us.get()
}

 
f_calendar.445.get = function() {
  mysubset <- c(grep("hol_", names(calendar)))
  calendar[, (mysubset) := lapply(.SD, function(x) as.integer(as.logical(x))), .SDcols = mysubset]
  
  calendar.445 = calendar.weekly[,
                                 list( end.date = max(as.Date(week_end_date)),
                                        weeks.in.period = length(WEEK),
                                       (mysubset) := lapply(.SD, function(x) as.integer(as.logical(x)))), 
                                 by = c("Calendar_Year", "Calendar_Period")]
}

#dat.w = sp.item
f_ts.prep.weekly.to.monthly = function(dat.w=sp.item)  
{
  # variables to aggregate = UNITS, DOLLARS, AVG PRICE, PR_FLAG - weighted average/average, 
  # dat.m = dat.w
  dat.d = merge(f_calendar.daily.get(),
                dat.w,
                by="WEEK")
  names(dat.d)
  
  dat.d$UNITS =  dat.d$UNITS/7
  dat.d$DOLLARS =  dat.d$DOLLARS/7
  #dat.d$PRICE = dat.d$DOLLARS/dat.d$UNITS
  
  dat.d$MONTH <- floor_date(dat.d$dates,"month")
  dat.d$Calendar_Month <- month(dat.d$dates)
  dat.d$Calendar_Year <- year(dat.d$dates)
  dat.m = 
    dat.d[,list(UNITS = sum(UNITS, na.rm=TRUE),
              DOLLARS = sum(DOLLARS, na.rm=TRUE),
              PRICE = sum(DOLLARS, na.rm=TRUE)/sum(UNITS, na.rm=TRUE),
              PR = mean(PR, na.rm=TRUE),
              FEAT_ANY = mean(FEAT_ANY, na.rm=TRUE),
              FEAT_A_PLUS = mean(FEAT_A_PLUS, na.rm=TRUE),
              FEAT_A = mean(FEAT_A, na.rm=TRUE),
              FEAT_B = mean(FEAT_B, na.rm=TRUE),
              FEAT_C = mean(FEAT_C, na.rm=TRUE),
              DISP_1 = mean(DISP_2, na.rm=TRUE),
              DISP_2 = mean(DISP_2, na.rm=TRUE)  )
        , by = c("fc.item","MONTH","Calendar_Year","Calendar_Month")]
  
  dat.m
}



f_ts.plot.events = function(mydat, shoulder.span = 3) {
  
  hol.weeks = mydat[Hols_any_binary==1,
                    c("WEEK","Hols_name"),
                    with = F]
  shoulder.weeks = ddply(.data=hol.weeks,
                         .variables = c("WEEK","Hols_name"),
                         .fun=function(x) { 
                           data.table(WEEK = x$WEEK + c(-shoulder.span:shoulder.span),
                                      Offset = c(-shoulder.span:shoulder.span),
                                      Hols_name = rep(x$Hols_name,1+(2*shoulder.span))) 
                           })  
  event.data = merge(shoulder.weeks,mydat,by="WEEK")      
  event.data$Hols_name.x
  myrainbow = brewer.pal(6, "Spectral")  
  p = qplot(data=event.data, x = Offset, y = UNITS, 
            colour = factor(Calendar_Year),
            shape = factor(Calendar_Year),
            geom = "line") +
    scale_color_manual(name = "Year",values = myrainbow) +
    scale_shape_manual(name = "Year",values = 15:20) +
    geom_point(size = 4) + facet_wrap( ~Hols_name.x,nrow=2)
  p = p + theme_bw() + 
    ggtitle ("Holiday Profiles by Year (including shoulder weeks)\n") +
    xlab("Weeks Offest to Holiday Week\n(0 = week of holiday falling)") +
    opts(legend.position = "top")
  
  print(p)
}



f_calendar.hols.xtables = function(mydat)
{
  hol.weeks = mydat[Hols_any_binary==1,
                    c("WEEK","Hols_name","Calendar_Year", "Calendar_Week", "week_end_date", "Hols_any_weekday"),
                    with = F]
  t1 = cast(data=hol.weeks,formula=Hols_name~Calendar_Year,fun.aggregate=sum,value="Calendar_Week")
  t2 = cast(data=hol.weeks,formula=Hols_name~Calendar_Year,fun.aggregate=sum,value="Hols_any_weekday")
  
  print(xtable(t1, caption = "\nWeek of Year in which Holiday occurs"),type="html",caption.placement = "top")
  print("")
  print(xtable(t2,digits=0,caption = "\nDay of Week in which Holiday occurs"),type="html",caption.placement = "top")
  
}






##############


# f_ts.prep.weekly.to.monthly_old = function(dat.fi)  
# {  
#   ## runs the aggregation from weeks to months for all listed variables
#   # sum(UNITS)
#   x = dat.fi
#   x = x[,list(WEEK,PRICE)]
#   x
#   dat.daily = merge(f_calendar.daily.get(),
#                     x,
#                     by="WEEK")
#   dat.daily$units.daily =  dat.daily$day.weight * dat.daily$PRICE *7
#   
#   #dat.daily = dat.daily[,c(2,4)]
#   z = zoo(x=dat.daily$units.daily, order.by=dat.daily$dates)
#   z = xts(z)
#   plot.xts(z,main = "Z Daily")
#   z.monthly = apply.monthly(z,mean,na.rm=TRUE)
#   plot.xts(z.monthly, main = "Z Monthly")    
#   abline(v=.index(z.monthly)[54], col="red") 
#   ts.monthly = ts(z.monthly,freq=12)
#   seasonplot(ts.monthly,col=rainbow(6))
#   stl(ts.monthly)
#   ts.monthly
# }
#f_ts.prep.weekly.to.monthly(dat.fi = sp.item)




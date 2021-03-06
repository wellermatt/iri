## time series plotting functions:
# - actuals/forecast, in-sample, out-of-sample, full span
# - x-axis as time scale, monthly, weekly, custom markers


# need to consider adding markers, possibilities include discount % as size, promo y/n with colour for ABC,
# display as shape: star types, hols as vertical bars
# promo indicators are only really relevant at lowest level, otherwise a composite measure of period promo intensity must be developed


library(ggplot2)
setwd(pth.dropbox.code)
source("./data/DataAdaptor/00_data_adaptor_test.R")
source("./data/DataAdaptor/10_load_data_various.R")

f_load.calendar ()
period.ends = as.Date(calendar.445$end_date[1:72])
week.ends = as.Date(calendar.weekly$week_end_date[1:312])



tsPlot = function(dt, plot.title = "Plot Title")
{
    p = ggplot(data = dt, aes(x = Date, y = UNITS, colour = series.name, symbol = series.name) )    + geom_point() + geom_line() + 
        facet_grid(periodicity~., scales="free_y") +
        theme_bw() + expand_limits(y=0) +
        scale_x_date() #+        scale_y_continuous(limits=c(0,max(dt$UNITS)))
    # restrict y axis to zero
    p = p + xlab("") + ylab("Units Sales\n") + ggtitle(plot.title) + theme(legend.position="none")
    p
}



tsPlot.multi.item = function(dt, plot.title = "Plot Title")
{
    p = ggplot(data = dt, aes(x = Date, y = UNITS, colour = series.name, symbol = series.name) )    + geom_point() + geom_line() + 
        facet_grid(fc.item ~ periodicity, scales="free_y") +
        theme_bw() + expand_limits(y=0) +
        scale_x_date() #+        scale_y_continuous(limits=c(0,max(dt$UNITS)))
    # restrict y axis to zero
    p = p + xlab("") + ylab("Units Sales\n") + ggtitle(plot.title) + theme(legend.position="none")
    p
}

tsPlot.data = function(par.category="beer",par.periodicity = "weekly", par.upc = NULL, par.fc.item = NULL, par.Level = NULL )
{
    if (par.periodicity == "all") 
    {
        sp1 = f_load_data_sp(par.category=par.category, par.periodicity="weekly", par.upc=par.upc, par.fc.item = par.fc.item, par.Level = par.Level )[, periodicity := "weekly"]  
        sp1$Date = week.ends
        sp2 = f_load_data_sp(par.category=par.category, par.periodicity="445", par.upc=par.upc, par.fc.item = par.fc.item, par.Level = par.Level ) [, periodicity := "445"]  
#=======
tsPlot.data = function(par.periodicity = "weekly", par.upc = NULL, par.fc.item = NULL, par.Level = NULL )
{
    if (par.periodicity == "all") 
    {
        sp1 = f_load_data_sp( par.periodicity="weekly", par.upc=par.upc, par.fc.item = par.fc.item, par.Level = par.Level )[, periodicity := "weekly"]  
        sp1$Date = week.ends
        sp2 = f_load_data_sp( par.periodicity="445", par.upc=par.upc, par.fc.item = par.fc.item, par.Level = par.Level ) [, periodicity := "445"]  
        sp2$Date = period.ends 
        common.cols = intersect(names(sp2) , names(sp1))
        
        sp = rbindlist(list(sp1[,eval(common.cols), with=F],
                            sp2[,eval(common.cols), with=F]))
    } else {
        sp = f_load_data_sp(par.category=par.category,par.periodicity=par.periodicity, par.upc=par.upc, par.fc.item = par.fc.item, par.Level = par.Level  ) [, periodicity := par.periodicity] 
        sp = f_load_data_sp(par.periodicity=par.periodicity, par.upc=par.upc, par.fc.item = par.fc.item, par.Level = par.Level  ) [, periodicity := par.periodicity] 
        sp$Date = if (par.periodicity == "445") period.ends else week.ends
    }
    
    sp$series.name = "Actuals"
    # this should be changed to a merge to handle missing weeks in lower level data and multiple fc.items
    # interpolate zeros??
    
    sp
}

TEST = FALSE
if (TEST == TRUE) {
    library("ggplot2")
    #####  TESTING
    dt = tsPlot.data(par.periodicity = "445", par.upc = "00-01-18200-53030", par.Level = 1)
    tsPlot(dt, plot.title = "Units Sold for item 00-01-18200-53030\n")
    
    dt = tsPlot.data(par.periodicity = "weekly", par.upc = "00-01-18200-53030", par.Level = 1)
    tsPlot(dt, plot.title = "Units Sold for item 00-01-18200-53030\n")
    
    
    # chains for a SKU
    dt = tsPlot.data(par.periodicity = "all", par.upc = "00-01-18200-53030", par.Level = 2)
    tsPlot.multi.item (dt, plot.title = "Units Sold for item 00-01-18200-53030\n")
    
    
    ### top 10 per category - use apply/vectorisation!
    
    dt = tsPlot.data(par.category="milk", par.periodicity = "all", par.Level = 1)
    tsPlot.multi.item (dt, plot.title = "Units Sold for Top 10 milk SKUs\n")
    
    dt = tsPlot.data(par.category="carbbev", par.periodicity = "all", par.Level = 1)
    tsPlot.multi.item (dt, plot.title = "Units Sold for Top 10 carbbev SKUs\n")
    
    dt = tsPlot.data(par.category="beer", par.periodicity = "all", par.Level = 1)
    tsPlot.multi.item (dt, plot.title = "Units Sold for Top 10 beer SKUs\n")
}

#####  TESTING
dt = tsPlot.data(par.periodicity = "445", par.upc = "00-01-18200-53030", par.Level = 2)

dt = tsPlot.data(par.periodicity = "all", par.upc = "00-01-18200-53030", par.Level = 2)
tsPlot(dt, plot.title = "Units Sold for item 00-01-18200-53030\n")

dt = tsPlot.data(par.periodicity = "all")
tsPlot.multi.item (dt, plot.title = "Units Sold for item 00-01-18200-53030\n")


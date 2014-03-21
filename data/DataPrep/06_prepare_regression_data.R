
# important functionality of this script is to expand the weekly data to include missing periods as NA 
# and adding lags, harmonic and holiday variables

library("data.table")
options(width=120)
machine = (Sys.info()["nodename"])

#source("E:/Git/iri/.Rprofile")
source("~/projects/iri/.Rprofile")


###############################
# THIS CODE NEEDS TO BE RUN TO CREATE THE WEEKLY INPUT DATASETS
options(echo=TRUE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)
print(args)



setwd(pth.dropbox.code)    	; source("./data/DataAdaptor/10_load_data_various.R")   # loads the calendars, the category data, the fc.items, the stores file
setwd(pth.dropbox.code)    	; source("./data/DataPrep/06_prepare_regression_data_functions.R")   # loads the calendars, the category data, the fc.items, the stores file


f_load.stores()
f_load.calendar()
weeks = calendar.weekly[,list(WEEK)][1:312]
harmonics.weekly = f_ts.seas.harmonics(season.type="WEEKLY", num.periods = 312, max.harmonics = 26)      # add the 26 sin and cos variables
harmonics.weekly = data.table(cbind(WEEK = 1:312, harmonics.weekly))
calendar.slim = calendar.weekly.lead.lag[,c(1:2,7:length(calendar.weekly.lead.lag)),with=FALSE][1:312,!"Hols_name",with=FALSE]
harmonics.445 = f_ts.seas.harmonics(season.type="445", num.periods = 72, max.harmonics = 6)      # add the 26 sin and cos variables
harmonics.445 = data.table(cbind(period_id = 1:72, harmonics.445), key = "period_id")    


f_run_prep.reg.dat = function(par.category)
{
    
    
}

setwd(pth.dropbox.data)
categories = c("beer", "carbbev", "milk", "razors")

cats = lapply(categories[4], 
              function(par.category) {
                  f_load.dat.subset(par.category)
			    f_load.fc.items.subset(par.category)
                  f_regression.data.main(par.category, par.weekly = TRUE, par.445=TRUE)
              })




head(dat.cat)    ; sapply(dat.cat, class)
head(fc.items)  ; sapply(fc.items, class)



#  this code needs to execute only once regardless of how many categories are being processed
# need to clean up  references to 313 in some cases



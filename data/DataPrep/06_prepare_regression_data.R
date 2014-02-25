
# important functionality of this script is to expand the weekly data to include missing periods as NA 
# and adding lags, harmonic and holiday variables

library("data.table")
rm(list=ls())
options(width=150)
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

###############################
# THIS CODE NEEDS TO BE RUN TO CREATE THE WEEKLY INPUT DATASETS
options(echo=TRUE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)
print(args)

#  this code needs to execute only once regardless of how many categories are being processed
# need to clean up  references to 313 in some cases
setwd(pth.dropbox.code)		; source("./ModelFitting/LoadData.R")   # loads the calendars, the category data, the fc.items, the stores file
f_load.calendar()
setwd(pth.dropbox.code)		; source("./DataPrep/06_prepare_regression_data_functions.R")   # loads the calendars, the category data, the fc.items, the stores file

harmonics.weekly = f_ts.seas.harmonics(season.type="WEEKLY", num.periods = 313, max.harmonics = 26)      # add the 26 sin and cos variables
harmonics.weekly = data.table(cbind(WEEK = 1:313, harmonics.weekly))
calendar.slim = calendar.weekly.lead.lag[,c(1:2,7:length(calendar.weekly.lead.lag)),with=FALSE][1:313,!"Hols_name",with=FALSE]
harmonics.445 = f_ts.seas.harmonics(season.type="445", num.periods = 72, max.harmonics = 6)      # add the 26 sin and cos variables
harmonics.445 = data.table(cbind(period_id = 1:72, harmonics.445), key = "period_id")	

setwd(pth.dropbox.data)

categories = c("beer", "carbbev", "milk")

cats = lapply(categories[3], 
	function(par.category) f_regression.data.main(par.category, par.weekly = TRUE, par.445=TRUE))

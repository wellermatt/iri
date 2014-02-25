

library(reshape2) #library(plyr) ; ; library(ggplot2)
rm(list = ls()) ; gc()

## define the parameters for this procedure
args <- commandArgs(trailingOnly = TRUE)
print (args)
par.category = args[1]    #;par.category = "beer"

opt.within.chains.correlations = TRUE
opt.between.chains.correlations = FALSE

options(width = 250)
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

setwd(pth.dropbox.code)
source("./DataPrep/10_load_data_various.R")   # loads the calendars, the category data, the fc.items, the stores file
source("./DataAnalysis/05b_correlation_functions.R")
source("./GenericRoutines/useful_functions.R")

setwd(pth.dropbox.data)
f_load.dat.subset(par.category)
f_load.fc.items.subset(par.category)
f_load.stores()
f_load.calendar()


dat.cat = dat.cat.weekly ; dat.cat.weekly = NULL
dat.cat = dat.cat[!is.infinite(PRICE)]  ######### this needs to be implemented in the data cleansing routine
variables.to.test = c("PRICE", "PR", sort(grep("FEAT_|DISP_", names(dat.cat), value = TRUE)))
#variables.to.test = "PRICE"

if (opt.within.chains.correlations == TRUE) f_cor.within.chains.par()     # f_cor.within.chains()
if (opt.between.chains.correlations == TRUE) f_cor.between.chains()  # does not benefit from parall

#system.time(f_cor.between.chains())
#system.time(f_cor.between.chains.par())
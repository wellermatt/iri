#source("config.R")

source("./data/DataAdaptor/10_load_data_various.R") 

setwd(pth.dropbox.data) ; f_load.calendar()

par.category = "beer" ; par.periodicity = "445"	



f_da.reg.cat.all = function(par.category, par.periodicity, par.item="00-01-18200-53030", bo.save =FALSE) {

	# get an input dataset for regression for a whole category
	# optionally split it for a single item
	# optionally save it to a subset file as a test regression file	
    setwd(pth.dropbox.data)
    fil = paste("./regression datasets/", par.category, ".regression.data.", par.periodicity, ".rds", sep= "")
	sp = readRDS(fil)
	if (!is.null(par.item) & bo.save == TRUE) {
		sp = sp[UPC==par.item]
		fil = paste("./regression datasets/", par.category, ".test.regression.data.", par.periodicity, ".rds", sep= "")
		sp[,fc.item:=factor(fc.item)]
		saveRDS(sp,fil)
	}
	sp
}

f_da.reg.cat.test = function(par.category, par.periodicity) {
	
    setwd(pth.dropbox.data)
    fil = paste("./regression datasets/", par.category, ".test.regression.data.", par.periodicity, ".rds", sep= "")
	sp = readRDS(fil)
	sp
}




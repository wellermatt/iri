
setwd(pth.dropbox.code) ; source("./data/DataAdaptor/10_load_data_various.R") 
setwd(pth.dropbox.data) ; f_load.calendar()

#par.category = "beer" ; par.periodicity = "445"	


f_da.reg.cat.all = function(par.category, par.periodicity, par.item = NULL, bo.save.subset = FALSE) {

	# get an input dataset for regression for a whole category
	# optionally split it for a single item
	# optionally save it to a subset file as a test regression file	
    setwd(pth.dropbox.data)
    fil = paste("./regression datasets/", par.category, ".regression.data.", par.periodicity, ".rds", sep= "")
	sp = readRDS(fil)
    
    # optionally filter the data
	if (!is.null(par.item)) sp = sp[UPC==par.item]
    
    # take out week 313 for the time being
    if (par.periodicity == "weekly") sp = sp[WEEK != 313]
    
    # optionally save the subset of data
	if (bo.save.subset == TRUE) {		
		fil = paste("./regression datasets/", par.category, ".test.regression.data.", par.periodicity, ".rds", sep= "")
		sp[,fc.item:=factor(fc.item)]
		saveRDS(sp, fil)
	}
	sp
}

f_da.reg.cat.test = function(par.category, par.periodicity) {
	
    setwd(pth.dropbox.data)
    fil = paste("./regression datasets/", par.category, ".test.regression.data.", par.periodicity, ".rds", sep= "")
	sp = readRDS(fil)
	sp
}




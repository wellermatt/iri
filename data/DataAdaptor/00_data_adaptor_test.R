
#setwd(pth.dropbox.code) ; source("./data/DataAdaptor/10_load_data_various.R") 
#setwd(pth.dropbox.data) ; f_load.calendar()
#par.category = "beer" ; par.periodicity = "445"	


f_adaptor.reg.cat.all = function(par.category, par.periodicity, 
                            par.upc = NULL, Level = 1, univariate = TRUE, bo.save.subset = FALSE) {

	# get an input dataset for regression for a whole category
	# optionally split it for a single item
	# optionally save it to a subset file as a test regression file	
    setwd(pth.dropbox.data)
    fil = paste("./regression datasets/", par.category, ".regression.data.", par.periodicity, ".rds", sep= "")
	sp = readRDS(fil)
    if (par.periodicity == "445") setnames(sp, "period_id", "period")
    if (par.periodicity == "weekly") setnames(sp, "WEEK", "period")
    
    # optionally filter the data
	if (!is.null(par.upc)) sp = sp[UPC==par.upc]
    if (Level < 3) {
        items = sp[,as.character(unique(fc.item))]
        items.sub = items[which(unlist(lapply(strsplit(items,"/"),length)) <= Level)]
        sp = droplevels(sp[fc.item %in% items.sub])
    }
    
    # take out week 313 for the time being
    if (par.periodicity == "weekly") sp = sp[period <= 312]
    
    # reduce the dataset columns if multivariate modelling is not required (dependent variable and indices only)
    if (univariate == TRUE) sp = data.table(category = par.category, sp[,list(fc.item, period, UNITS)])
    
    # optionally save the subset of data
	if (bo.save.subset == TRUE) {		
		fil = paste("./regression datasets/", par.category, ".test.regression.data.", par.periodicity, ".rds", sep= "")
		sp[,fc.item:=factor(fc.item)]
		saveRDS(sp, fil)
	}
	sp
}


f_load_data_sp = function(par.category="beer", par.periodicity="445", par.item = NULL)
{
    if (par.periodicity == "445") sp = f_da.reg.cat.all(par.category = par.category, par.periodicity = par.periodicity)
    if (par.periodicity == "weekly") sp= f_da.reg.cat.all(par.category=par.category, par.periodicity=par.periodicity)
    
    
    sp
}
#f_load_data_sp()


f_da.reg.cat.test = function(par.category, par.periodicity) {
	
    setwd(pth.dropbox.data)
    fil = paste("./regression datasets/", par.category, ".test.regression.data.", par.periodicity, ".rds", sep= "")
	sp = readRDS(fil)
	sp
}




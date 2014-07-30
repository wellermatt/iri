

f_adaptor.reg.cat.all = function(par.category, par.periodicity, 
                                 par.upc = NULL, par.fc.item = NULL, par.Level = 1, 
                                 par.univariate = TRUE, bo.save.subset = FALSE) {

	# get an input dataset for regression for a whole category
	# optionally split it for a single item (upc or fc.item)
	# optionally save it to a subset file as a test regression file	
    setwd(pth.dropbox.data)
    fil = paste("./regression datasets/", par.category, ".regression.data.", par.periodicity, ".rds", sep= "")
	sp = readRDS(fil)
    if (par.periodicity == "445") setnames(sp, "period_id", "period")
    if (par.periodicity == "weekly") setnames(sp, "WEEK", "period")
    
    # optionally filter the data
	if (!is.null(par.upc)) sp = sp[UPC==par.upc]
    if (!is.null(par.fc.item)) sp = sp[fc.item==par.fc.item]
    if (par.Level < 3) {
        items = sp[,as.character(unique(fc.item))]
        items.sub = items[which(unlist(lapply(strsplit(items,"/"),length)) == par.Level)]
        sp = droplevels(sp[fc.item %in% items.sub])
    }
    
    # take out week 313 for the time being
    if (par.periodicity == "weekly") sp = sp[period <= 312]
    
    # reduce the dataset columns if multivariate modelling is not required (dependent variable and indices only)
    if (par.univariate == TRUE) sp = data.table(category = par.category, sp[,list(fc.item, period, UNITS)])
    
    # optionally save the subset of data
	if (bo.save.subset == TRUE) {		
		fil = paste("./regression datasets/", par.category, ".test.regression.data.", par.periodicity, ".rds", sep= "")
		sp[,fc.item:=factor(fc.item)]
		saveRDS(sp, fil)
	}
	sp
}


f_load_data_sp = function(par.category="beer", par.periodicity="445", 
                          par.upc = NULL, par.fc.item = NULL, par.Level = 1)
{
    if (par.periodicity == "445") 
        sp = f_adaptor.reg.cat.all(par.category = par.category, par.periodicity = par.periodicity, 
                                   par.upc=par.upc, par.fc.item = par.fc.item, par.Level = par.Level, par.univariate = FALSE)
    
    if (par.periodicity == "weekly") 
        sp = f_adaptor.reg.cat.all(par.category=par.category, par.periodicity=par.periodicity, 
                                   par.upc=par.upc, par.fc.item = par.fc.item, par.Level = par.Level,par.univariate = FALSE)
    
    if (!is.null(par.fc.item)) sp = sp[fc.item == par.fc.item]
    if (!is.null(par.upc)) sp = sp[UPC == par.upc]    
    #if (!is.null(par.Level)) sp = sp[Level == par.Level]
    sp
}



f_da.reg.cat.test = function(par.category, par.periodicity) {
	
    setwd(pth.dropbox.data)
    fil = paste("./regression datasets/", par.category, ".test.regression.data.", par.periodicity, ".rds", sep= "")
	sp = readRDS(fil)
	sp
}






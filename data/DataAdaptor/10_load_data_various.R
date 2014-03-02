#library(data.table)
#setwd(pth.dropbox.data)

f_load.stores = function(){
	stores <<- readRDS("./iri reference data/stores.clean.rds")
	#qplot(x=store_rev_avg,data=stores) + 
	#ggtitle("Distribution of store size\nTotal Store Revenue  ($m/year)\n")
	#qplot(y=store_rev_avg, x = chain, data=stores, geom="point", stat="summary", fun.y="sum")
}

f_load.calendar = function(){
  setwd(pth.dropbox.data)
	calendar.weekly <<- readRDS("./iri reference data/calendar/calendar.weekly.rds")
	calendar.weekly.lead.lag <<- readRDS("./iri reference data/calendar/calendar.weekly.lead.lag.rds")
	calendar.445.lead.lag <<- readRDS("./iri reference data/calendar/calendar.445.lead.lag.rds")
	cols.to.drop.445 = grep("_LEAD1|_LAG1",names(calendar.445.lead.lag), value = TRUE)
	calendar.445 <<- readRDS("./iri reference data/calendar/calendar.445.rds")
	calendar.445[,(cols.to.drop.445) := NULL]
}

f_load.dat.subset = function(category, par.weekly = TRUE, par.445 = TRUE){
	if (par.weekly == TRUE) {
		fil = paste("./iri category subsets/reformatted/",  category, ".subset.sales.promos.weekly.rds", sep = "")  #category, "/",
		dat.cat.weekly <<- readRDS(fil)
	}
	if (par.445 == TRUE) {
		fil = paste("./iri category subsets/reformatted/",  category, ".subset.sales.promos.445.rds", sep = "")  #category, "/",
		dat.cat.445 <<- readRDS(fil)
	}
}

f_load.fc.items.subset = function(category) {
	fil = paste("./iri category subsets/reformatted/",  category, ".subset.fc.items.rds", sep = "")  #category, "/",
	fc.items <<- readRDS(fil)
}

f_load.products = function(category, opt.fieldset = 1) {

	# add the product attributes
	setwd(pth.dropbox.data)
	fil = paste("./iri reference data/upc/prod_",par.category,".csv", sep = "")
	upc.master = data.table(read.csv(fil, stringsAsFactors=FALSE))
	if (opt.fieldset == 1) return(upc.master[, list(UPC,L9,VOL_EQ)])
	
}


#=============================== MAIN ==========================
#f_load.dat.subset(par.category)
#f_load.fc.items.subset(par.category)
#f_load.stores()
#f_load.calendar()

#weeks = calendar.weekly[,list(WEEK)][1:313]

#head(dat.cat)	; sapply(dat.cat, class)
#head(fc.items)  ; sapply(fc.items, class)


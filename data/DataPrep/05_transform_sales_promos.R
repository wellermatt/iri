# Script to take the categorigal promotional variables in IRI format and convert them to dummies
# Aggregate through the levels to the Item total with promotional dummies at each level
# now includes the expansion through NA periods

# needs revision to move some elements of the data tidy up into the subsetting process
#
# Additionally aggregate the transformed data from weekly to monthly
#
#

f_load.stores.clean = function()
## load the cleaned list of stores with their related chain and market attributes
{
	setwd(pth.dropbox.data)
    stores = readRDS("./iri reference data/stores.clean.rds")
	stores
}

f_cleanse.data.1 = function(dat, upc = as.character(NA))

    ##===  LOAD THE MAIN DATA FROM A SUBSET e.g. Top 10 Beer UPCs ================================
{	
	if (!is.na(upc)) { upc.list = unique(dat$UPC)   ; 	dat = dat[UPC == upc]  }
	
	dat$year	= NULL							# drop the year column for this analysis
	dat			= dat[IRI_KEY %in% stores$IRI_KEY]  # filter out any stores that are not in our master list
	
	# update any zeros in sales/price to NA
	dat[UNITS <= 0 | DOLLARS <= 0,c("UNITS","DOLLARS"):=NA] 
	dat[,PRICE:= (DOLLARS/UNITS)]
	
	# extend the data to every period
	sku.span = dat[,list(first.period = min(WEEK), last.period = max(WEEK)), by = list(UPC,IRI_KEY)]
	sku.span = sku.span[,list(WEEK = first.period:last.period), by = list(UPC,IRI_KEY)]
	setkeyv(sku.span, c("UPC","IRI_KEY","WEEK"))
	dat = merge(sku.span, dat, all.x=TRUE)
	
	print(paste("Number of items", nrow(dat)))
   	print(paste("Unique number of stores", length(unique(dat$IRI_KEY))))
	
	## tag the CHAIN and MARKET onto the main data for aggregation and generation of fc.item field
	dat = droplevels(merge(dat, stores[,list(IRI_KEY, chain)], by="IRI_KEY", all.x = TRUE))
    
	## generate the fc.item field for all records and make it the unique key
	dat = within(dat,expr = fc.item <- paste(UPC,chain,IRI_KEY,sep="/"))
	keycols = c("fc.item","WEEK") ; setkeyv(dat,keycols)
 	
	dat 
}

f_generate.fc.items = function(dat)
## FURTHER DATA MANIPULATION, CREATION OF UNIQUE FORECAST ITEMS TABLE
# very much hard coded to a 3 level hierarchy of store, chain, item WITHOUT market
{	
	fc.items = unique(dat[,list(fc.item,lvl = 1,UPC,chain,IRI_KEY)])
	keycols = c("fc.item") ; setkeyv(fc.items,keycols)   
	
	#========== generate the level 2 items  
	tmp.2 = unique(fc.items[,list(fc.item		= paste(UPC, chain, sep="/"),
									lvl			= 2,
									UPC,
									chain,
									IRI_KEY		= as.integer(NA))])
	tmp.2$IRI_KEY = tmp.2[,lapply(.SD,as.integer),.SDcols="IRI_KEY"]
	
	#========== generate the level 3 items
	tmp.3 = unique(fc.items[,list(fc.item		= as.character(UPC),
									lvl			= 3,
									UPC,
									chain		= as.integer(NA),
									IRI_KEY		= as.integer(NA))])
	tmp.3$IRI_KEY = tmp.3[,lapply(.SD,as.integer),.SDcols="IRI_KEY"]
	
	# merge the 3 levels together to form the fc.items listing
	fc.items = rbind(fc.items, tmp.2,tmp.3)
	
	# merge with the products file to get the product description
	upc.master = f_load.products(par.category)
	fc.items = merge(fc.items, upc.master, by = "UPC")
	fc.items[,lvl:=ordered(lvl,levels = c(1,2,3), labels = c("STORE","CHAIN","ITEM"))]
	fc.items[, lvl := factor(lvl,levels(lvl)[c(3,1,2)])]
	keycols = c("fc.item") ; setkeyv(fc.items,keycols)
	fc.items
}

f_level1_promo_dummies = function(dat)
{
	dat$D = factor(dat$D,labels = c("NONE", "MINOR", "MAJOR"))
	dat$F = factor(dat$F)
	setnames(dat, old = c("D","F"), new = c("DISP","FEAT"))
	dummies = dummyVars(UNITS ~ FEAT + DISP, sep = "_", data=dat)
	dummies = data.table(predict(dummies, newdata=dat))
	dummies$FEAT_ANY = 1 - dummies$FEAT_NONE #abs(promos.pos.feat$FEAT_NONE-1)
	dummies$DISP_ANY = 1 - dummies$DISP_NONE
	dat$FEAT = NULL ; dat$DISP = NULL
	dat = cbind(dat, dummies)
	dat = setcolorder(dat, c(col.order, names(dat)[(length(col.order)+1):length(dat)]))
	dat		
}

f_level2_promo_dummies = function(dat)
{
	DT1 = dat[, list(IRI_KEY = as.integer(NA),
					UNITS = sum(UNITS,na.rm=TRUE),
					DOLLARS = sum(DOLLARS, na.rm = TRUE),
					PR = weighted.mean(PR,w=UNITS,na.rm=TRUE)  ),    #########  weighted mean or mean?
				 by="UPC,chain,WEEK"]
	DT1$fc.item = with(DT1, paste(UPC, chain, sep="/"))
	DT1$PRICE = DT1$DOLLARS/DT1$UNITS
	
	promo.cols = grep("UNITS|FEAT_|DISP_", names(dat))	
	if (promo.flags.agg.methods == "wmean"){  		# using weighted mean
		DT2 = dat[, lapply(.SD, weighted.mean, w=UNITS, na.rm=TRUE), by="UPC,chain,WEEK", .SDcols = promo.cols]
	} else {		# using mean
		DT2 = dat[, lapply(.SD, mean, na.rm=TRUE), by="UPC,chain,WEEK", .SDcols=promo.cols]
	}
	DT2$fc.item = with(DT2,paste(UPC,chain,sep="/"))
	DT2$UPC = DT2$chain = DT2$UNITS = NULL
	DT = merge(DT1, DT2, by=c("fc.item","WEEK"))
	#setcolorder(DT, names(DT)[c(length(DT),3:(length(DT)-1),1:2)])
	setcolorder(DT, c(col.order, names(dat)[(length(col.order)+1):length(dat)]))
	DT
}

f_level3_promo_dummies = function(dat)
{
	DT1 = dat[, list(IRI_KEY = as.integer(NA),
					chain = as.integer(NA),
					UNITS = sum(UNITS,na.rm=TRUE),
					DOLLARS = sum(DOLLARS, na.rm = TRUE),
					PR = weighted.mean(PR,w=UNITS,na.rm=TRUE)  ),    #########  weighted mean or mean?
				 by="UPC,WEEK"]
	DT1$fc.item = DT1$UPC
	DT1$PRICE = DT1$DOLLARS/DT1$UNITS
	
	promo.cols = grep("UNITS|FEAT_|DISP_", names(dat))	
	if (promo.flags.agg.methods == "wmean"){  		# using weighted mean
		DT2 = dat[, lapply(.SD, weighted.mean, w=UNITS, na.rm=TRUE), by="UPC,WEEK", .SDcols = promo.cols]
	} else {		# using mean
		DT2 = dat[, lapply(.SD, mean, na.rm=TRUE), by="UPC,WEEK", .SDcols=promo.cols]
	}
	DT2$fc.item = DT2$UPC
	DT2$UPC = DT2$UNITS = NULL
	DT = merge(DT1, DT2, by=c("fc.item","WEEK"))
	#setcolorder(DT, names(DT)[c(length(DT),3:(length(DT)-1),1:2)])
	setcolorder(DT, c(col.order, names(dat)[(length(col.order)+1):length(dat)]))
	DT
}

f_data.aggregate.week.445 = function(dat.w, opt.weighted.mean = FALSE) {

    # after generating the weekly file with no gaps and dummies
	calendar.weekly <<- readRDS("./iri reference data/calendar/calendar.weekly.rds")
	calendar.map.week.445 = calendar.weekly[,list(WEEK, period_id)]
	dat.w = merge(calendar.map.week.445,dat.w, by = "WEEK")
	dat.m1 = 
		dat.w[,list(UNITS = sum(UNITS, na.rm=TRUE),
					DOLLARS = sum(DOLLARS, na.rm=TRUE),
					PRICE = sum(DOLLARS, na.rm=TRUE)/sum(UNITS, na.rm=TRUE),
					PR = mean(PR, na.rm=TRUE))
			  , by = c("fc.item", "UPC", "chain", "IRI_KEY","period_id")]
	sd.cols = c("UNITS", "PR", grep("FEAT_|DISP_", names(dat.w), value = TRUE))
	if (opt.weighted.mean == TRUE) {
		dat.m2 = dat.w[,lapply(.SD, weighted.mean, w=UNITS, na.rm=TRUE)
				, by = c("fc.item", "UPC", "chain", "IRI_KEY","period_id")
				, .SDcols = sd.cols]
	} else {
		dat.m2 = dat.w[,lapply(.SD, mean, na.rm=TRUE)
				, by = c("fc.item", "UPC", "chain", "IRI_KEY","period_id")
				, .SDcols = sd.cols]
	}
	dat.m2[,(c("UNITS", "UPC", "IRI_KEY", "chain", "PR")):= NULL]
	keycols = c("fc.item", "period_id")
	setkeyv(dat.m1, keycols)  ;  setkeyv(dat.m2, keycols)
	dat.m1[dat.m2]
}
#==================================================================
#  MAIN PROCEDURE
#==================================================================

# potential enhancements include using the dummies package, integrating MARKET into the fc.items as an additional level,
# dropping the fc.item descriptor fields (UPC, IRI_KEY, chain), parameterising the script for categories
# move the further subsetting to the earlier stage of subsetting on HEC

library(data.table) ; library(plyr) ; library(reshape2)  ;  library(caret)

setwd(pth.dropbox.code)  ;  source("./data/DataAdaptor/10_load_data_various.R")

categories = c("yoghurt")

categories = list.files("/storage/users/wellerm/data/04_subset")

for (par.category in categories[15:17])
{
	setwd(pth.dropbox.data)
    
	promo.flags.agg.methods = "wmean"    # which method to use to aggregate the promotional flags (wmean or mean)

	# load the subset of data in original IRI format (i.e. categorical variables for promotions, no price calculated)
	fil = paste0("/storage/users/wellerm/data/04_subset/", par.category, "/", par.category, ".subset.rds")
	dat.cat.1 = data.table(readRDS(fil))
	col.order = c("fc.item", "UPC", "chain", "IRI_KEY",  "WEEK", "UNITS", "DOLLARS", "PRICE", "PR")

	stores = f_load.stores.clean()
	dat.cat.1 = f_cleanse.data.1(dat.cat.1)    # this will create the fc.item field and tags on the chain and market which are used for aggregation
	#dat.cat.1 = f_cleanse.data.2(dat.cat.1)    # this will subset further based on the summary stats of the subset
	fc.items = f_generate.fc.items(dat.cat.1)
	fc.items[,.N,by=list(lvl)]

	sp.cat.1 = f_level1_promo_dummies(dat.cat.1)
	sp.cat.2 = f_level2_promo_dummies(sp.cat.1)
	sp.cat.3 = f_level3_promo_dummies(sp.cat.1)

	sp.cat = rbind(sp.cat.1, sp.cat.2, sp.cat.3)
	setnames(x = sp.cat, gsub("\\+", "_PLUS", x = names(sp.cat)))

	# remove the PROMO_NONE flags as we now have PROMO_ANY flags instead which make more sense
	sp.cat$FEAT_NONE = NULL
	sp.cat$DISP_NONE = NULL
    
    pth.formatted = paste0("/storage/users/wellerm/data/04_subset/", par.category, "/")
    
	#fil = paste0(pth.formatted, par.category, ".subset.reformatted.rds")
	saveRDS(sp.cat, paste0(pth.formatted, par.category,".subset.sales.promos.weekly.rds"))
	saveRDS(fc.items, paste0(pth.formatted, par.category,".subset.fc.items.rds"))
	
	# monthly data aggregation and saving
	setwd(pth.dropbox.data)
	sp.cat.445 = f_data.aggregate.week.445(dat.w = sp.cat, opt.weighted.mean = FALSE)
	saveRDS(sp.cat.445, paste0(pth.formatted, par.category,".subset.sales.promos.445.rds"))
	
}
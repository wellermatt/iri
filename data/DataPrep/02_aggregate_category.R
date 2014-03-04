# this script will handle the aggregation of a full dataset for a category to various levels.

library(data.table)
#source("../scripts/memory_usage.R")

## define the parameters for this procedure
args <- commandArgs(trailingOnly = TRUE)
print (args)

par.category = args[1]    # "diapers" # 

f_consecutive.missing.values = function(weeks.avail) {

	# potential to enhance performance of this function considerably
	# http://stackoverflow.com/questions/5012516/count-how-many-consecutive-values-are-true

	y = rep(0, max(weeks.avail) - min(weeks.avail)+1) 
	y[weeks.avail] = 1
	runs = rle(y)
	z = as.integer(max(runs$lengths[runs$values==0]))
	if (is.na(z)) z = 0
	as.integer(z)
}

##########################################################################################################
#  MAIN BODY OF THE PROCEDURE
##########################################################################################################
f_iri.category.summarise = function(par.category,
                                    pth.trans = "/storage/users/wellerm/data/02_tf/sales/all/",
                                    pth.agg = "/storage/users/wellerm/data/03_agg/")
{    
    # function will aggregate the data for the whole category at multiple levels
    # currently the levels required cannot be parameterised and hence all are produced
    
    # this is the destination path for the summary files
    dir.create(file.path(pth.agg, par.category))
	pth.agg = paste0(pth.agg, par.category, "/")

    ## load the transformed data for the category (all years) and convert to a data table and output stats
    fil =  paste0(pth.trans, par.category, ".tf.all.rds") ; print(fil)
    da = readRDS(fil)

    # just do a memory check when the data has been loaded
    print(tables()) ; print(gc())

    
	###### WEEKLY ITEM STATS
	dat.upc.week = da[,j=list(revenue = sum(DOLLARS), 
				units_sold = sum(UNITS), 
				store_count = length(IRI_KEY)),
				by=list(UPC,WEEK)]
	dat.upc.week = dat.upc.week[with(dat.upc.week, order(-revenue)), ]
	saveRDS(dat.upc.week, file = paste0(pth.agg, par.category, ".dat.upc.week.rds"))
	print("dat.upc.week") ; nrow(dat.upc.week)
	dat.upc.week = NULL ; gc()

	###### WEEKLY STORE STATS (all items
	dat.store.week = da[,j=list(revenue = sum(DOLLARS), 
				units_sold = sum(UNITS), 
				item_count = length(UPC)),
				by=list(IRI_KEY,WEEK)]
	dat.store.week = dat.store.week[with(dat.store.week, order(-revenue)), ]
	saveRDS(dat.store.week, file = paste0(pth.agg, par.category, ".dat.store.week.rds")) ; 
	print("dat.store.week")  ; nrow(dat.store.week)
	dat.store.week = NULL ; gc()
	
	## WEEKLY CATEGORY STATS (all stores, all items)
	dat.category.week = da[,j=list(
				revenue = sum(DOLLARS), 
				units_sold = sum(UNITS), 
				n = length(DOLLARS), 
				item_count = length(unique(UPC)), 
				store_count = length(unique(IRI_KEY))),
				by = WEEK]
	dat.category.week = dat.category.week[with(dat.category.week, order(WEEK)),] 
	saveRDS(dat.category.week, file = paste0(pth.agg, par.category, ".dat.category.week.rds"))  
	dat.category.week = NULL; gc()
	print("dat.category.week") ; nrow(dat.category.week)

	## CALCULTAE SUMMARIES OVER THE WHOLE HORIZON
	# sku-level stats
	dat.upc.store.horizon = da[,j=list(revenue = sum(DOLLARS), 
				units_sold = sum(UNITS), 
				start_week = min(WEEK),
				end_week = max(WEEK),
				num_weeks = length(WEEK),
				max_consecutive_missing = f_consecutive.missing.values(WEEK)),
				by=list(IRI_KEY,UPC)]
	dat.upc.store.horizon  = dat.upc.store.horizon [with(dat.upc.store.horizon , order(-revenue)), ]
	saveRDS(dat.upc.store.horizon , file = paste0(pth.agg, par.category, ".dat.upc.store.horizon.rds"))  
	print("dat.upc.store.horizon") ; nrow(dat.upc.store.horizon)
	dat.upc.store.horizon  = NULL ; gc()

	# store-level stats
	dat.store.horizon = da[,j=list( 
				revenue = sum(DOLLARS),  
				units_sold = sum(UNITS), 
				n = length(DOLLARS),
				item_count = length(unique(UPC)),
				weeks_sold = length(unique(WEEK)),
				start_week = min(WEEK),
				end_week = max(WEEK),
				max_consecutive_missing = f_consecutive.missing.values(WEEK)),
				by = IRI_KEY]
	dat.store.horizon = dat.store.horizon[with(dat.store.horizon, order(-revenue)),]
	saveRDS(dat.store.horizon, file = paste0(pth.agg, par.category, ".dat.store.horizon.rds"))   
	print("dat.store.horizon") ; nrow(dat.store.horizon)  
	dat.store.horizon = NULL ; gc()

	# item level stats	
	dat.upc.horizon = da[,j=list(
				revenue = sum(DOLLARS), 
				units_sold = sum(UNITS), 
				n = length(DOLLARS), 
				store_count = length(unique(IRI_KEY)), 
				weeks_sold = length(unique(WEEK)),
				start_week = min(WEEK),
				end_week = max(WEEK),
				max_consecutive_missing = f_consecutive.missing.values(WEEK)),
				by = UPC]
	dat.upc.horizon = dat.upc.horizon[with(dat.upc.horizon, order(-revenue)), ]
	saveRDS(dat.upc.horizon, file = paste0(pth.agg, par.category,".dat.upc.horizon.rds"))
	top.10.upc = dat.upc.horizon[1:10,1]  
    
    lsos()
    da = NULL ; dat.upc.horizon = NULL ; gc()
}

f_iri.category.summarise(par.category)


if (TEST == TRUE) {
    pth.trans = paste0(pth.dropbox.data, "tf-test/")
    pth.agg = "C:/Users/Matt/Dropbox/HEC/IRI_DATA/iri category summaries/"
    f_iri.category.summarise(par.category, pth.trans, pth.agg)
}



print ("===== SCRIPT COMPLETED =====")

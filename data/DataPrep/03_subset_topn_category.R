# this procedure needs considerable overhaul to make it quicker and allow more flexible subsetting
# should accept parameters for:

    # the minimum number of weeks required and/or start/end week for the item
    # the minimum number of weeks required and/or start/end week for the item/store
    # the minimum number of stores in a chain to allow inclusion
    # the number of items to include in the eventual filtered transaction file

# also requires fitting in a test to ensure IRI_KEY is in the list of stores with no problems

# there should be two modes for this:
# 1. investigating the scope of a subset given a set of input splitting parameters
# 2. generate a subset of transactions from the full data set based on a set of splitting parameters

#====================== PARAMETERS =================================

library(data.table)



f_load.upc.category = function(pth.upc, par.category)
{
	## load the product catalogue information for the specified category
	
    fil = paste0(pth.upc, "prod_", par.category, ".csv")
	upc <- read.csv(fil, header=TRUE, strip.white = TRUE)
	upc.attributes = names(upc)[15:length(names(upc))]        # get the custom attributes for the category
	print(upc.attributes)

    upc = upc[,c(11,8,6, 1:5)]
    names(upc) = c("VEND","UPC","UPC_NAME","CAT1","CAT2","SUPP1","SUPP2","BRAND")
    
    # exclude private label brands
    upc$PRV = grepl("+PRV ", upc$UPC_NAME)
    upc = upc[upc$PRV==FALSE,]
    data.table(droplevels(upc))
}

f_upc.store.horizon = function(pth.agg, par.category, 
                               par.upc.top.n = 10,
                               par.upc.store.min.weeks = 0, 
                               par.upc.store.proportion.weeks = 0, 
                               par.max.consec.missing.periods) 
    
    # this funmction will examine the category summary file to see which sku/location forecast items are in scope 
    # given the restricting parameters provided.  it will return a list of upc/store combinations for use elsewhere
    {
	
	# function to identify the SKUS (item by store) that we will be interested in using
	# will take the top n items and identify where they have been sold for more than a specified
	# number of weeks
	fil = paste0(pth.agg, par.category, "/", par.category,".dat.upc.store.horizon.rds")
    dush = data.table(readRDS(fil))
    
    dush[, category := par.category]
	dush[, start_week := as.integer(start_week)]
   	dush[, end_week := as.integer(end_week)]
	
    print(paste(nrow(dush), " rows in the UPC/Store summary file (i.e. unique combinations)"))

	# calculate the minimum number of weeks to warrant inclusion for the data set
	if (par.upc.store.min.weeks > 0) {
		min.weeks = par.upc.store.min.weeks
	} else {
		min.weeks = as.integer(par.proportion.of.weeks * (1+ max(dush$start_week, na.rm = TRUE) - min(dush$start_week, na.rm = TRUE)))
	}
	
	# subset to the minimum number of weeks and the maximum number of CONSECUTIVE MISSING weeks
	dush = dush[num_weeks >= min.weeks & max_consecutive_missing <= par.max.consec.missing.periods ]
	print(paste(nrow(dush), " UPC/Store combinations after min.weeks and max.consec filtering"))
	
    # now obtain the top n UPCs in the subset based on revenue
    top.upc.revenue = dush[,list(revenue = sum(revenue)), by = "UPC"]
    top.upc.revenue = head(top.upc.revenue[with(top.upc.revenue, order(-revenue)), ]$UPC, par.upc.top.n)
    
    # return the sorted dataset of UPC/store combinations fitting the criteria
    return(dush[with(dush, order(-revenue)), ])
	
}


f_subset.chain.restrictions = function(skus.to.keep, par.min.num.stores.in.chain)
{
    ## now subset the SKU/Store combinations to only include the chains with a specified number of stores
    # we need to get the chain for each store and then count the stores per upc/chain combination
    stores = readRDS(paste0(pth.dropbox.data,"/iri reference data/stores.clean.rds"))
    skul.h = skus.to.keep
    skul.h = merge(skul.h, stores[,c(1:3), with =FALSE], by = "IRI_KEY")
    sku.chain = skul.h[, list(store.count = length(unique(IRI_KEY))) , by = c("UPC","chain")]
    sku.chain = sku.chain[store.count> par.min.num.stores.in.chain, c(1,2), with = FALSE]
    skus.to.keep = merge(skul.h, sku.chain, by = c("UPC","chain"))
    skus.to.keep
}


f_subset.transactions = function(pth.tf, pth.subset, par.category, skus.to.keep)
{
	# this is the function in use to subset the whole transformed category file, based on the SKUs in scope:
	# that is the UPCs in the top n sold for a sufficient number of periods
    # the function creates a new file containing only sales data for the required SKUs
    
	#fil.prefix = paste0(pth.tf, par.category, "/", par.category)  ; fil = paste0(fil.prefix, ".tf.all.rds")
    fil = paste0(pth.tf, par.category, ".tf.all.rds")
	dat.all = readRDS(fil)
	
    setkeyv(dat.all, c("UPC", "IRI_KEY", "WEEK"))
	print(head(skus.to.keep)) ; nrow(skus.to.keep)
	print(paste(nrow(dat.all), "rows in dat.all"))
	
	# subset the data.table for the relevant items  - removed thi step because SKU handles it
	#dat.all = dat.all[UPC %in% upc.top.n$UPC,]
	#print(paste(nrow(dat.all), "rows in dat.all"))
	
	# subset the data.table for SKUs in scope
	dat.all = dat.all[skus.to.keep[,list(UPC,IRI_KEY)]]
	print(paste(nrow(dat.all), "rows in dat.out"))
	
	
    # create the new directory if it doesn't already exist
	dir.create(file.path(pth.subset, par.category))
	pth.subset = paste0(pth.subset, par.category, "/")
	
    # save the filtered dataset to file
	fil = paste0(pth.subset, par.category, ".subset.rds") 
	print(fil) ; print(nrow(dat.all)) ; 	print(head(dat.all,20))
	saveRDS(dat.all, file=fil)
	nrow(dat.all)
	
}


f_subset.get.upc.stores = function(pth.upc, pth.agg,par.category,
                                   par.top.n.items = 10,
                                   par.upc.store.min.weeks = 300,
                                   par.upc.store.proportion.weeks = 0.9,
                                   par.upc.store.max.consec.missing.periods = 2,
                                   par.min.num.stores.in.chain = 3) {

	# load the item master listing and filter out private label UPCs
    upc = f_load.upc.category(pth.upc, par.category)
	
    # get the UPC/store combinations matching orequirements
	skus.to.keep = f_upc.store.horizon(pth.agg, par.category, par.top.n.items, par.upc.store.min.weeks, 
                                       par.upc.store.proportion.weeks, par.upc.store.max.consec.missing.periods)
    
    # now filter out any chains without sufficient stores
    skus.to.keep = f_subset.chain.restrictions(skus.to.keep = skus.to.keep, 
                                               par.min.num.stores.in.chain = par.min.num.stores.in.chain)
    
	setkeyv(skus.to.keep, c("UPC","IRI_KEY"))
	skus.to.keep	
}



########### MAIN ############


pth.upc = "/home/users/wellerm/IRI_DATA/iri reference data/upc/"
pth.tf = "/storage/users/wellerm/data/02_tf/sales/"
pth.agg = "/storage/users/wellerm/data/03_agg/"
pth.subset = "/storage/users/wellerm/data/04_subset/"


categories = c("beer", "carbbev", "milk")


#lapply(categories, f_subset.category.main)


TEST = TRUE

if (TEST == TRUE) {
    
    pth.upc = paste0(pth.dropbox.data, "iri reference data/upc/")
    pth.tf = paste0(pth.dropbox.data, "tf-test/")
    pth.agg = paste0(pth.dropbox.data, "iri category summaries/")
    pth.subset = paste0(pth.dropbox.data, "iri category subsets/unformatted/")
    
    # determine which items to keep in the subset
    skus.to.keep = f_subset.get.upc.stores(pth.upc, pth.agg, par.category = "razors", 
                                           par.top.n.items = 10, par.upc.store.min.weeks=200, par.upc.store.max.consec.missing.periods=2,
                                           par.min.num.stores.in.chain = 3)
    
    skus.to.keep[,list(revenue=sum(revenue), store_count=length(unique(IRI_KEY))),by=c("UPC","chain")]
    
    num.recs = f_subset.transactions(pth.tf, pth.subset, par.category, skus.to.keep)    
    upc = NULL; upc.top.n = NULL ; sku=NULL ; dat.subset=NULL; gc()
    num.recs
}

 ##### EOF ######



# 
# f_upc.top.n.horizon = function(par.category, upc, par.top.n.items, par.upc.start.week = 1, par.upc.end.week = 313 )
# {
#     
#     # function to get the UPCs which are the top sellers in terms of revenue within the category
# 	# the function will read the aggregated revenue per UPC over the whole horizon and return the Top n items
# 	
#     fil.prefix = paste0(pth.agg, par.category, "/", par.category)
# 	dupc = readRDS(paste0(fil.prefix,".dat.upc.horizon.rds"))
# 
# 	#dupc$UPC = factor(dupc$UPC)
# 	print(paste("Rows in dupc:",nrow(dupc)))
#     dupc <- dupc[with(dupc, order(-revenue)), ]
# 	print("TOP 10 ITEMS IN CATEGORY BASED ON SALES")
# 	print(head(dupc,10))
# 
# 	# filter the UPC listing to only include those with a specified start/end/number of weeks and which are in the master file
# 	dupc = dupc[start_week == par.upc.start.week & end_week == par.upc.end.week & UPC %in% as.character(upc$UPC) ,]  #
# 	dupc = droplevels(dupc)
#     
# 	print("TOP 10 ITEMS IN CATEGORY BASED ON SALES (only selling from weeks 1-313)")
# 	print(head(dupc,10))
# 
#     head(dupc, par.top.n.items)
# }
# 

if (machine == "M11") pth.dropbox = "C:/Users/Matt/Dropbox/HEC/IRI_DATA/"
if (machine == "DESKTOP") pth.dropbox = "D:/Dropbox/Dropbox/HEC/IRI_DATA/"
setwd(pth.dropbox)

load("./iri reference data/stores.raw.Rda")

stores = data.table(stores.raw)
setnames(stores,"store_id","IRI_KEY")
keycols = "IRI_KEY"
setkeyv(stores,keycols) 
stores = stores[IRI_KEY %in% droplevels(stores[,list(store_count=.N),by=IRI_KEY][store_count==1,IRI_KEY])]
f = droplevels(stores$IRI_KEY)   ;  stores$IRI_KEY = as.integer(levels(f))[f]
saveRDS(stores, "./iri reference data/stores.clean.rds")
	
	
	#load("./iri reference data/stores.raw.Rda")
	#stores = data.table(stores.raw)
	#stores.raw = NULL
	#sapply(stores, class)
	#keycols = "IRI_KEY"
	#setkeyv(stores,keycols) 
	?setnames
nrow(stores)

stores.instances = stores[,list(instances = .N),by=IRI_KEY]
head(stores)

hist(stores$count_years)
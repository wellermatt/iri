# this procedure will further subset the data based on taking the 

# we are looking to find the set of items and stores to satisfy our needs which are:

# correlation of promotions, importance of chain, 

library(data.table) ; library(ggplot2)  ; library(reshape2)

rm(list=ls())  ;  options(width=200)
machine = (Sys.info()["nodename"])

options(echo=TRUE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)
print(args)

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
#====================================================================
# Source functions & Load data
#====================================================================
par.category = "beer"
setwd(pth.dropbox.code)
source("./StaticData/ChainsMarketsStores.R")
stores = f_load.stores.raw()

setwd(paste(pth.dropbox.data, "iri category subsets/unformatted/", par.category,sep=""))

#f_wd.subsets.unformatted = function() setwd()
#f_wd.subsets.unformatted()

# item summary
skul.h = data.table(readRDS(paste(par.category,".subset.upc.store.horizon.rds", sep = "")))
sku.h = data.table(readRDS(paste(par.category,".subset.upc.horizon.rds", sep = "")))
names(sku.h)
print(sku.h)
ggplot(skul.h, aes(x=factor(max_consecutive_missing))) + geom_bar() + facet_wrap(~UPC) + ggtitle("Consecutive missing weeks: store count by SKU")

# item location - subsetting rules
stores
skul.h = merge(skul.h, stores[,c(1:3), with =FALSE], by = "IRI_KEY")

sku.chain = skul.h[, list(
	market.count = length(unique(market)),
	store.count = length(unique(IRI_KEY)),
	chain.revenue = sum(revenue)) , by = c("UPC","chain")]
sku.chain = sku.chain[store.count>3]
sku.chain[order(UPC,-chain.revenue)]


dcast(sku.chain, chain~UPC,sum, value.var = "chain.revenue")


# this needs to be tied together with the correlations within the chains

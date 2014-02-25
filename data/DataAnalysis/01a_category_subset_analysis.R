
# for the selected subset this will produce a summary table showing 

library("data.table") ; library("ggplot2") ; library("reshape2") ; library("scales")
rm(list=ls())
options(width=200)
machine = (Sys.info()["nodename"])

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

#============== load forecast items for all 3 categories ==================
setwd(pth.dropbox.data)
categories = c("beer", "carbbev", "milk")	

f_load.fc.items = function(category) data.table(category = category, readRDS(paste("./iri category subsets/reformatted/", category, ".subset.fc.items.rds", sep = "")))
fc.items = rbindlist(lapply(categories, f_load.fc.items))
fc.items[, lvl := factor(lvl,levels(lvl)[c(3,1,2)])]


#============= summary tables from subset(s) ==================

# table showing number of fc.items per level per category
dcast(data = fc.items, formula = category ~ lvl)

# table showing number of unique items/chains/stores per category
fc.items[lvl=="STORE",list(ITEMS = length(unique(UPC)),CHAINS = length(unique(chain)),STORES = length(unique(IRI_KEY))),by = list(category)]

# table showing number of unique chains/stores per item
fc.items[lvl=="STORE",list(CHAINS=length(unique(chain)),STORES=length(unique(IRI_KEY))),by = list(category,UPC,L9)]






store.counts.chain = fc.items[lvl=="STORE",list(length(unique(IRI_KEY))),by = list(category,chain)]
store.counts = fc.items[lvl=="STORE",list(length(unique(IRI_KEY))),by = list(category)]

chain.counts = fc.items[lvl=="CHAIN",.N,by = list(category,UPC)]


store.counts = fc.items[lvl=="STORE",.N,by = list(category,UPC,chain)]






# function for number of observations 
give.n <- function(x,y){
  return(c(y = 2, label = length(x))) #*1.05
  # experiment with the multiplier to find the perfect position
}

give.n <- function(x){
   return(c(y = 2, label = length(x)))
}

ggplot(data = store.counts, aes(x = UPC, y = N)) + geom_boxplot() + facet_grid(~category,scales = "free_x",space = "free", drop = TRUE) +
	stat_summary(fun.data = give.n, geom = "text", fun.y = median, size = 3, colour = "darkred") +
	geom_hline(yintercept = 4, colour = "darkgreen", linetype = "dotted", size = 1.0) +
	theme(text = element_text(size=11), axis.text.x = element_text(angle=-90, vjust=0.5, hjust=0)) +
	ggtitle ("Number of stores in a chain, per Item\n(number of chains displayed in red)\n") +
	labs(x="\nItem - UPC", y = "Number of stores per chain\n")
	

	
	
#==================== revenue distribution of chains =================================

#  need to run summary stats on all category subsets (milk/carbbev)
f_load.subset.summaries = function(category) data.table(category = category, readRDS(paste("./iri category subsets/unformatted/", category, "/", category, ".subset.upc.store.horizon.rds", sep = "")))
subset.summaries = rbindlist(lapply(categories, f_load.subset.summaries))
setkeyv(subset.summaries,c("category","IRI_KEY","UPC"))
fci.1 = fc.items[lvl=="STORE"]

setkeyv(fci.1, c("category","IRI_KEY", "UPC"))
subset.summaries = merge(subset.summaries,fci.1)
subset.summaries

qplot(data = subset.summaries, x=factor(313-num_weeks), geom = "bar") + facet_wrap(facets = ~category, ncol=1)
qplot(data = subset.summaries, x=factor(max_consecutive_missing), geom = "bar") + facet_wrap(facets = ~category, ncol=3)


chain.revenue.item = subset.summaries[lvl=="STORE",list(chain.revenue=sum(revenue)),by = list(category,UPC,chain)]

give.n <- function(x,y){
  return(c(y = -100, label = length(x))) #*1.05
  # experiment with the multiplier to find the perfect position
}

ggplot(data = chain.revenue.item, aes(x = UPC, y = chain.revenue/1000)) + geom_boxplot() + facet_grid(~category,scales = "free_x",space = "free", drop = TRUE) +
	stat_summary(fun.data = give.n, geom = "text", fun.y = median, size = 3, colour = "darkred") +
	#geom_hline(yintercept = 4, colour = "darkgreen", linetype = "dotted", size = 1.0) +
	theme(text = element_text(size=11), axis.text.x = element_text(angle=-90, vjust=0.5, hjust=0)) +
	ggtitle ("Revenue ($000) per chain, per Item\n(number of chains displayed in red)\n") +
	labs(x="\nItem - UPC", y = "Revenue ($000) per chain\n")
	


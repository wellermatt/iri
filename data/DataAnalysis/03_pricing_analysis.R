library(plyr) ; library(reshape2); library(ggplot2) ; library(data.table)

rm(list = ls())
options(width = 250)  ;  options(scipen=100)
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

#========================= LOAD DATA =============================
categories = c("beer", "carbbev", "milk")


setwd(pth.dropbox.data)

f_load.fc.items = function(category) data.table(category = category, readRDS(paste("./iri category subsets/reformatted/", category, ".subset.fc.items.rds", sep = "")))
fc.items = rbindlist(lapply(categories, f_load.fc.items))

f_load.subset = function(category) data.table(category = category, readRDS(paste("./iri category subsets/reformatted/", category, ".subset.sales.promos.weekly.rds", sep = "")))
subsets.w = rbindlist(lapply(categories, f_load.subset))

setkeyv(subsets.w,c("fc.item", "WEEK"))
fci = fc.items[,list(fc.item,lvl)]
setkeyv(fci,"fc.item")
subsets.w = merge(subsets.w,fci)

l3 = subsets.w[lvl==3]

ggplot(data=l3[category=="beer"], aes(x=WEEK, y = PRICE)) + geom_line() + facet_wrap(~UPC,ncol=3, scales = "free_y") + theme_bw()

	


tmp = readRDS("./iri category subsets/unformatted/carbbev/carbbev.subset.rds")
tmp[UNITS <= 0]
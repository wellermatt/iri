# procedure to analyse 

library("data.table") ; library("ggplot2") ; library("reshape2") ; library("scales")


#============== load data ==================
setwd(pth.dropbox.data)
categories = c("beer", "carbbev", "milk")	

f_load.fc.items = function(category) data.table(category = category, readRDS(paste0("./iri category subsets/reformatted/", category, ".subset.fc.items.rds")))
fc.items = rbindlist(lapply(categories, f_load.fc.items))

f_load.subset = function(category) data.table(category = category, readRDS(paste0("./iri category subsets/reformatted/", category, ".subset.sales.promos.weekly.rds")))
subsets = rbindlist(lapply(categories, f_load.subset))
setkeyv(subsets,c("fc.item","WEEK"))
setkeyv(fc.items, c("fc.item"))
ss = merge(subsets, fc.items[,list(fc.item,lvl,L9)], by ="fc.item")

# at the ITEM level, calculate the means for promo variables and melt
ss3 = ss[lvl == "ITEM"]  
sdcols = c("PRICE","PR", grep("FEAT_|DISP_",names(ss3),value=TRUE))
ss3.means = ss3[,lapply(.SD, mean,na.rm=TRUE),by = list(category,UPC,L9),.SDcols = sdcols]
ss3.melt = data.table(melt(ss3.means))[variable %in% c("PR","DISP_ANY","FEAT_ANY")]

# quick plot of the means of the promotional variables by item (aggregated to item level)
qplot(data=ss3.melt, x= value, y = L9, colour = variable, geom="point") + scale_x_continuous(limits = c(0,1)) +
    facet_grid(category~variable,scales="free_y",drop=TRUE,space="free") +
    theme_bw()







round(with(subsets, 100*prop.table(table(F,D, PR))), digits=2)
format(with(subsets, prop.table(table(F,D, PR))), digits=2)


round(with(subsets, 100*prop.table(table(F,D,category))), digits=2)

ggplot(data=subsets, aes(fill=factor(D), x = F)) + stat_bin() + geom_bar(position="stack") +
facet_wrap(~category)

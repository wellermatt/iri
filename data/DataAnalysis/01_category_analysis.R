# perfrpms analysis on the summary datasets for the whole category, look at category, item, store measures
# for a single category

library("data.table") ; library("ggplot2") ; library("reshape2") ; library("scales")
# rm(list=ls())
# options(width=200)
# machine = (Sys.info()["nodename"])
# 
# pth.dropbox = "/home/users/wellerm/"
# if (machine == "M11") pth.dropbox = "C:/Users/Matt/Dropbox/"
# if (machine == "DESKTOP") pth.dropbox = "D:/Dropbox/Dropbox/"
# if (machine == "IDEA-PC") pth.dropbox = "C:/Users/welle_000/Dropbox/"
# 
# pth.dropbox.data = paste(pth.dropbox, "HEC/IRI_DATA/", sep = "")
# pth.dropbox.code = paste(pth.dropbox, "HEC/Code/exp1.1/", sep = "")
# if (pth.dropbox == "/home/users/wellerm/") {
# 	pth.dropbox.data = paste(pth.dropbox, "IRI_DATA/", sep = "")
# 	pth.dropbox.code = paste(pth.dropbox, "projects/exp1.1/", sep = "")
# }
# 
# setwd(pth.dropbox.data)
#============================== LOAD DATA ===================================

cal = readRDS("./iri reference data/calendar/calendar.weekly.rds")
cal$Week = as.Date(cal$week_end_date)

categories = c("beer", "carbbev", "milk")

# summarisation and plotting of the category as a whole
fil = ".dat.category.week.rds"
f_load.cat = function(category) data.table(category = category, readRDS(paste("./iri category summaries/", category, fil, sep = "")))
cat.summary = rbindlist(lapply(categories, f_load.cat))

#cat.summary[,WEEK:=factor(WEEK, ordered=TRUE)]
cat.summary[,category:=factor(category)]
cat.summary[,n:=NULL]

csm = melt(data = cat.summary, id = c("category", "WEEK"))
csm = merge(csm, cal[,list(WEEK,Week), with = TRUE], by = "WEEK")
ggplot(data = csm, aes(x=Week, y = value)) + 
	geom_line(aes(colour = category), size =1) + facet_wrap(~variable, scales = "free") +  expand_limits(y=0) +
	scale_x_date(breaks = "1 year", minor_breaks = "1 month", labels=date_format("%Y")) +
	theme_bw() + theme(legend.position="bottom") +
	ggtitle("Category summaries over time for key variables\n")
	
# analysis of the detail in the category
fil = ".dat.upc.store.horizon.rds"
f_load.cat = function(category) data.table(category = category, readRDS(paste("./iri category summaries/", category, fil, sep = "")))
cat.detail = rbindlist(lapply(categories, f_load.cat))

names(cat.detail)
cat.detail[,missing_weeks:=(end_week-start_week-num_weeks+1)]

cat.summary.stats = 
	cat.detail[,list("store/sku (000)" = .N, 
					store = length(unique(IRI_KEY)),
					sku = length(unique(UPC)),
					"observations(m)" = sum(num_weeks)/1e6,
					"missing" = sum(missing_weeks)/1e6)
				, by=category]
				
write.csv(cat.summary.stats, file = "./summary stats/cat.summary.stats.csv", row.names =FALSE)

# sku analysis
head(cat.detail)

df.weekly.revenue.by.item = cat.detail[,
	j=list(
		tot.rev.000 = sum(revenue)/1000,
		weeks.span = max(end_week) - min(start_week) + 1),
	by=list(category,UPC)][,avg.weekly.revenue := tot.rev.000/weeks.span]


ggplot( data = df.weekly.revenue.by.item,aes(x=weeks.span,y=avg.weekly.revenue*1000, colour=category)) +
	geom_point(alpha =  0.5) + scale_y_log10() + facet_wrap(facets = ~category, ncol=1)

# perfrpms analysis on the summary datasets for the whole category, look at category, item, store measures
# for a single category

library("data.table") ; library("ggplot2") ; library("reshape2") ; library("scales")


#============================== LOAD DATA FUNCTIONS ===================================

f_get_cat_summary_data = function(categories = c("beer", "carbbev", "milk"))
{
  
  # load multiple categories from the category summaries (all data)
  setwd(pth.dropbox.data)
  fil = ".dat.category.week.rds"
  f_load.cat = function(category) data.table(category = category, 
                                             readRDS(paste0("./iri category summaries/", category, fil)))
  cat.summary = rbindlist(lapply(categories, f_load.cat))
  cat.summary[,category:=factor(category)]
  
  #cat.summary[,WEEK:=factor(WEEK, ordered=TRUE)]
  cat.summary[,n:=NULL]

  cat.summary
}

#f_get_cat_summary_data()

f_cat_summary.plot = function(all.data=NULL, cal = NULL)
{
  setwd(pth.dropbox.data)
  cal = readRDS("./iri reference data/calendar/calendar.weekly.rds")
  #print(head(cal))
  cal$Week = as.Date(cal$week_end_date)
  
  if (is.null(all.data)) all.data = f_get_cat_summary_data()
  
  #print(head(all.data))
  csm = melt(data = cat.summary, id = c("category", "WEEK"))
  csm = merge(csm, cal[,list(WEEK,Week), with = TRUE], by = "WEEK")
  p=ggplot(data = csm, aes(x=Week, y = value)) + 
    geom_line(aes(colour = category), size =1) + facet_wrap(~variable, scales = "free") +  expand_limits(y=0) +
    scale_x_date(breaks = "1 year", minor_breaks = "1 month", labels=date_format("%Y")) +
    theme_bw() + theme(legend.position="bottom") +
    ggtitle("Category summaries over time for key variables\n")
  p
}


## define the parameters for this procedure
args <- commandArgs(trailingOnly = TRUE)
print (args)


par.category = if (is.null(args[1])) "diapers" else args[1]
if (par.category == "all")
{
	the.files = list.files("/storage/users/wellerm/data/02_tf/sales/all", pattern = "*.tf.all.rds")
	categories = gsub(".tf.all.rds", "", the.files)
	print(categories)
	for(category in categories)	f_iri.category.summarise(category)

	
} else f_iri.category.summarise(par.category)




#f_cat_summary(cat.summary)






TEST=FALSE

# analysis of the detail in the category
if (TEST ==TRUE)
  {
  setwd(pth.dropbox.data)
  categories = c("beer","milk","carbbev")
  fil = ".dat.upc.store.horizon.rds"
  f_load.cat = function(category) data.table(category = category, readRDS(paste("./iri category summaries/", category, fil, sep = "")))
  cat.detail = rbindlist(lapply(categories, f_load.cat))
  
  cat.detail[units_sold>0 & revenue>0.1]
  
  # issue in data: units_sold = 0
  cat.detail[units_sold ==0, revenue]
  cat.detail = cat.detail[units_sold >=1]
  
  names(cat.detail)
  cat.detail[,missing_weeks:=(end_week-start_week-num_weeks+1)]
  
  # build a table summarising the full category
  cat.summary.stats = 
  	cat.detail[,list("store/sku (000)" = .N, 
                     "revenue (m)" = sum(revenue)/1e6,
  					stores = length(unique(IRI_KEY)),
  					items = length(unique(UPC)),
  					"observations (m)" = sum(num_weeks)/1e6,
  					"missing (m)" = sum(missing_weeks)/1e6)
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
}
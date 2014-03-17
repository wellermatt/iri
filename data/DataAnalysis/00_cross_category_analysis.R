

library(data.table) ; library(foreach)
#source("../scripts/memory_usage.R")

f_get.categories2 = function()
{
  setwd("C:/Users/welle_000/Documents/03_agg")
  categories = dir()[file.info(dir())$isdir]
  categories
}


library(foreach)
xcat.summary = foreach(category = f_get.categories2()) %do%
{
  setwd("C:/Users/welle_000/Documents/03_agg")
  
  dat.upc.store.horizon = readRDS(paste0(category,"/",category,".dat.upc.store.horizon.rds"))
  cat.summary.dat = dat.upc.store.horizon[,list(category = category,
                                                total_revenue_m = sum(revenue,na.rm=TRUE)/10e6,
                                                observations_m = sum(num_weeks, na.rm=TRUE)/10e6,
                                                items_unique = length(unique(UPC)),
                                                stores_unique = length(unique(IRI_KEY)),
                                                average_span = mean(num_weeks))]
  
  
}
xcat = rbindlist(xcat.summary)

library("ggplot2")
qplot(data=xcat, x = reorder(category,-total_revenue_m), y = total_revenue_m, geom="bar", stat="identity") + 
  coord_flip() + theme_bw()

xcat.revenue.week = foreach(category = f_get.categories2()) %do%
{
  setwd("C:/Users/welle_000/Documents/03_agg")
  
  dat.cat.rev.week = readRDS(paste0(category,"/",category,".dat.category.week.rds"))
  cat.rev.week = dat.cat.rev.week[,list(category = category,
                                                total_revenue_m = sum(revenue,na.rm=TRUE)/10e6),
                                     by = "WEEK"]
  
}
xcat.rev = rbindlist(xcat.revenue.week)

qplot(data=xcat.rev, x =  WEEK, y = total_revenue_m, colour = category,geom = "line") 


f_get.categories = function()
{
  the.files = list.files("/storage/users/wellerm/data/02_tf/sales/all", pattern = "*.tf.all.rds")
  categories = gsub(".tf.all.rds", "", the.files)
  categories
}
if (par.category == "all")
{
  
  for(category in categories)	f_iri.category.summarise(category)
  
  
} else f_iri.category.summarise(par.category)


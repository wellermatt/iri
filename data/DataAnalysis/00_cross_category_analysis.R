

library(data.table) ; library(foreach)
#source("../scripts/memory_usage.R")

## define the parameters for this procedure
args <- commandArgs(trailingOnly = TRUE)
print (args)


par.category = if (is.null(args[1])) "diapers" else args[1]
categories = c("beer","milk", "carbbev", "razors")

setwd(pth.dropbox.data)
setwd("./iri category summaries/")
xcat.summary = foreach(category = categories) %do%
{
  dat.upc.store.horizon = readRDS(paste0(category,"/",category,".dat.upc.store.horizon.rds"))
  cat.summary.dat = dat.upc.store.horizon[,list(category = category,
                                                total_revenue_m = sum(revenue,na.rm=TRUE)/10e6,
                                                observations_m = sum(num_weeks, na.rm=TRUE)/10e6,
                                                items_unique = length(unique(UPC)),
                                                stores_unique = length(unique(IRI_KEY)),
                                                average_span = mean(num_weeks))]
  
}
rbindlist(xcat.summary)


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


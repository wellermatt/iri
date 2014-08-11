library(reshape2)
library(data.table)
library(foreach)

library(doParallel)
#if (opt.dopar =="dopar") registerDoParallel(cores)
#library(doSNOW)

cores = 2
cl <- makeCluster(cores, outfile="")
registerDoParallel(cl)
print(paste("**** Cluster started with", cores, "cores"))

dtcomb <- function(...) {
    rbindlist(list(...))
}


categories = readRDS("~/data/iri reference data/categories.rds")$category

cat.promo.summary =
    foreach (category = categories,
             .combine='dtcomb', .multicombine=TRUE,
             .errorhandling = "remove",.verbose=TRUE,
             .packages=c("data.table", "reshape2")) %dopar%
    {
        dat = readRDS(paste0("/storage/users/wellerm/data/02_tf/sales/all/", category, ".tf.all.rds"))    
        #rbindlist(list(data.table(category = category, dimension = "FEAT", melt(prop.table(table(dat$F))*100)),
        #               data.table(category = category, dimension = "DISP", melt(prop.table(table(dat$D))*100)),
        #               data.table(category = category, dimension = "PR", melt(prop.table(table(dat$PR))*100))))
        data.table(x=nrow(dat))
    }

saveRDS(cat.promo.summary, "~/data/iri category summaries/cat.promo.summary.rds")

categories=c("beer","milk")
foreach (category = categories) %do% print(category)
stopCluster(cl)

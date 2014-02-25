# fc.item, model-family
# regression needs to be run for every forecast item.  Information to record per forecast item includes the following:
# 1. the selected variables per fc.item/model: variable/coefficient/p-value
# 2. the error measures for the fc.item/model: MAPE, MPE, MAE, MASE, RMSE, MdRAE (fc.item, 
# 3. the error distributions per fc.item/model: fcim, period, fc, act, err, rel err, abs err
# 4. elasticities per model variable


#====================================================================
# Parameters, libraries, file locations
#====================================================================
if (require("doParallel") == FALSE) {install.packages("doParallel") ; library("doParallel")  }
if (require("data.table") == FALSE) {install.packages("data.table") ; library("data.table")  }

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

setwd(pth.dropbox.code)
source("./ModelFitting/RegressionModelling/02_regression_functions_modelling.R")
source("./ModelFitting/RegressionModelling/03_regression_functions_diagnostics.R")
source("./GenericRoutines/useful_functions.R")
#====================================================================
# Experiment options
#====================================================================
# set up the data.tables used to collect the data in the standard environment
print.options = list(opt.print.summary = TRUE, opt.print.aov = TRUE, opt.print.diag = TRUE, opt.print.stats = TRUE, opt.print.coef = TRUE)
expt.design.master = data.table(id = 1:3, include.AR.terms = FALSE, log.model = FALSE, price.terms = c("PRICE_DIFF","PRICE","PRICE_LAG"), time.period = "WEEK")
expt.design = as.list(expt.design.master[2])
for  (x in names(expt.design)) assign(x, expt.design[[x]])
	
#====================================================================
# Set up parallel environment
#====================================================================


# prepare the parallel environment
registerDoParallel(8)

#sp = sp[!is.infinite(PRICE)]
#saveRDS(sp, fil)
#saveRDS(sp[1:3130], "./regression datasets/beer.regression.data.weekly.small.rds")

#====================================================================
# Experiment Run in parallel environment
#====================================================================

#f_run.regressions = function(par.periodicity, par.category) {
par.category = "carbbev" ; par.periodicity = "weekly"	
	# get the input data for the regressions
	setwd(pth.dropbox.data)
	fil = paste("./regression datasets/", par.category, ".regression.data.", par.periodicity, ".rds", sep= "")
	#TESTING fil = paste("./regression datasets/", par.category, ".regression.data.", par.periodicity, ".TEST.rds", sep= "")
	sp = readRDS(fil)
	#TESTING fil = paste("./regression datasets/", par.category, ".regression.data.", par.periodicity, ".TEST.rds", sep= "")
	#TESTING saveRDS(sp[1:720],fil)
	sp[,fc.item:=factor(fc.item)]
	setkey(sp, fc.item)
	
	expt.coef.all = foreach(dt.sub = isplitDT(sp, levels(sp$fc.item)),
						.combine='dtcomb', .multicombine=TRUE,
						.packages=c("data.table", "MASS")) 	%dopar% 
	{	
		
		fc.item = dt.sub$key[1]
		model.best = f_ts.regression.auto.step(dt.sub$value)
		expt.coef = f_ts.diag.coef.table(model.best, opt.elasticity = TRUE)
		expt.coef
	}

	#expt.coef.all
	fil = paste("./output/", par.category, "_", par.periodicity, ".regression.coef.rds", sep = "")
	saveRDS(expt.coef.all, fil)
#}

#  main loop

#categories = c("beer","carbbev","milk") # #args[1] #"carbbev"
#f_run.regressions("weekly","milk")

closeAllConnections() 

#rm(list=ls())
#gc()




#writeLines(c(""), "E:/testing/log.txt")
	# sink("E:/testing/log.txt", append=TRUE)
	# cat(paste("This instance",head(dt$value),"\n"))
	# cat(paste("Objects in ",ls(),"\n"))

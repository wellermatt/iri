### ROLLING REGRESSION FUNCTIONALITY TESTING & LOOPING ####
setwd("C:/Users/matt/Dropbox/HEC/Code/iri")
#setwd(pth.dropbox.code)
#rm(list=ls())
source("./.Rprofile")

require("forecast")
require("data.table") ; require("reshape2")
require("ggplot2")
require("foreach")

setwd(pth.dropbox.code) ; source("./data/DataAdaptor/00_data_adaptor_test.R")
#setwd(pth.dropbox.code) ; source("./model/ModelFitting/ets/ets_functions.R")
setwd(pth.dropbox.code) ; source("./model/ModelFitting/RegressionModelling/02_regression_functions_modelling.R")
setwd(pth.dropbox.code) ; source("./model/ModelFitting/RegressionModelling/10_regression_roll_functions.R")
source("./model/ModelFitting/RegressionModelling/03_regression_functions_diagnostics.R")
source("./other/GenericRoutines/useful_functions.R")


#============== DATA LOADING =====================
# get the necessary data for a specific item: weekly or monthly data loaded
spw = f_da.reg.cat.test(par.category="beer", par.periodicity="weekly")   # spw is the regression dataset, all nodes
spm = f_da.reg.cat.test(par.category="beer", par.periodicity="445")     # spm is 445 version of above


#items = spw[!is.na(IRI_KEY),as.character(unique(fc.item))]
items = spm[,as.character(unique(fc.item))]

print.options = list(opt.print.summary = TRUE, opt.print.aov = TRUE, opt.print.diag = TRUE, opt.print.stats = TRUE, opt.print.coef = TRUE)
expt.design.master = data.table(id = 1:3, include.AR.terms = FALSE, log.model = FALSE, price.terms = c("PRICE_DIFF","PRICE","PRICE_LAG"), time.period = "WEEK")
expt.design = as.list(expt.design.master[2])
for  (x in names(expt.design)) assign(x, expt.design[[x]])

##
# main functions: take a model (), get the xreg : the x values for future periods



# major decisions here about the paramters in terms of variable selection:
# price: diff/lag/log
# promo: lag/split/combination (FEAT/DISP)

# hols:

# model.config
log.model = FALSE
include.AR.terms = FALSE 
price.terms = "PRICE_DIFF"

periodicity = "weekly"




## SET UP MULTICORE
#determine which OS (Windows/Linux)


# for each fc.item (at each level)
# the list of items will be used

rr = f_reg.roll.multi()

saveResults = TRUE
if (saveResults == TRUE){
  
  setwd(pth.dropbox.data)
  saveRDS(results.all, "./output/errors/errors_reg2.rds")
  setwd(pth.dropbox.code)
  
}

library(stringr)
rr[,lvl := str_count( fc.item, "/")+1 ]
rr[,list(mape = mean(abs(re),na.rm=TRUE)),by=list(lvl,k)]

dcast(data = rr,k~lvl,fun.aggregate=median,na.rm=TRUE,value.var="rae")
dcast(data = rr[lvl==2],fc.item~k,fun.aggregate=median,na.rm=TRUE,value.var="rae")

# results[,list(mape = mean(abs(re))),by=k]
# 

qplot(data=rr, x = re) + facet_wrap(facets = ~lvl,ncol=1, scales = "free")
ggplot(data= rr, aes(x = re, colour = factor(k))) +  geom_density() + facet_wrap(facets = ~lvl, scales = "free")
ggplot(data= rr, aes(y = rae, x = factor(lvl), fill = factor(lvl))) +  geom_boxplot() + coord_flip()


# 
# pacf(mm$residuals)
# 
# boxplot(results$re~results$k)
# 
# summary(mm)
# plot(mm)


# 
# test.reg.roll = function(mod.reg, xreg, h)
# {
# 
#     # takes the model and the xreg args and does an h-step ahead forecast
#     
#     foreach (o = 300:(end.week-1), 
#              .combine='dtcomb', .multicombine=TRUE) %do%          #(fit.week.end)  #(end.week-1)
# {          
#     print(o)
# 
#     # build the xreg variables, based on the variables in formula!!        
#     xregnew = ssw[WEEK %in% (o+1):(o+h), eval(model.vars), with = F]
#     
#     #fit.model = f_ts.regression.auto.stepAIC(ssw[1:o])  # window.t = 1:198
#     revised.model = lm(formula = frm.text, data=ssw[1:o])
#     
#     fc = predict.lm(object=revised.model,newdata=xregnew)
#     #             fc = forecast(revised.model,
#     #                           h=min(h, end.week-o), 
#     #                           newdata = data.frame(xregnew))
#     
#     act = ssw[WEEK %in% c(o+1:(o+min(h, end.week-o))),UNITS]
#     
#     fc.comparison = data.table(t = o, 
#                                k = 1:min(h, end.week-o),
#                                fc = fc, act)
#     fc.comparison[,`:=`(e=fc-act, re = (fc-act)/act)]   
#     print(fc.comparison)
#     fc.comparison
# }
#     
# }
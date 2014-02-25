### ROLLING REGRESSION FUNCTIONALITY ####



rm(list=ls())
getwd()
#setwd()
source('D:/Dropbox/Dropbox/HEC/Code/iri/.Rprofile')


library("forecast") ; library("xts") ; library("data.table") ; library("reshape2")
library("ggplot2")
require("foreach")

setwd(pth.dropbox.code) ; source("./data/DataAdaptor/00_data_adaptor_test.R")
#setwd(pth.dropbox.code) ; source("./model/ModelFitting/ets/ets_functions.R")
setwd(pth.dropbox.code) ; source("./model/ModelFitting/RegressionModelling/02_regression_functions_modelling.R")
setwd(pth.dropbox.code) ; source("./model/ModelFitting/RegressionModelling/10_regression_roll_functions.R")
source("./model/ModelFitting/RegressionModelling/03_regression_functions_diagnostics.R")
source("./other/GenericRoutines/useful_functions.R")

#============== DATA LOADING =====================

# get the necessary data for a specific item
spw = f_da.reg.cat.test(par.category="beer", par.periodicity="weekly")   # spw is the regression dataset, all nodes
spm = f_da.reg.cat.test(par.category="beer", par.periodicity="445")     # spm is 445 version of above

items = spw[!is.na(IRI_KEY),as.character(unique(fc.item))]
items = spm[,as.character(unique(fc.item))]

print.options = list(opt.print.summary = TRUE, opt.print.aov = TRUE, opt.print.diag = TRUE, opt.print.stats = TRUE, opt.print.coef = TRUE)
expt.design.master = data.table(id = 1:3, include.AR.terms = FALSE, log.model = FALSE, price.terms = c("PRICE_DIFF","PRICE","PRICE_LAG"), time.period = "WEEK")
expt.design = as.list(expt.design.master[2])
for  (x in names(expt.design)) assign(x, expt.design[[x]])

##
# main functions: take a model (), get the xreg : the x values for future periods

require(forecast)

names(spw)[1:10]



# major decisions here about the paramters in terms of variable selection:
# price: diff/lag/log
# promo: lag/split/combination (FEAT/DISP)

# hols:


log.model = FALSE
include.AR.terms = FALSE 
price.terms = "PRICE_DIFF"


foreach (i = 1:3) %do%
{
    item = items[i] # as.character(spw$fc.item[[i]])
    ssw = spw[fc.item == item]
    #id <<- i
    fc.item <<- item
    
    reg.roll = f_reg.roll(ssw)    
    
    mm = reg.roll$final.model
    results = reg.roll$roll.stats
    
    results[,list(mape = mean(abs(re))),by=k]
    
    qplot(data=results, x = re,facets = ~k)
    ggplot(data= results, aes(x = re, colour = factor(k))) +  geom_density()
    ggplot(data= results, aes(y = re, x = factor(k), fill = factor(k))) +  geom_boxplot() + coord_flip()
    
    pacf(mm$residuals)
    
    boxplot(results$re~results$k)
    
    results
}

results


summary(mm)
plot(mm)


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
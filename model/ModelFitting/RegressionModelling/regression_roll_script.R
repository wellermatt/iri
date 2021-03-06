#rm(list=ls())
### ROLLING REGRESSION FUNCTIONALITY TESTING & LOOPING ####
if (!exists("machine")) {
    if (.Platform$OS.type == "unix") {
        source("~/projects/iri/.Rprofile")
    } else {
        source("E:/Git/iri/.Rprofile") #source("~/projects/iri/.Rprofile") #else source("E:/Git/iri/.Rprofile")
    }
}

require("forecast") ; require("data.table") ; require("reshape2")
require("ggplot2") ; require("foreach") ; require("xtable")

setwd(pth.dropbox.code) ; source("./model/ModelFitting/RegressionModelling/02_regression_functions_modelling.R")
setwd(pth.dropbox.code) ; source("./model/ModelFitting/RegressionModelling/regression_roll_functions.R")
setwd(pth.dropbox.code) ; source("./model/ModelFitting/RegressionModelling/03_regression_functions_diagnostics.R")
setwd(pth.dropbox.code) ; source("./other/GenericRoutines/useful_functions.R")
setwd(pth.dropbox.code) ; source("./data/DataAdaptor/00_data_adaptor_test.R")
setwd(pth.dropbox.code) ; source("./data/DataAdaptor/10_load_data_various.R")


#=============== EXPT DESIGN - MODEL SELECTION PARS =========================
log.model = FALSE
include.AR.terms = FALSE 
price.terms = "PRICE_DIFF"
#categories = c("beer","carbbev","milk")

# default parameter values
par.category = "beer"
par.upc =   "00-01-18200-53030"      # NULL   #  
par.fc.item = NULL # 00-01-41383-09036/12#  NULL # "00-02-28000-24610/99"   #NULL #"00-01-18200-53030/104/228694" # NULL# "00-01-18200-53030/57" #"00-01-18200-53030/104/228694"
freq = 52
freq.cycle = 52
Level = 3
cores = 6
TRACE = 0

# custom parameter values
args <- commandArgs(trailingOnly = TRUE)
print (args)
if (length(args) > 0) for(i in 1:length(args)) eval(parse(text=args[[i]]))

    #par.category = args[1]
    #freq = as.integer(args[2])
    #freq.cycle = as.integer(args[3])
    ##h.max = as.integer(args[4])
    #Level = as.integer(args[4])
    #par.upc = args[5] ; if(par.upc == "NULL") par.upc = NULL
    #par.fc.item = args[6] ; if(par.fc.item == "NULL") par.fc.item = NULL
    #cores = as.integer(args[7])

h.max = if(freq == 52) 13 else 3    # maximum length of the horizon
par.periodicity = if (freq == 52) "weekly" else "445"
print(ls())

print.options = list(opt.print.summary = TRUE, opt.print.aov = FALSE, opt.print.diag = FALSE, opt.print.stats = TRUE, opt.print.coef = FALSE)
expt.design.master = data.table(id = 1:3, include.AR.terms = FALSE, log.model = FALSE, 
                                price.terms = c("PRICE_DIFF","PRICE","PRICE_LAG"), time.period = "WEEK")
expt.design = as.list(expt.design.master[2])
for  (x in names(expt.design)) assign(x, expt.design[[x]])

#============== DATA LOADING =====================
# use the standard beer SKU
sp = f_adaptor.reg.cat.all (par.category=par.category, par.periodicity=par.periodicity,
                            par.Level = Level, par.univariate = FALSE, 
                            par.upc = par.upc, par.fc.item = par.fc.item)   # spw is the regression dataset, all nodes    

print(paste(length(unique(sp$fc.item))," items and", nrow(sp), " data points"))

# ========= TESTING ===============
this.time = system.time(reg.roll <- f_reg.roll.multiCORE(sp = sp,  par.category = par.category,  par.periodicity=par.periodicity, 
                                                         freq=freq, freq.cycle = freq.cycle,
                                                         h.max = h.max,  cores = cores) )

reg.roll
reg.roll[,.N, by=fc.item]

#dcast(reg.roll[fc.item %in% reg.roll[,.N,by=fc.item][N<300,fc.item]],fc.item~o)
#dcast(reg.roll[fc.item %in% reg.roll[,.N,by=fc.item][N<300,fc.item]],fc.item~t)


# create a file name and save the results
saveResults = TRUE
if (saveResults == TRUE){
    
    setwd(paste0(pth.dropbox.data,"/output/errors/")  )
    if (platform=="windows") setwd("E:/data/errors/")
    fil = paste0(paste("reg", freq, freq.cycle, Level, par.category, par.upc,sep="_"), ".rds")
    saveRDS(object = reg.roll, file = fil)    
}

print("======== OUTPUT FROM reg.roll =======")
run.summary = data.table(method = "reg", freq, freq.cycle, h.max, 
                        par.category, Level, par.upc =  nz(par.upc), par.fc.item = nz(par.fc.item), 
                        item_count = length(unique(sp$fc.item)), cores = cores, this.time = this.time[3])
print(run.summary)






#==========================================================================



#rr=reg.roll


##
# main functions: take a model (), get the xreg : the x values for future periods
# major decisions here about the paramters in terms of variable selection:
# price: diff/lag/log
# promo: lag/split/combination (FEAT/DISP)

# hols:
# model.config

# function to get the data for multiple categories
# if (TRUE == FALSE)
# {
#     sp.all = lapply(categories, function (this.cat) f_da.reg.cat.all(par.category=this.cat, par.periodicity="weekly") )  # spw is the regression dataset, all nodes    
#     sp = rbindlist(sp.all)
# }
# 


# blah blah
# 
# library(stringr)
# rr[,lvl := str_count( fc.item, "/")+1 ]
# rr[,list(mape = mean(abs(re),na.rm=TRUE)),by=list(lvl,k)]
# rr=reg.roll
# dcast(data = rr,h~lvl,fun.aggregate=median,na.rm=TRUE,value.var="ape")
# dcast(data = rr,lvl+fc.item~k,
#       fun.aggregate=median,na.rm=TRUE,value.var="rae")
# 
# # results[,list(mape = mean(abs(re))),by=k]
# # 
# 
# qplot(data=rr, x = re) + facet_wrap(facets = ~lvl,ncol=3, scales = "free")
# ggplot(data= rr, aes(x = re, colour = factor(k))) +  geom_density() + facet_wrap(facets = ~lvl, scales = "free")
# ggplot(data= rr, aes(y = rae, x = factor(lvl), fill = factor(lvl))) +  geom_boxplot() + coord_flip()
# 
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



# 
# # get the necessary data for a specific item: weekly or monthly data loaded
# if (par.periodicity == "weekly") {
#     sp = f_da.reg.cat.all(par.category="beer", par.periodicity="weekly")   # spw is the regression dataset, all nodes    
# } else {
#     sp = f_da.reg.cat.test(par.category="beer", par.periodicity="445")     # spm is 445 version of above    
# }

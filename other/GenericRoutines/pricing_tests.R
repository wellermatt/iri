
setwd("C:/Users/Matt/Dropbox/HEC/code/exp1.1")
rm(list=ls())
library("ggplot2")
source("./config.R")
setwd(pth.dropbox.code)  ; source("./GenericRoutines/f_pricing_functions.R")
setwd(pth.dropbox.code)  ; source("./DataAdaptor/00_data_adaptor_test.R")


spw = f_da.reg.cat.all(par.category="beer",par.periodicity = "weekly") ; gc()
spw.store = spw[!is.na(IRI_KEY),list(UPC,chain,IRI_KEY,fc.item,WEEK,PRICE,PR)]  #, with = FALSE
spw = NULL  ;  gc()  ;  tables()

spw.store[,f_reference.prices_KM(PRICE,PR),by = fc.item]

# examine chains with fractional average prices
price.precision = spw.store[,list(chain, IRI_KEY, WEEK, PRICE, rounded.price = round(PRICE,2), deviation = PRICE - round(PRICE,2), rounded.TF = (PRICE != round(PRICE,2)))]
ggplot(data=price.precision, aes(x = chain, fill = rounded.TF)) + geom_bar(position = "fill") + theme_bw() + coord_flip()
ggplot(data=price.precision[deviation!=0], aes(x = chain, y=100*deviation)) + geom_boxplot() + theme_bw() + coord_flip()
ggplot(data=price.precision[deviation!=0], aes(x=100*deviation)) + geom_histogram() + facet_wrap(~chain)
ggplot(data=price.precision[deviation!=0]) + geom_density( aes(x=deviation)) + facet_wrap(~chain)

price.precision[deviation != 0]




spw = f_da.reg.cat.test(par.category="beer",par.periodicity = "weekly")
spw.store = droplevels(spw[!is.na(IRI_KEY)])

spw.store[,PRICE:=round(PRICE,2)]
prices = data.table(dcast(spw.store, WEEK~IRI_KEY, value.var = "PRICE", fun.aggregate = sum))  #, na.rm=TRUE
prices

PR.flag = data.table(dcast(spw.store, WEEK~IRI_KEY, value.var = "PR", fun.aggregate = sum))  # , na.rm=TRUE
PR.flag

i = 20
dpv = data.table(WEEK = prices[[1]], price = prices[[i]], PR = PR.flag[[i]], PR.price = prices[[i]]*PR.flag[[i]])




ggplot(data = dpv, aes(x=WEEK,y=price)) + geom_line() + geom_point(aes(colour = factor(PR)))




prices2 = rbindlist(lapply(21:21,function(i)data.table(IRI_KEY = names(prices)[i], f_reference.prices_KM(prices[[i]], PR.flag[[i]]))))

tail(prices2,100)


#prices2 = f_reference.prices(prices[[2]])
#prices2$period= as.integer(rownames(prices2))
prices2.melt = data.table(melt(prices2, id.vars = c("IRI_KEY","period")))
library(ggplot2)
ggplot(data=prices2.melt, aes(x=period,y=value)) + 
	geom_line(aes(colour=variable)) +
	facet_wrap(~IRI_KEY)
	
	
ggplot(data=prices2.melt[variable=="pt"], aes(x=period,y=log(value))) + 
	geom_line(aes(colour=variable)) +
	facet_wrap(~IRI_KEY)
	
	
	
	tbl.txt = "
	Hydrocarbons	T0	2days	5days	8days	12days	16days	21days
    C12-C16b	 	255	210	134	91	64	41	189
    C17-C25b		857	707	428	405	322	208	708
    C26-C32b		232	193	139	122	92	57	165
    C33-C37b	          84	79	72	63	61	32	84"

	tbl = read.table(text=tbl.txt,header=TRUE)
	tbl.melt = melt(tbl, id.vars="Hydrocarbons", variable.name = "measured.at", value.name = "measurement")
	tbl.melt
	
	names(tbl)
	stack(tbl, select = c("T0", "X2days", "X5days", "X8days"))
head(dat.item)


head(sp)




f_data.aggregate.save.old = function(dat.item, promo.flags.agg.methods = "wmean")
{
	##############################################################################
	### PREPARE WEEKLY RAW DATA AND AGGREGATE ALL VARIABLES TO CHAIN/UPC LEVELS
	##############################################################################
		
	# initially we need to get a data.table containing every SKU (fc.item at the lowest level) for each week in the set
	fi1 = data.table(expand.grid(fc.item = fc.item[lvl==1, fc.item],
								WEEK= min(dat.item$WEEK):max(dat.item$WEEK)),key="fc.item,WEEK")
  
	### Firstly aggregate Sales and Price Data (UNITS, DOLLARS, PR, PRICE) 
	sales.pos = dat.item[,list(fc.item,WEEK,UNITS,DOLLARS,PR)]
	#sales.pos$PR = as.integer(sales.pos$PR)-1
	sales.pos$PRICE = sales.pos$DOLLARS/sales.pos$UNITS
	sales.pos = merge(fi1,
					sales.pos,
					all.x=TRUE,)
	sales.pos = merge(sales.pos,
					fc.item[lvl==1,list(fc.item,UPC,chain)],
					by="fc.item")
  
	###AGGREGATE THE SALES DATA TO NEXT LEVEL
	# note that weighted mean is used on the PR flag here as opposed to mean
	DT = sales.pos[, list(UNITS = sum(UNITS,na.rm=TRUE),
						DOLLARS = sum(DOLLARS, na.rm = TRUE),
						PR = weighted.mean(PR,w=UNITS,na.rm=TRUE)  ),    #########  weighted mean or mean?
				 by="UPC,chain,WEEK"]

	DT$PRICE = DT$DOLLARS/DT$UNITS
	DT$fc.item = with(DT,paste(UPC,chain,sep="/"))
	setcolorder(DT, names(DT)[c(length(DT),3:(length(DT)-1),1:2)])
	DT$UPC = NULL ; DT$chain = NULL
	sales.chain = DT ; DT = NULL
	head(sales.chain,100)
	## Aggregate sales to the ITEM level
	DT = sales.pos[, list(UNITS = sum(UNITS,na.rm=TRUE),
						DOLLARS = sum(DOLLARS, na.rm = TRUE),
						PR = weighted.mean(PR,w=UNITS,na.rm=TRUE)),    ############ weighted mean or mean?
					by="UPC,WEEK"]
	DT$PRICE = DT$DOLLARS/DT$UNITS
	DT$fc.item = as.character(DT$UPC)
	setcolorder(DT, names(DT)[c(length(DT),2:(length(DT)-1),1)])
	DT$UPC = NULL
	sales.upc = DT ; DT = NULL

	sales.pos$UPC = NULL ; sales.pos$chain=NULL   # clear up the extra columns
	sales = rbindlist(list(sales.pos,sales.chain, sales.upc))
	keycols = c("fc.item","WEEK")
	setkeyv(sales,keycols)
  
	# set UNITS and DOLLARS to NA from zero
	sales[UNITS ==0,c("UNITS","DOLLARS"):= NA]
  
	##################### PROMOTIONS FLAG TRANSFORMATION
	# FEATURE
	promos.pos.feat = data.table(cast(dat.item, fc.item + WEEK ~ F, value="counter", sum))
	setnames(promos.pos.feat ,
		   old=names(promos.pos.feat),
		   new= c("fc.item","WEEK",paste("FEAT_",names(promos.pos.feat)[3:length(promos.pos.feat)],sep="")))
	promos.pos.feat$FEAT_ANY = abs(promos.pos.feat$FEAT_NONE-1)
	keycols = c("fc.item","WEEK")
	setkeyv(promos.pos.feat,keycols)

	promos.pos.feat = merge(fi1,   #data.table(expand.grid(fc.item=fc.item[lvl==1,fc.item],WEEK= 1:313),key="fc.item,WEEK")
						  promos.pos.feat,all.x=TRUE)

	#promos.pos.feat = merge(fc.item.weeks.all,promos.pos.feat,all.x=TRUE)
	# DISPLAY
	promos.pos.disp = data.table(cast(dat.item, fc.item + WEEK ~ D, value = "counter", sum))
	setnames(promos.pos.disp ,
		   old=names(promos.pos.disp),
		   new= c("fc.item","WEEK",paste("DISP_",names(promos.pos.disp)[3:length(promos.pos.disp)],sep="")))
	keycols = c("fc.item","WEEK")
	setkeyv(promos.pos.disp,keycols) 
	promos.pos.disp = merge(fi1,
						  promos.pos.disp,
						  all.x=TRUE)
	# this is the key table
	promos.pos = merge(promos.pos.feat, promos.pos.disp)

	###### aggregate chain level promos data
	promos.pos.2 = merge(fc.item, promos.pos, by="fc.item")
	promos.pos.2 = merge(promos.pos.2, dat.item[,list(fc.item,WEEK,UNITS)], by=c("fc.item","WEEK"),all.x=TRUE)
	DT = promos.pos.2	
	if (promo.flags.agg.methods == "wmean"){  
		# using weighted mean
		DT = DT[, lapply(.SD, weighted.mean, w=UNITS, na.rm=TRUE), by="UPC,chain,WEEK", .SDcols=7:(length(DT))]
	} else {
		# using mean
		DT = DT[, lapply(.SD, mean, na.rm=TRUE), by="UPC,chain,WEEK", .SDcols=7:(length(DT)-1)]
	}
	DT$fc.item = with(DT,paste(UPC,chain,sep="/"))
	setcolorder(DT, names(DT)[c(length(DT),3:(length(DT)-1),1:2)])
	DT$UPC = NULL ; DT$chain = NULL ; DT$UNITS = NULL
	promos.chain = DT ; DT = NULL

	###### aggregate upc level promos data
	DT=promos.pos.2
	if (promo.flags.agg.methods == "wmean"){  
		DT = DT[, lapply(.SD, mean, na.rm=TRUE), by="UPC,WEEK", .SDcols=7:(length(promos.pos.2)-1)]
	} else {
		DT = DT[, lapply(.SD, weighted.mean, w=UNITS, na.rm=TRUE), by="UPC,WEEK", .SDcols=7:(length(promos.pos.2)-1)] }

	DT$fc.item = as.character(DT$UPC)  #with(DT,paste(UPC,chain,sep="/"))
	setcolorder(DT, names(DT)[c(length(DT),2:(length(DT)-1),1)])
	DT$UPC = NULL #; DT$UNITS = NULL

	promos.upc = DT ; DT = NULL
	promos = rbind(promos.pos,promos.chain,promos.upc)
	keycols = c("fc.item","WEEK")
	setkeyv(promos,keycols)

	sales.promos = merge(sales,promos)

  getwd()
  #sapply(promos.pos,class)
  #sapply(promos.upc,class)
  #sapply(promos,class)
  #sapply(sales,class)
  
  
  setwd("./iri category subsets/reformatted")
  #saveRDS(promos,"beer.top.upc.promos.rds")
  #saveRDS(sales,"beer.top.upc.sales.rds")
  saveRDS(sales.promos,"beer.top.upc.sales.promos.rds")
  #write.csv(sales.promos, file="beer.top.upc.sales.promos2.csv")
	sales.promos
  
  
}
rm(list=ls())
dt = data.table(grp = sample(letters[1:3],100, replace = TRUE),
                    v1 = rnorm(100), 
                    v2 = rnorm(100), 
                    v3 = rnorm(100))
    sd.cols = c("v1","v2", "v3")
    dt.out = dt[, list(sum(v1), lapply(.SD,mean)), by = grp, .SDcols = sd.cols]



dt = data.table(grp = sample(letters[1:3],100, replace = TRUE),
				v1 = rnorm(100), 
				v2 = rnorm(100), 
				v3 = rnorm(100))
sd.cols = c("v2", "v3")
dt.out = dt[, c(sum(v1), lapply(.SD,mean)), by = grp, .SDcols = sd.cols]

dt.out1 = dt[, sum(v1), by = grp]
dt.out2 = dt[, lapply(.SD,mean), by = grp, .SDcols = sd.cols]
dt.out = merge(dt.out1, dt.out2, by = "grp")
#dupes = DT[duplicated(DT$IRI_KEY)]
	#dupes[order(-rank(IRI_KEY)),]
	#stores.old = stores
	
	#DT = data.table(read.csv("./iri reference data/stores.csv",header=TRUE,stringsAsFactors=FALSE))
	#DT = unique(DT)
	#if (sum(duplicated(DT)) > 0) {
		#dupes = unique(DT[duplicated(DT$IRI_KEY)])[,IRI_KEY]
		#stores = DT[!DT$IRI_KEY %in% dupes]  
	#} else {
		#stores = DT
	#}	
	
	#droplevels(stores)

	
	
	## testing performance of cast versus model.matrix
	#ptm <- proc.time()	
		#promos.pos.feat = data.table(cast(dat.item, fc.item + WEEK ~ F, value="counter", sum))
	#setnames(promos.pos.feat ,
		   #old=names(promos.pos.feat),
		   #new= c("fc.item","WEEK",paste("FEAT_",names(promos.pos.feat)[3:length(promos.pos.feat)],sep="")))
	#promos.pos.feat$FEAT_ANY = abs(promos.pos.feat$FEAT_NONE-1)
	#keycols = c("fc.item","WEEK")
	#setkeyv(promos.pos.feat,keycols)
	#proc.time() - ptm
	#
	#?model.matrix
	#library(caret)
	#
	#
	#feat = 
	#td = dat.item
	#ptm <- proc.time()	
		#promos.pos.feat = data.table(cast(td, fc.item + WEEK ~ F, value="counter", sum))
	#proc.time() - ptm
	#ptm <- proc.time()	
		#promos.pos.feat = data.table(cast(td, fc.item + WEEK ~ F, length))
	#proc.time() - ptm
	#ptm <- proc.time()	
		#promos.pos.all = data.table(model.matrix(~F+D-1,data = td))
		#names(promos.pos.feat)
	#proc.time() - ptm
	#ptm = proc.time()
	#dat.item$D = factor(dat.item$D,labels = c("NONE", "MINOR", "MAJOR"))
	#dat.item$F = factor(dat.item$F)
	#setnames(dat.item,old = c("D","F"),new = c("DISP_","FEAT_"))
	#dummies = dummyVars(UNITS~D+F, data=dat.item)
	#dummies = data.table(head(predict(dummies,newdata=dat.item)))
	#proc.time()-ptm
	#
	#?model.matrix
	#
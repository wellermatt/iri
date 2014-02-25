library(Hmisc)


#=========================================================================================
#  correlations between stores per item, per chain - all promo & price variables
#=========================================================================================

# the correlation analysis for a category needs to be run at levels 1 and levels 2
# level 1 will look at the correlations within a chain for each variable

f_cor.within.chains = function() {
	
	upc.chains = unique(fc.items[lvl == 2,list(UPC,chain)])

	# run correlation analysis within chains for each SKU
	for (i in 1:nrow(upc.chains))
	{
		print(upc.chains[i])
		dat.fi = merge(upc.chains[i], dat.cat[!is.na(IRI_KEY)], by = c("UPC","chain"))
		
		for (variable.name in variables.to.test) {
			if(sum(dat.fi[,eval(variable.name), with = FALSE], na.rm=TRUE) != 0) {
				cor.method = f_cor.choose.method(NULL, variable.name, 1)
				mx.in = f_cor.prepare.matrix(dat.fi, variable.name, 1)
				if (sum(colSums(mx.in,na.rm=T)>0)>1) {   # test to make sure we have more than one column with a non-zero value
					if (exists("cor.output") && is.data.frame(cor.output))  {
						cor.output = rbindlist(list(cor.output, f_cor.run(mx.in, cor.method, variable.name, upc.chains[i])))
					} else {
						cor.output = f_cor.run(mx.in, cor.method, variable.name, upc.chains[i])
					}
				}
			}
		}
	}

	cor.output$UPC = factor(cor.output$UPC)
	cor.output = data.table(droplevels(cor.output))
	setwd(pth.dropbox.data)
	fil = paste("./iri analysis output/correlations/", par.category, ".intra.chain.correlations.rds", sep="")
	saveRDS(cor.output, fil)
}


f_cor.within.chains.par = function() {
	
	# parallel version of the function to calculate corrrelations for all variables for all chains/items
	
	library("doSNOW")
	upc.chains = unique(fc.items[lvl == 2,list(UPC,chain)])
	cl <- makeCluster(8)
	registerDoSNOW(cl) 
	clusterExport(cl, c("%do%","foreach", "dtcomb", "isplitDT"))
	clusterExport(cl, c("f_cor.prepare.matrix","f_cor.run", "f_cor.choose.method", "variables.to.test", "dat.cat"))
	clusterExport(cl, c("upc.chains"),  envir=environment())
	
	## run correlation analysis within chains for each UPC/chain combination
	#dt.sub = isplitDT(sp, levels(sp$fc.item))
	cor.all =
		foreach (i = 1:nrow(upc.chains),.packages = c("data.table","Hmisc","reshape2"),
			.combine = "dtcomb", .multicombine = TRUE, .verbose =  TRUE) %dopar%  #function(x,y)rbindlist(list(x,y))
		{
			print(upc.chains[i])
			dat.fi = merge(upc.chains[i], dat.cat[!is.na(IRI_KEY)], by = c("UPC","chain"))
			
			# run the correlation analysis for each variable
			cor.item.chain =
				foreach (variable.name = variables.to.test,
 					.combine = "dtcomb", .multicombine = TRUE, .verbose =  TRUE) %do%     #function(x,y)rbindlist(list(x,y))
				{
					if(sum(dat.fi[,eval(variable.name), with = FALSE], na.rm=TRUE) != 0) {
						cor.method = f_cor.choose.method(NULL, variable.name, 1)
						mx.in = f_cor.prepare.matrix(dat.fi, variable.name, 1)
						if (sum(colSums(mx.in,na.rm=T)>0)>1) {   # test to make sure we have more than one column with a non-zero value
							f_cor.run(mx.in, cor.method, variable.name, upc.chains[i])
						}						
					}
				}
		}

	cor.all$UPC = factor(cor.all$UPC)
	cor.all = data.table(droplevels(cor.all))
	setwd(pth.dropbox.data)
	fil = paste("./iri analysis output/correlations/", par.category, ".intra.chain.correlations.rds", sep="")
	saveRDS(cor.all, fil)
	stopCluster(cl)
}

#=========================================================================================
#  correlations between chains per item - all promo & price variables
#=========================================================================================

f_cor.between.chains = function() {
	
	upcs = unique(fc.items[lvl == 3,list(UPC)])

	for (i in 1:nrow(upcs))
	{
		print(upcs[i])
		dat.fi = droplevels(merge(upcs, dat.cat[is.na(IRI_KEY) & !is.na(chain)], by = c("UPC")))
		for (variable.name in variables.to.test) {
			if(sum(dat.fi[,eval(variable.name), with = FALSE], na.rm=TRUE) != 0) {
				cor.method = f_cor.choose.method(NULL, variable.name, 2)
				mx.in = f_cor.prepare.matrix(dat.fi, variable.name, 2)
				if (sum(colSums(mx.in,na.rm=T)>0)>1) {   # test to make sure we have more than one column with a non-zero value
					if (exists("cor.output") && is.data.table(cor.output)) {
						cor.output = rbindlist(list(cor.output, f_cor.run(mx.in, cor.method, variable.name, upcs[i])))
					} else {
						cor.output = f_cor.run(mx.in, cor.method, variable.name, upcs[i])
					}
				}
			}
		}
	}
	cor.output$UPC = factor(cor.output$UPC)
	cor.output = data.table(droplevels(cor.output))
	setwd(pth.dropbox.data)
	fil = paste("./iri analysis output/correlations/", par.category, ".cross.chain.correlations.rds", sep="")
	saveRDS(cor.output, fil)
}

f_cor.between.chains.par = function() {
	
	# does not offer better performance as the overhead of copying the data out and setting upp the cluster 
	# hampers things too much
	
	library("doSNOW")
	upcs = unique(fc.items[lvl == 3,list(UPC)])
	cl <- makeCluster(8)
	registerDoSNOW(cl) 
	clusterExport(cl, c("%do%","foreach"))
	clusterExport(cl, c("f_cor.prepare.matrix","f_cor.run", "f_cor.choose.method", "variables.to.test", "dat.cat"))
	clusterExport(cl, c("upcs"),  envir=environment())
	
	
	# run correlation analysis across chains for each UPC combination
	cor.all =
		foreach (i = 1:nrow(upcs),.packages = c("data.table","Hmisc","reshape2"), .combine=function(x,y)rbindlist(list(x,y))) %dopar%
		{
			print(upcs[i])
			dat.fi = droplevels(merge(upcs, dat.cat[is.na(IRI_KEY) & !is.na(chain)], by = c("UPC")))
			cor.item.chain =
				foreach (variable.name = variables.to.test,	.combine=function(x,y)rbindlist(list(x,y))) %do%
				{
					if(sum(dat.fi[,eval(variable.name), with = FALSE], na.rm=TRUE) != 0) {
						cor.method = f_cor.choose.method(NULL, variable.name, 2)
						mx.in = f_cor.prepare.matrix(dat.fi, variable.name, 2)
						 # test to make sure we have more than one column with a non-zero value
						if (sum(colSums(mx.in,na.rm=T)>0)>1) {  
							f_cor.run(mx.in, cor.method, variable.name, upcs[i])
						}
					}
				}
		}
	
	cor.all$UPC = factor(cor.all$UPC)
	cor.all = data.table(droplevels(cor.all))
	setwd(pth.dropbox.data)
	fil = paste("./iri analysis output/correlations/", par.category, ".cross.chain.correlations.rds", sep="")
	saveRDS(cor.all, fil)
}

# generic functions to implement the correlations, including tasks such as:
# - deciding the appropriate correlation method (spearman/pearson)
# - preparing the input matrix for a single variable with different locations as columns
# - generating a correlation matrix of p-values and the correlation statistic

f_cor.choose.method = function(mx.in = NULL, variable.name, agg.level) {
	
	if (agg.level > 1) return('spearman') 
	if (agg.level == 1 & variable.name == "PRICE") return('spearman') else return('pearson')	
}
f_cor.prepare.matrix = function(dat.fi, variable.name, agg.level)
{
	# function to prepare the matrix for the corelations for a single variable
	# IN: dat.fi - the data in long format for a single product.
	# first stage is to cross-tabulate the data for a single variable, either with stores as column headings
	if (agg.level == 1)	{
		mx.in = dcast(dat.fi, WEEK ~ IRI_KEY, fun.aggregate=sum, na.rm=TRUE, value.var = variable.name)#,add.missing=TRUE,fill=NA)
  	}
	# or chains as column headings
	if (agg.level == 2) {
		mx.in = dcast(dat.fi, WEEK ~ chain, fun.aggregate = sum, na.rm=TRUE, value.var = variable.name)#, add.missing=TRUE, fill=NA)
  	}
	mx.in$WEEK = as.integer(as.character(mx.in$WEEK))
	weeks = data.table(WEEK = min(mx.in$WEEK):max(mx.in$WEEK))
	mx.in = merge(weeks, mx.in, by = "WEEK", all.x=TRUE)
	mx.in$WEEK = NULL
	mx.in = as.matrix(mx.in)
	if (variable.name == "PRICE") mx.in = diff(mx.in)
	mx.in
}

f_cor.run = function(mx.in, cor.method, cor.variable, sku.chain, cor.plot = FALSE)
{
	# create a correlation matrix and the p.values and sort it
	cor.set = rcorr(x = mx.in, type = cor.method)
	mx.r = cor.set$r
	mx.p = cor.set$P
	cor.sort = (rowSums(mx.r)-1)/(sqrt(length(mx.r))-1)
	mx.r = mx.r[order(-cor.sort),order(-cor.sort)] # order(names(mx),rank(-row.cor))
	mx.p = mx.p[order(-cor.sort),order(-cor.sort)]
	mx.r[upper.tri(mx.r,diag=TRUE)] <- NA
	mx.p[upper.tri(mx.p,diag=TRUE)] <- NA
	#cor.r = (mx.r[lower.tri(mx.r)]) 
    #cor.p = (mx.p[lower.tri(mx.p)])
 	cor.r = melt(mx.r, varnames = c('fc.item1', 'fc.item2'), na.rm = TRUE) ; cor.r = cor.r[!is.na(cor.r$value),]
	cor.p = melt(mx.p, varnames = c('fc.item1', 'fc.item2'), na.rm = TRUE) ; cor.p = cor.p[!is.na(cor.p$value),]
	names(cor.r)[3] = "cor.stat"
	names(cor.p)[3] = "test.stat"
	cor.output = data.table(sku.chain, variable.name = cor.variable, cor.r, test.stat = cor.p$test.stat)  
	#if (cor.plot ==TRUE) f_chain.cor.plot(ch, cor.variable, cor.set, mx)    
	cor.output
 	
}



f_chain.cor.plot = function(ch, cor.variable, cor.set, mx)
{
  this.mean = mean(cor.set$cor.set)  
  plot.title = paste("Correlation of ", cor.variable, " in chain ", ch," = ", format(this.mean, digits = 3),"\n",sep="")
  p1 = qplot(data = cor.set, y = cor.set, x= paste("CHAIN\n",as.character(chain),sep=""), geom="boxplot") + geom_hline(yintercept=0, color="black", linetype="dashed") +
    geom_jitter(colour = "red", size = 1) + 
    scale_y_continuous(limits = c(-1, 1)) +
    coord_flip() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                         axis.text.x  = element_text(size=16),
                         axis.title.y = element_text(face="bold", size=19),
                         axis.text.y  = element_text(size=16))
  
  p2 = qplot(data = cor.set, x=cor.set) + geom_vline(xintercept=0, color="black", linetype="dashed") +
    scale_y_continuous(name="Number of store pairs") +
    scale_x_continuous(name="Between store correlation", limits = c(-1, 1)) +
    theme(axis.title.x = element_text(face="bold", size=19),
          axis.text.x  = element_text(size=16),
          axis.title.y = element_text(face="bold", size=19),
          axis.text.y  = element_text(size=16)) +
    ggtitle(plot.title)   
  
  ggb<-ggplot_build(p2)
  ymax<-max(ggb$data[[1]]$y)  ## you have to hunt around a bit
  plot.text = paste("Number of pairs (stores): ", nrow(cor.set), " (",nrow(mx),")",sep="")
  p2 = p2 + annotate(geom = "text",hjust=0, label=plot.text, x=-1, y=.95*ymax)
  #print(p1)
  multiplot(p2,p1)
}

f_store.correlations = function(dat.fi, ch, variables.to.test = c("PRICE","PR","FEAT_ANY","DISP_ANY")) 
{

  ###### section relates to generating a price matrix for a single chain and testing 
  ###### correlations between store pairs.
  if (ch != "0") dat.fi = droplevels(dat.fi[dat.fi$chain == ch])
  x = ldply(variables.to.test, function(cor.variable) f_store.correlations.sub(ch=ch,dat.fi=dat.fi, cor.variable))
  x  
}
#f_store.correlations(sp.pos,"71",variables.to.test = c("PRICE","PR","FEAT_ANY","DISP_0"))


f_panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = "spearman", use = "pairwise.complete.obs"))  #method = c("pearson", "kendall", "spearman")
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
#pairs(dat.p, lower.panel=panel.smooth, upper.panel=panel.cor)

f_scatterplotmatrix_stores = function(ch, print.scatterplotmatrix = FALSE, meas.var = "avg_price")
{
  
  # get the chain data - weekly price by store
  if (meas.var == "avg_price") meas.var = 7  
  if (meas.var == "p_feat_any") meas.var = 8  
  #if (meas.var == "avg_price") meas.var = 7  
  
  dat.p = dat[dat$chain == ch & dat$agg_level == "STORE",c(1,4,meas.var)]
  this.chain = chains[chains$chain == ch,]            
  # cast the store data ready for correlation
  dat.p = data.frame(cast(dat.p, period_id~store_id,mean,na.rm=T))
  
  # there must be 2 or more store for a matrix
  cor.set <- data.frame(chain=character(),
                        meas.var = character(),
                        cor.set=numeric(), 
                        stringsAsFactors=TRUE)
  if (ncol(dat.p) > 2)
  {
    dat.p = dat.p[,-1]
    
    # replace zeros with NA
    dat.p[] <- lapply(dat.p, function(dat.p){replace(dat.p, dat.p == 0, NA)}) 
    mx = as.matrix(cor(dat.p, method = "spearman", use = "pairwise.complete.obs"))
    
    row.cor = (rowSums(mx)-1)/(sqrt(length(mx))-1)
    dat.p = dat.p[,order(-row.cor)]
    #print(paste("CHAIN:",ch, "stores:", this.chain$stores, "revenue ($m):",this.chain$revenue ))
    # now create a correlation matrix and do the scatterplot
    
    #list =
    row.cor = (rowSums(mx)-1)/(sqrt(length(mx))-1)
    mx = mx[order(-row.cor),order(-row.cor)] # order(names(mx),rank(-row.cor))
    
    this.mean = mean((mx[lower.tri(mx)]))
    cor.set = (mx[lower.tri(mx)]) 
    cor.set = data.frame(chain = ch, meas.var = meas.var, cor.set)
    
    p1 = qplot(data = cor.set, y = cor.set, x= chain, geom="boxplot") + geom_hline(yintercept=0, color="black", linetype="dashed") +
      geom_jitter(colour = "red", size = 1) + 
      scale_y_continuous(limits = c(-1, 1)) +
      coord_flip() + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
    
    p2 = qplot(data = cor.set, x=cor.set) + geom_vline(xintercept=0, color="black", linetype="dashed") +
      #scale_y_discrete(name="Number of stores") +
      scale_x_continuous(name="Between store correlation", limits = c(-1, 1)) +
      ggtitle(paste("Correlation of price in chain", ch,"=", format(this.mean, digits = 3),"\n"))
    
    multiplot(p2,p1)
    if (print.scatterplotmatrix == TRUE) plt = pairs(dat.p, lower.panel=panel.smooth, upper.panel=f_panel.cor)  #hist(mx)
  }
  
  cor.set
  
}


library(plyr) ; library(reshape2); library(ggplot2) ; library(data.table)

rm(list = ls())
options(width = 250)  ;  options(scipen=100)
machine = (Sys.info()["nodename"])

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

## define the parameters for this procedure (category from arguments or set a default)
args <- commandArgs(trailingOnly = TRUE)  ;  print (args)
par.category = args[1]    ; if (is.na(par.category)) par.category = "beer"

f_cor.results.load = function(par.category, agg.level = "intra.chain") {
	# will load the results of the inter-store correlations for each variable
	setwd(pth.dropbox.data)
	if (agg.level == "intra.chain") {
		fil = paste("./iri analysis output/correlations/", par.category, ".intra.chain.correlations.rds", sep="")
	} else {
		fil = paste("./iri analysis output/correlations/", par.category, ".cross.chain.correlations.rds", sep="")
	}
	readRDS(fil)
}

#=========================================================
# run the analysis plots for a single category at a time
cor.output = f_cor.results.load(par.category)
variables.to.test = unique(cor.output$variable.name)

cor.output[,mean(cor.stat),by =c("UPC","chain","variable.name")]
dcast(cor.output, chain + UPC ~ variable.name, mean,value.var="cor.stat")


ggplot(data=cor.output[variable.name %in% c("PRICE","PR")], aes(x=reorder(chain,cor.stat, FUN=median), y = cor.stat)) +
	geom_boxplot(fill="lightblue") + facet_wrap( ~ variable.name) + 
	coord_flip() + xlab("Chain") + ylab("Correlation") + ggtitle(paste("Correlation of Differenced Prices\n between stores of a Chain (", par.category, ")\n", sep="")) + theme_bw()
fil = paste("./iri analysis output/correlations/", par.category, ".PRICE.intra.chain.correlations.png", sep="")
ggsave(fil)
	
		
ggplot(data=cor.output[variable.name %in% grep("FEAT_",variables.to.test, value = TRUE) ], aes(x=reorder(chain,cor.stat, FUN=median), y = cor.stat)) +
	geom_boxplot(fill="lightgoldenrod1") + facet_wrap( ~ variable.name, nrow=1) + 
	coord_flip() + xlab("Chain") + ylab("Correlation") + ggtitle(paste("Correlation of FEATURE Variables\n between stores of a Chain (", par.category, ")\n", sep="")) + theme_bw()
fil = paste("./iri analysis output/correlations/", par.category, ".FEAT.intra.chain.correlations.png", sep="")
ggsave(fil)
	
ggplot(data=cor.output[variable.name %in% grep("DISP_",variables.to.test, value = TRUE) ], aes(x=reorder(chain,cor.stat, FUN=median), y = cor.stat)) +
	geom_boxplot(fill="turquoise") + facet_wrap( ~ variable.name, nrow=1) + 
	coord_flip() + xlab("Chain") + ylab("Correlation") + ggtitle(paste("Correlation of DISPLAY Variabless\n between stores of a Chain (", par.category, ")\n", sep="")) + theme_bw()
fil = paste("./iri analysis output/correlations/", par.category, ".DISP.intra.chain.correlations.png", sep="")
ggsave(fil)

	
# ranking????


#=======================================================================================
# compare ALL variables across ALL categories
#=======================================================================================
categories = c("beer", "carbbev", "milk")
cor.output.all = rbindlist(lapply(categories,function(x) data.table(category = x, f_cor.results.load(x))))
ggplot(data=cor.output.all, aes(x=variable.name,fill = category, y = cor.stat)) + geom_boxplot() + coord_flip()





#================================================================================================
#  CROSS CHAIN CORRELATIONS - which chains are correlated with one another?
#================================================================================================

# need to use this to identify which chains are highly correlated
par.category = "carbbev"
cor.dat = f_cor.results.load(par.category,agg.level = "cross.chain")
cor.dat[,cor.pair:=paste(pmin(as.integer(fc.item1),as.integer(fc.item2)), pmax(as.integer(fc.item1),as.integer(fc.item2)),sep="+")]
ggplot(cor.dat,aes(variable.name, cor.stat)) + geom_boxplot() + coord_flip()

names(cor.dat)
cor.dat[cor.stat>0.5][order(-cor.stat)]

cor.means = cor.dat[,list(cor.stat = mean(cor.stat)), by=c("cor.pair")] ; setkey(cor.means, cor.pair)
cor.variables = data.table(dcast(cor.dat, cor.pair ~ variable.name, mean, value.var = "cor.stat"), key="cor.pair") 
cor.summary = merge(cor.means, cor.variables)[order(-cor.stat)] 
cor.summary = cbind(cor.summary[,1,with=FALSE],round(cor.summary[,-1,with=FALSE],4) )
cor.summary



#cor.dat[,list(cor.stat = mean(cor.stat)), by=c("cor.pair","variable.name")][order(-cor.stat)]



cor.dat.temp = cor.dat
cor.dat.temp$fc.item1a = cor.dat.temp$fc.item1
cor.dat.temp$fc.item1 = cor.dat.temp$fc.item2
cor.dat.temp$fc.item2 = cor.dat.temp$fc.item1a
cor.dat.temp$fc.item1a = NULL
cor.dat = rbindlist(list(cor.dat, cor.dat.temp))
cor.dat.temp = NULL



print(cast(cor.dat, fc.item1 + fc.item2 ~ variable.name, mean, value = "cor.stat"), digits = 4)

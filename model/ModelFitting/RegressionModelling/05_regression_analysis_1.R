
library("data.table") ; library("plyr")  ; library("reshape2")
rm(list=ls())
options(width=200)
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

#========== LOAD THE BASE DATA (coefficients, variable hierachy and fc.items) =================
setwd(pth.dropbox.data)

vars = read.csv("./other/variables.csv", header = TRUE, stringsAsFactors=TRUE)
categories = c("beer", "carbbev", "milk")	

# load in the coefficients for weekly and monthly data based on regression models
f_load.cf = function(category, periodicity) data.table(periodicity = periodicity, readRDS(paste("./output/", category, "_", periodicity, ".regression.coef.rds", sep = "")))
cf.all = rbindlist(lapply(categories, function(category) rbindlist(list(f_load.cf(category,"weekly"),f_load.cf(category,"445")))))
setkeyv(cf.all, c("variable", "fc.item", "periodicity"))

# load the fc.items (UPC, level, chain, market, etc)
f_load.fc.items = function(category) data.table(category = category, readRDS(paste("./iri category subsets/reformatted/", category, ".subset.fc.items.rds", sep = "")))
fc.items = rbindlist(lapply(categories, f_load.fc.items))

# use expand.grid to create every possible combination of item/variable to fill the gaps
cf.expand = data.table(	expand.grid(variable = unique(cf.all$variable), fc.item = unique(cf.all$fc.item), periodicity = c("weekly","445")), key = "variable,fc.item,periodicity")
cf.expand = merge(cf.expand, vars, by = "variable")
cf.expand = merge(cf.expand,fc.items[,list(fc.item, lvl, category)],  by="fc.item", all.x=TRUE)
setkeyv(cf.expand,  c("variable", "fc.item", "periodicity"))
cf.expand = merge(cf.expand, cf.all, all.x=TRUE)

cf.all = cf.expand

cf.all$variable = factor(cf.all$variable)
cf.all$signif = factor(as.character(cf.all$signif), levels = c(" ", ".","*", "**","***"), labels = c("<1","<0.1","<0.05","<0.01","<0.001"), ordered = TRUE)
with(cf.all,table(periodicity,signif))
#levels(cf.all$signif) = c(".","**","***","****","*")
cf.weekly = cf.all[periodicity=="weekly"]
cf.445= cf.all[periodicity=="445"] 
cf.445[,sum(is.na(estimate))/.N,by=list(variable,category)][V1!=1,]


#========== Analyse significance =================
library(ggplot2);library(scales)

ggplot(data=cf.weekly, aes(x=display.name)) + 
	geom_bar(position = "fill",aes(fill = signif)) + scale_fill_brewer(palette="RdYlGn") + scale_y_continuous(labels = percent_format()) +
	facet_grid(category~var.group.short, drop = TRUE, scales = "free_x", space = "free") + theme_bw() +
	theme(text = element_text(size=10), axis.text.x = element_text(angle=-90, vjust=0.5, hjust=0)) +
	ggtitle ("Proportion of time series with significanct inclusion of variables in regression model (Weekly Data)") + labs(x="Variable", y="")

ggplot(data=cf.445, aes(x=display.name)) + 
	geom_bar(position = "fill",aes(fill = signif)) + scale_fill_brewer(palette="RdYlGn") + scale_y_continuous(labels = percent_format()) +
	facet_grid(category~var.group.short, drop = TRUE, scales = "free_x", space = "free") + theme_bw() +
	theme(text = element_text(size=10), axis.text.x = element_text(angle=-90, vjust=0.5, hjust=0)) +
	ggtitle ("Proportion of time series with significanct inclusion of variables in regression model (445 Data)") + labs(x="Variable", y="")

	
library(reshape2)
t2 <- acast(cf.expand, category+var.group1~signif, length)
t3 = data.table(rownames(t2,),prop.table(t2,1))
t4 = cbind(colsplit(t3$V1,"_", c("category","variable")), t3[,!"V1", with = FALSE])

dcast(cf.all, category+var.group1+lvl~signif)
	
library(ggplot2)
p = qplot(data=cf.all, x=variable, geom="point", stat="bin") + facet_grid(category~var.group1, scales = "free",space = "free", drop = TRUE) 
p + theme(text = element_text(size=10), axis.text.x = element_text(angle=-90, vjust=0.5, hjust=1)) 

#========== Elasticities =================
cf.cast = data.table(dcast(cf.all, category + fc.item ~  variable, sum, na.rm=TRUE,value.var = "elasticity") )
names(cf.cast)
head(cf.cast)	

elas = cf.all[!is.na(elasticity)]

dcast(elas[variable == "PRICE"], category + periodicity ~ lvl,	fun.aggregate = median, na.rm = TRUE,value.var="elasticity")

library(ggplot2)

plot.data = elas[variable == "DISP_MINOR"]

ggplot(data = plot.data, aes(x=periodicity, y=elasticity)) + geom_boxplot() + facet_grid(lvl~category, scales = "free")
ggplot(data = plot.data, aes(x=elasticity)) + geom_histogram() + facet_grid(lvl~category, scales = "free")
ggplot(data = plot.data, aes(x=elasticity)) + geom_density(aes(colour=periodicity),size=0.75)+ scale_x_continuous(limits = c(-10,5)) + facet_grid(lvl~category, scales = "free") + theme_bw()

?geom
qplot(data=elas, x=variable,y=elasticity, geom = "boxplot") + facet_grid(lvl~category) +coord_flip()


#==============================================================================================

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

library(data.table)
store.stats = spw[!is.na(IRI_KEY), 
	j =	list(revenue = sum(DOLLARS, na.rm=TRUE),
		deal_intensity = mean(PR,na.rm=TRUE), 
		feat_intensity = mean(FEAT_ANY, na.rm=TRUE),
		disp_intensity = mean(DISP_ANY, na.rm=TRUE),
		deal_plus_feat_intensity = mean((PR * FEAT_ANY), na.rm=TRUE),
		deal_plus_disp_intensity = mean((PR * DISP_ANY), na.rm=TRUE),
		deal_plus_feat_disp_intensity = mean((PR * FEAT_ANY * DISP_ANY), na.rm=TRUE),
		price_consistency = sd(PRICE,na.rm=TRUE)/mean(PRICE,na.rm=TRUE)),
	by = list(UPC,chain,IRI_KEY) ]

		
chain.stats = spw[!is.na(IRI_KEY), 
	j =	list(revenue = sum(DOLLARS, na.rm=TRUE), 
		stores = length(unique(IRI_KEY)),
		deal_intensity = mean(PR,na.rm=TRUE), 
		feat_intensity = mean(FEAT_ANY, na.rm=TRUE),
		disp_intensity = mean(DISP_ANY, na.rm=TRUE),
		deal_plus_feat_intensity = mean((PR * FEAT_ANY), na.rm=TRUE),
		deal_plus_disp_intensity = mean((PR * DISP_ANY), na.rm=TRUE),
		deal_plus_feat_disp_intensity = mean((PR * FEAT_ANY * DISP_ANY), na.rm=TRUE),
		price_consistency = sd(PRICE,na.rm=TRUE)/mean(PRICE,na.rm=TRUE)),
	by=chain]
	
chain.stats[,market.share:=100*revenue/sum(revenue)]
library(ggplot2)

ggplot(data=chain.stats, aes(x= reorder(factor(paste("Chain",chain)),-market.share), y = market.share)) + 
	geom_bar(fill="blue") + coord_flip() +
	ggtitle("Market Share by chain for item 00-01-18200-53030") + labs(y="Market Share (%)", x = "Chain") +
	theme_bw()

#=====================================================================================================================


f_gg.ts.plot(dat.in) {
	#p = ggplot(dat.in, aes(x = 
	}
# weekly
ggplot(spw[!is.na(chain) & is.na(IRI_KEY)],aes(x=WEEK,y=UNITS))+ geom_line(aes(colour=chain)) + facet_wrap(~chain, scales = "free_y",ncol=1)
ggplot(spw[!is.na(chain) & is.na(IRI_KEY)],aes(x=WEEK,y=UNITS))+ geom_line(aes(colour=chain)) #+ facet_wrap(~chain, ncol=1)
# monthly/445
ggplot(spm[!is.na(chain) & is.na(IRI_KEY)],aes(x=period_id,y=UNITS))+ geom_line(aes(colour=chain)) + facet_wrap(~chain, ncol=1, scales = "free_y")


names(ssm)


## FIRST STEP
## convert the raw files to a more suitable format (rds) whilst also combining 
## 4 columns into a UPC code and merging all the years into one consolidated file

rm(list=ls())
ptm = proc.time()
library(plyr)  ;  library(data.table)

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


## libraries and sourcing additional R scripts for functions used in this script
setwd(pth.dropbox.code)
source("./scripts/memory_usage.R")


## Parameter definitions, will be defined as arguments passed in by scripts in the future
args <- commandArgs(trailingOnly = TRUE)
print (args)

## define the parameters for this procedure
par.category = args[1]  #par.category = "carbbev" "milk"  #
#par.category = "blades"
par.start.year = 1
par.end.year = 6
par.drug.opt = TRUE  ;  par.groc.opt = TRUE
par.transform.data = TRUE   ;  par.merge.data = TRUE

## Define file paths in use
#pth.raw = "/storage/users/wellerm/data/01_raw/"
pth.raw = "/scratch/hpc/29/wellerm/Academic Dataset External/"
#pth.tf = "/storage/users/wellerm/data/02_tf/sales/"
pth.tf = "/scratch/hpc/29/wellerm/IRI sales consolidated/"
colClasses = c("integer", "integer","integer","integer","integer","integer","integer","numeric", "character", "integer","integer")

dir.create(file.path(pth.tf, par.category), showWarnings = FALSE)
#setwd(file.path(mainDir, subDir))

################
## DATA - calendar is required to determine the IRI start and end weeks for each year
setwd(pth.dropbox.data)
cal = read.csv("./iri reference data/IRI Calendar.csv")[,c(1,4)]
years = ddply(cal,.(dataset_Year),summarise,min(IRI_week),max(IRI_week))
names(years) = c("yr","start.week","end.week")
print(years)

###############

f_iri.process.data.file = function(dt)
{  
  dt[,`:=`(
		WEEK=(WEEK - 1113),
		UPC = paste(as.character(sprintf("%02d", SY)),
                as.character(sprintf("%02d", GE)), 
                as.character(sprintf("%05d", VEND)),
                as.character(sprintf("%05d", ITEM)),sep ="-"))]
	dt[,c("SY","GE","VEND","ITEM"):=NULL]
  dt
}

f.transform = function(this.year) {

	# build the file locations
	pth.yr.cat = paste(pth.raw, "Year", this.year, "/External/", par.category, "/", par.category, sep ="")
	
	# now process the drug and grocery files as required, loading from file and then cleaning
	if (par.drug.opt == TRUE) {
		fil.drug = paste(pth.yr.cat, "_drug_", years[this.year, 2], "_", years[this.year,3], sep = "")
		dt.drug = data.table(read.table(file = fil.drug, sep = "", header = TRUE, colClasses = colClasses, strip.white = TRUE, comment.char = "")) ; print(nrow(dt.drug))
		dt.drug = f_iri.process.data.file(dt.drug) ; print(nrow(dt.drug))
 }
  	if (par.groc.opt == TRUE) {
		fil.groc = paste(pth.yr.cat, "_groc_", years[this.year, 2], "_", years[this.year,3], sep = "")
		dt.groc = data.table(read.table(file = fil.groc, sep = "", header = TRUE, colClasses = colClasses, strip.white = TRUE, comment.char = "")) ; print(nrow(dt.groc))
		dt.groc = f_iri.process.data.file(dt.groc)  ; print(nrow(dt.groc)) }
	print(lsos(n=5))
  	
	# combine the files for this year
	if ((par.groc.opt + par.drug.opt) == 2) {
  		dat.processed = rbindlist(list(dt.drug, dt.groc)) 
	} else {
		if (par.groc.opt == TRUE) dat.processed = dt.groc else dat.processed = dt.drug
	}
	dt.drug = dt.groc = NULL ; 	gc()
	print(proc.time() - ptm)
	
	# finally output the transformed and consolidated file for the year/category
	output.file = paste(pth.tf, par.category,"/", par.category,".tf.", this.year, ".rds", sep="") 
	saveRDS(dat.processed, file = output.file)
	print(paste(nrow(dat.processed), "rows saved to file:", output.file))
	dat.processed = NULL ; gc(reset=TRUE)	
}

###### EXECUTE THE TRANSFORMATIONS AND CONSOLIDATION FOR A CATEGORY #####

# actually perform the transforms and save the individual year files as rds
if (par.transform.data == TRUE) {
	for (this.year in par.start.year:par.end.year) {
		print(paste("Loading:", par.category, "year", this.year))
		f.transform(this.year)
	}
}

####### now merge the yearly files into one
if (par.merge.data==TRUE) {
	fil = paste(pth.tf, par.category,"/", par.category,".tf.1", ".rds", sep="") 
	dat.all = readRDS(fil)
	dat.all$year = 1
	for (i in 2:par.end.year) {
		fil = paste(pth.tf, par.category,"/", par.category,".tf.", i, ".rds", sep="") 
		print(paste("processing file: ", fil, sep=""))
		dat.i = readRDS(fil)
		dat.i$year = i
		dat.all = rbindlist(list(dat.all, dat.i))
		dat.i = NULL ; gc()
		print(lsos(n=5))
	}
	fil = paste(pth.tf, par.category,"/", par.category, ".tf.all.rds", sep="")
	saveRDS(dat.all, file = fil)
} 

dat.i = NULL ; gc()
print(lsos())

# This file contains the functions necessary to turn a single category dataset with dummy variables
# into the format for passing to auto.step.reg


f_prepare.reg.dat.main = function(par.category, par.weekly = TRUE, par.445 = TRUE, 
                                       sp.weekly = NULL, sp.445 = NULL) {

    
    ### changes needed her for paths
    
	#=========================================================================================
	# ALL fc.items WEEKLY for this category data file
	#=========================================================================================
	if (par.weekly == TRUE) {	
		fil = paste("./iri category subsets/reformatted/",  par.category, ".subset.sales.promos.weekly.rds", sep = "")  #category, "/",
		sp.weekly = readRDS(fil)
		setkeyv(sp.weekly, c("fc.item","WEEK"))

		sp.weekly = merge(sp.weekly, calendar.slim, by="WEEK")  # add in the calendar fields
		sp.weekly = merge(sp.weekly, harmonics.weekly, by="WEEK")  # add in the calendar fields

		setcolorder(sp.weekly, c("fc.item","WEEK", names(sp.weekly)[3:length(sp.weekly)]))
		keycols = c("fc.item","WEEK")
		setkeyv(sp.weekly,keycols) 

		##### this is the workhorse function
		sp.weekly = cbind(sp.weekly, sp.weekly[, f_data.transform.add.variables(.SD), by = fc.item][, fc.item := NULL])
		fil = paste("./regression datasets/", par.category, ".regression.data.weekly.rds", sep="")
		saveRDS(object=sp.weekly, file=fil)  
	}

	#=========================================================================================
	# ALL fc.items MONTHLY for this category data file
	#=========================================================================================
	if (par.445 == TRUE) {	
		fil = paste("./iri category subsets/reformatted/",  par.category, ".subset.sales.promos.445.rds", sep = "")  #category, "/",
		sp.445 = readRDS(fil)
		setkeyv(sp.445, c("fc.item","period_id"))
		
		#sp.445 = data.table(dat.cat.445, key = c("fc.item","period_id"))  ;  dat.cat.445 = NULL  ; gc()
		sp.445 = merge(sp.445, calendar.445, by="period_id")  # add in the calendar fields
		sp.445 = merge(sp.445, harmonics.445, by="period_id")  # add in the calendar fields
		gc()
		tables()

		setcolorder(sp.445, c("fc.item","period_id", names(sp.445)[3:length(sp.445)]))
		keycols = c("fc.item","period_id")
		setkeyv(sp.445,keycols) 

		##### this is the workhorse function
		sp.445 = cbind(sp.445, sp.445[, f_data.transform.add.variables(.SD), by = fc.item][, fc.item := NULL])
		fil = paste("./regression datasets/", par.category, ".regression.data.445.rds", sep="")
		saveRDS(object=sp.445, file=fil)  
	}

	par.category
}


f_ts.lagpad_1 <- function(x, k) { c(rep(NA, k), x)[1 : length(x)]  }
f_ts.lagpad <- function(x, k) { c(rep(NA, k), head(x, -k))  }
f_ts.seas.harmonics = function(season.type = "WEEKLY", num.periods = 313, max.harmonics = NULL){
	if (season.type == "WEEKLY")	seas.freq = (365.25/7)
	if (season.type == "445")		seas.freq = 12
	t = 1:num.periods
	SIN = COS = matrix(nr = num.periods, nc = seas.freq/2)
	if (is.null(max.harmonics)) max.harmonics = as.integer(seas.freq/2)
	if (max.harmonics > as.integer(seas.freq/2)) max.harmonics = as.integer(seas.freq/2)
	for (i in 1:max.harmonics)
	{
		COS[,i] = cos(2 * pi * i * t/seas.freq)
		SIN[,i] = sin(2 * pi * i * t/seas.freq)
	}
	colnames(SIN) = paste("SIN_",1:max.harmonics, sep="")
	colnames(COS) = paste("COS_",1:max.harmonics, sep="")
	cbind(COS,SIN)
} 

f_data.transform.add.variables = function(dat.in){
  
  ## PREPARE THE CALENDAR FILE DUMMIES AND TRANSFORMS AND JOIN TO DATASET
  #dummies = f_calendar.holidays.by.year(calendar, shoulder.span = 1) 
  #calendar = f_calendar.lag.lead(calendar)  
	
	# DEPENDENT VARIABLE DIFFERENCING AND LAGGING
	UNITS_LAG1 = f_ts.lagpad(dat.in$UNITS,1)
	UNITS_LAG2 = f_ts.lagpad(dat.in$UNITS,2)
	UNITS_LAG3 = f_ts.lagpad(dat.in$UNITS,3)
	UNITS_LAG4 = f_ts.lagpad(dat.in$UNITS,4)
	UNITS_LAG5 = f_ts.lagpad(dat.in$UNITS,5)

	UNITS_DIFF = c(NA, diff(dat.in$UNITS))
	UNITS_DIFF_LAG1 = f_ts.lagpad(UNITS_DIFF,1)
	UNITS_DIFF_LAG2 = f_ts.lagpad(UNITS_DIFF,2)
	UNITS_DIFF_LAG2 = f_ts.lagpad(UNITS_DIFF,3)
	UNITS_DIFF_LAG2 = f_ts.lagpad(UNITS_DIFF,4)
	
	UNITS_LOG = log(dat.in$UNITS)

	# PRICE variables including differences and lags
	PRICE_LOG = log(dat.in$PRICE)
	PRICE_LOG_LAG1 = f_ts.lagpad(PRICE_LOG,1)
	PRICE_LOG_LAG2 = f_ts.lagpad(PRICE_LOG,2)
	PRICE_LOG_LAG3 = f_ts.lagpad(PRICE_LOG,3)

	PRICE_LAG1 = f_ts.lagpad(dat.in$PRICE,1)
	PRICE_LAG2 = f_ts.lagpad(dat.in$PRICE,2)
	PRICE_LAG3 = f_ts.lagpad(dat.in$PRICE,3)
	PRICE_LAG4 = f_ts.lagpad(dat.in$PRICE,4)

	PRICE_DIFF = c(NA, diff(dat.in$PRICE))
	PRICE_DIFF_LAG1 = f_ts.lagpad(PRICE_DIFF,1)
	PRICE_DIFF_LAG2 = f_ts.lagpad(PRICE_DIFF,2)
	PRICE_DIFF_LAG3 = f_ts.lagpad(PRICE_DIFF,3)
	PRICE_DIFF_LAG4 = f_ts.lagpad(PRICE_DIFF,4)

	# PROMO LAGS
	FEAT_ANY_LAG1 = f_ts.lagpad(dat.in$FEAT_ANY,1)
	FEAT_ANY_LAG2 = f_ts.lagpad(dat.in$FEAT_ANY,2)
	FEAT_ANY_LAG3 = f_ts.lagpad(dat.in$FEAT_ANY,3)

	# INTERACTION TERMS
	#PRICE_DIFF_FEAT_ANY = PRICE_DIFF * dat.in$FEAT_ANY
	#PRICE_DIFF_Hols_any_binary = PRICE_DIFF * dat.in$Hols_any_binary
	#FEAT_ANY_Hols_any_binary = dat.in$FEAT_ANY * dat.in$Hols_any_binary
	#PRICE_DIFF_FEAT_ANY_Hols_any_binary = PRICE_DIFF * dat.in$FEAT_ANY * dat.in$Hols_any_binary

	# TREND TERMS BASED ON A FUNCTION (estimate of the cut points)
	#dat.in$TREND_1 = c(rep(0,50),1:(225-50), rep(0,313-225)) #* dat.in$WEEK
	#dat.in$TREND_2 = c(rep(0,225), 1:(313-225)) #* dat.in$WEEK
	dat.out= data.table(UNITS_LAG1, UNITS_LAG2, UNITS_LAG3, UNITS_LAG4, UNITS_LAG5,
						UNITS_DIFF, UNITS_DIFF_LAG1, UNITS_DIFF_LAG2, UNITS_LOG,
						PRICE_LOG, PRICE_LOG_LAG1, PRICE_LOG_LAG2, PRICE_LOG_LAG3,
						PRICE_LAG1, PRICE_LAG2, PRICE_LAG3, PRICE_LAG4, 
						PRICE_DIFF, PRICE_DIFF_LAG1, PRICE_DIFF_LAG2, PRICE_DIFF_LAG3, PRICE_DIFF_LAG4, 
						FEAT_ANY_LAG1, FEAT_ANY_LAG2, FEAT_ANY_LAG3)
						#PRICE_DIFF_FEAT_ANY, PRICE_DIFF_Hols_any_binary, FEAT_ANY_Hols_any_binary, PRICE_DIFF_FEAT_ANY_Hols_any_binary)
	as.list(dat.out)
}

f_data.transform.add.variables.445 = function(dat.iri, calendar.445){
  
  ## PREPARE THE CALENDAR FILE DUMMIES AND TRANSFORMS AND JOIN TO DATASET
  #dummies = f_calendar.holidays.by.year(calendar, shoulder.span = 1) 
  #calendar = f_calendar.lag.lead(calendar)  
  
  # DEPENDENT VARIABLE DIFFERENCING AND LAGGING
  dat.in$UNITS_LAG1 = f_ts.lagpad(dat.in$UNITS,1)
  dat.in$UNITS_LAG2 = f_ts.lagpad(dat.in$UNITS,2)
  dat.in$UNITS_LAG3 = f_ts.lagpad(dat.in$UNITS,3)
  dat.in$UNITS_LAG4 = f_ts.lagpad(dat.in$UNITS,4)
  dat.in$UNITS_LAG5 = f_ts.lagpad(dat.in$UNITS,5)
  
  dat.in$UNITS_DIFF = c(NA, diff(dat.in$UNITS))
  dat.in$UNITS_DIFF_LAG1 = f_ts.lagpad(dat.in$UNITS_DIFF,1)
  dat.in$UNITS_DIFF_LAG2 = f_ts.lagpad(dat.in$UNITS_DIFF,2)
  
  dat.in$UNITS_LOG = log(dat.in$UNITS)
  
  # PRICE variables including differences and lags
  dat.in$PRICE_LOG = log(dat.in$PRICE)
  dat.in$PRICE_LOG_LAG1 = f_ts.lagpad(dat.in$PRICE_LOG,1)
  dat.in$PRICE_LOG_LAG1 = f_ts.lagpad(dat.in$PRICE_LOG,2)
  dat.in$PRICE_LOG_LAG1 = f_ts.lagpad(dat.in$PRICE_LOG,3)
  
  dat.in$PRICE_LAG1 = f_ts.lagpad(dat.in$PRICE,1)
  dat.in$PRICE_LAG2 = f_ts.lagpad(dat.in$PRICE,2)
  dat.in$PRICE_LAG3 = f_ts.lagpad(dat.in$PRICE,3)
  
  dat.in$PRICE_DIFF = c(NA, diff(dat.in$PRICE))
  dat.in$PRICE_DIFF_LAG1 = f_ts.lagpad(dat.in$PRICE_DIFF,1)
  dat.in$PRICE_DIFF_LAG2 = f_ts.lagpad(dat.in$PRICE_DIFF,2)
  dat.in$PRICE_DIFF_LAG3 = f_ts.lagpad(dat.in$PRICE_DIFF,3)
  dat.in$PRICE_DIFF_LAG4 = f_ts.lagpad(dat.in$PRICE_DIFF,4)
  
  # PROMO LAGS
  dat.in$FEAT_ANY_LAG1 = f_ts.lagpad(dat.in$FEAT_ANY,1)
  
  
  # INTERACTION TERMS
  #dat.in$PRICE_DIFF_FEAT_ANY = dat.in$PRICE_DIFF * dat.in$FEAT_ANY
  #dat.in$PRICE_DIFF_Hols_any_binary = dat.in$PRICE_DIFF * dat.in$Hols_any_binary
  #dat.in$FEAT_ANY_Hols_any_binary = dat.in$FEAT_ANY * dat.in$Hols_any_binary
  #dat.in$PRICE_DIFF_FEAT_ANY_Hols_any_binary = dat.in$PRICE_DIFF * dat.in$FEAT_ANY * dat.in$Hols_any_binary
  
  # TREND TERMS BASED ON A FUNCTION (estimate of the cut points)
  #dat.in$TREND_1 = c(rep(0,50),1:(225-50), rep(0,313-225)) #* dat.in$WEEK
  #dat.in$TREND_2 = c(rep(0,225), 1:(313-225)) #* dat.in$WEEK
  
  dat.in
}


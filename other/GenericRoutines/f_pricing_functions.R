library("data.table") ; library("plyr") ; library("reshape2")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

f_reference.prices_KM = function(pt, PR, l = 5, C = (1/3), a = 0.5)
{
	# implements the algorithm developed by Kehoe & Midrigan, 2010
	
	# parameters for algorithm are as follows:
	# l = window either side of the period
	# C = proportion of prices in a period to constitute a mode
	# a = tolerance to missing data

	# create dt to hold the data during the run, initialising it with the actual prices and the PR flag
	dt = data.table(pt, PR = PR, pm = NA, pr = NA, ft = NA)

	TT = length(pt)
	# first pass will calculate the mode value for each period in the price data series
	pass1 = ldply((1+l):(TT - l), 
				function(t) {
				  x = pt[(t - l):(t + l)]
				  if (sum(x > 0, na.rm = TRUE) >= 2 * a * l) { 
					pm = Mode(x)
					ft = (sum(x > 0, na.rm = TRUE))/(2 * l + 1)
					data.frame(pm, ft)
				  } else  {
					pm = NA
					ft = NA
					data.frame(pm, ft) }#function(x1)x1^2)
				})
	dt$pm[(1+l):(TT - l)] = pass1$pm
	dt$ft[(1+l):(TT - l)] = pass1$ft
	
	# now start setting the reference price in a first pass *** bug?
	dt$pr[l + 1] = if (pass1[1,1] == 0) pass1[1,4] else pass1[1,1]
	for (t in (l + 2):(TT - l))
	{        
		# if the actual price is missing then use the previous reference price
		if (is.na(dt$pt[t]) & dt$ft[t] > C ) {
			dt$pr[t] = dt$pr[t-1]
		} else {
			if (!is.na(dt$pm[t]) & dt$pm[t] != 0 & dt$ft[t] > C & dt$pt[t] == dt$pm[t]) dt$pr[t] = dt$pt[t] else dt$pr[t] = dt$pr[t-1]    
		}
	}
	dt# RR - set of periods with regular price change
	# 
	for (i in 1:l)
	{
		R1 = which(diff(dt$pr)!=0) + 1
		R2 = which(dt$pr != 0) + 1
		R3 = which(dt$pr != 0)
		R.set = intersect(intersect(R1, R2), R3)

		C1 = which(dt$pr - dt$pt==0)
		C2 = R3
		C3 = which(dt$pt != 0)
		C.set = intersect(intersect(C1, C2), C3)

		P1 = which(dt$pr - dt$pt == 0) + 1
		P2 = R2
		P3 = which(dt$pt != 0) + 1
		P.set = intersect(intersect(P1, P2), P3)

		RC.set = intersect(R.set, C.set)
		if (length(RC.set) > 0) dt$pr[RC.set - 1] = dt$pt[RC.set]

		RP.set = intersect(R.set, P.set)
		if (length(RP.set) > 0) dt$pr[RP.set] = dt$pt[RP.set - 1]
	}
	#dt = dt[-c(1:l,(TT-l+1):TT),]
	
	dt$price.discount = round(dt$pt - dt$pr,6)
	
	dt$price.discount[is.na(dt$pt)]=NA
	dt$ft=NULL ; dt$pm = NULL
	dt$period = as.integer(rownames(dt))
  dt
}

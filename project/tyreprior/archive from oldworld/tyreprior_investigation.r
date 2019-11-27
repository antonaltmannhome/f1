### let's try running the functions that have already been set up, display what it wants to do, see if it all looks sensible

PROJECTPATH = paste(USERPATH, 'project/tyreprior jan2018/', sep = '')
source(paste(PROJECTPATH, 'tyreprior_data_setup.r', sep = ''))
source(paste(PROJECTPATH, 'tyreprior_funct.r', sep = ''))

## so let's just run it for a single race, our favourite, russia 2016

allValidRace = '2016russia'
myRacename = '2016russia'
priorScale = list(driver = 10, tyre = 10, tlap = 10, fuel = 100)

DisplayRace = function(myRacename theta1, theta2 = NULL) {
	
	myrr = racedb$rr[racedb$racename == myRacename]
	
	priorScale1 = list(driver = exp(theta1[1]), tyre = 0.1, tlap = 0.1, fuel = exp(theta1[2]))
	phase1Fit = FitSingleRace(myRacename, priorScale1, 1, 'no')
	
	priorScale2 = list(driver = exp(theta2[1]),
						tyre = exp(theta2[2]),
						tlap = exp(theta2[3]),
						fuel = exp(theta2[4]))
	phase2Fit = FitSingleRace(myRacename, priorScale2, 2, 'no')

	mod30PriorDF = rddf %>% 
				filter(racename==myRacename & !is.na(startinggrid)) %>%
				select(driver, mod30mixqrsmdcoef)

	# first plot drivers against each other
	driverCoef = left_join(mod30PriorDF, phase1Fit$driver, by = 'driver')
	myXLim = range(driverCoef$mod30mixqrsmdcoef)
	myYLim = range(driverCoef$driverCoef)
	driverPlot = ggplot(driverCoef, aes(x = mod30mixqrsmdcoef, y = driverCoef)) +
					geom_point() +
					geom_text(aes(label = driver), hjust = 0, vjust = 0) +
					scale_x_continuous(expand = c(0.1, 0.1)) +
					geom_abline(intercept = 0, slope = 1)
	
	# now display tyre slope from model 30, then average tyre slope obtained here
	
	myTyreSummaryDF=stintdb %>%
				filter(racename == myRacename) %>%
				group_by(tyre) %>%
				summarise(maxStintLength = max(endlap-startlap))

	# now join in the coefs from model 30
	myTyreSummaryDF$mod30TyreInt = mod30tyrecoefint[myrr,myTyreSummaryDF$tyre]
	myTyreSummaryDF$mod30TyreSlo = mod30tyrecoefslo[myrr,myTyreSummaryDF$tyre]
				
	myTyreNumOb = stintdb %>%
					filter(racename == myRacename) %>%
					mutate(stintLength = endlap-startlap) %>%
					group_by(tyre, stintLength) %>%
					summarise(numOb = length(tyre)) %>%
					arrange(tyre,stintLength) %>%
					mutate(csNumOb=rev(cumsum(rev(numOb))))
	
	ExpandLap = function(tyreLapInfo) {
		return(tibble(tyre = tyreLapInfo$tyre, lap = 1:tyreLapInfo$maxStintLength))
	}
	
	myTyreLapDF = myTyreSummaryDF %>%
					rowwise() %>%
					do(ExpandLap(tyreLapInfo = data.frame(tyre = .$tyre, maxStintLength = .$maxStintLength))) %>%
					bind_rows() %>%
					ungroup()
	# now attach numbner of observations
	myTyreLapDF = left_join(myTyreLapDF,
							myTyreNumOb %>%
							select(tyre, stintLength, csNumOb) %>%
							dplyr::rename(lap = stintLength),
							by = c('tyre', 'lap'))
	# but need to fill in the gaps
	ImputeNA = function(x) rep(x[!is.na(x)], diff(c(0,which(!is.na(x)))))
	myTyreLapDF = myTyreLapDF %>%
					group_by(tyre) %>%
					mutate(csNumOb = ImputeNA(csNumOb))
	
	# right, now we fit the lines - so, model 30 first
	myTyreLapDF = left_join(myTyreLapDF,
							myTyreSummaryDF,
							by = 'tyre')
	
	myTyreLapDF$mod30Intercept = racedb$mod30intercept[myrr]
	
	myTyreLapDF = myTyreLapDF %>%
				mutate(mod30Adjustment = mod30Intercept + mod30TyreInt + mod30TyreSlo * lap)
	
	# now join in phase1 fit
	myTyreLapDF$phase1Intercept = phase1Fit$intercept
	myTyreLapDF$phase1TyreSlo = phase1Fit$tlap$tlapCoef
	myTyreLapDF$phase1Adjustment = with(myTyreLapDF, phase1Intercept + phase1TyreSlo * lap)
	
	# now plot the bastard
	phase1TyrePlot = ggplot(myTyreLapDF, aes(lap)) +
					geom_point(aes(y = mod30Adjustment, colour = tyre, size = csNumOb)) +
					geom_line(aes(y = phase1Adjustment), colour = 'black')
	
	### ok, that all looks sensible enough, but the more important one to view is the phase 2 - let's get that

	
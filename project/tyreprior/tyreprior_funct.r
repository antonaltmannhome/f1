
GetDriverPriorDF = function(phase1Coef, mod30PriorDF) {
	phase1PriorDF = phase1Coef %>%
					filter(coefType == 'driver') %>%
					select(coefname, coefvalue) %>%
					dplyr::rename(driver = coefname)
	combinedPriorDF = left_join(phase1PriorDF, mod30PriorDF, by = 'driver')
	interceptAdjustment = with(combinedPriorDF, mean(coefvalue - smoothDCoef))
	mod30PriorDF = mod30PriorDF %>%
					mutate(prior = smoothDCoef + interceptAdjustment)
	# what if a driver is new? have to prick about with finding out who their teammate is, ugh
	# think we've done that though in teamprior
	# no, ignore all that, all has been done in mixqrsmdcoef, hooray former me
	driverPriorDF = left_join(phase1PriorDF,
							mod30PriorDF,
							by = 'driver')
	# however, in the case where a driver has been omitted from the sample, helpful to have a list where they're in it
	return(list(driverPriorDF = driverPriorDF, mod30PriorDF = mod30PriorDF))
}

SolveRaceMatrix = function(myLbl, mod30PriorDF, priorValue,  priorScale, phase12) {

	myundriv=unique(myLbl$driver)
	myuntyre=unique(myLbl$tyre)

	# first need to fudge the driver priors

	xmat.intercept=matrix(1, nrow = nrow(myLbl))
	xmat.driv=t(sapply(myLbl$driver, function(x) {myundriv==x}))
	xmat.tyre=t(sapply(myLbl$tyre, function(x) {myuntyre==x}))
	# but sapply is a dick when there's only one column...
	if (nrow(xmat.tyre) == 1) xmat.tyre = t(xmat.tyre)
	xmat.tyreLap = xmat.tyre[,1,drop=F] * myLbl$tyreLap2
	if (length(myuntyre) > 1) {
		for (j in 2:length(myuntyre)) {
			xmat.tyreLap=cbind(xmat.tyreLap, xmat.tyre[,j,drop=F]*myLbl$tyreLap2)
		}
	}
	xmat.fuel=myLbl$fuel2
	rownames(xmat.intercept)=NULL
	rownames(xmat.driv)=NULL
	rownames(xmat.tyre)=NULL
	rownames(xmat.tyreLap)=NULL
	rownames(xmat.fuel)=NULL

	xmat=as.matrix(cbind(xmat.intercept,xmat.driv, xmat.tyre, xmat.tyreLap, xmat.fuel))
	yvec=myLbl$sec

	### but we then get hold of priors for this race. drivers:
	interceptprior=-999
	driverprior = with(mod30PriorDF, smoothDCoef[match(myundriv, driver)])
	tyreprior=rep(priorValue$tyre, ncol(xmat.tyre))
	tyreLapprior=rep(priorValue$tyreLap, ncol(xmat.tyre))
	fuelprior=priorValue$fuel

	prior=c(interceptprior, driverprior, tyreprior, tyreLapprior, fuelprior)

	precisionmat=matrix(0,nrow=length(prior), ncol=length(prior))
	drivix=2:(length(myundriv)+1)
	tyreix=(length(myundriv)+2):(length(myundriv)+1+length(myuntyre))
	tyreLapix=(length(myundriv)+length(myuntyre)+2):(length(myundriv)+1+2*length(myuntyre))
	fuelix=length(myundriv)+2+2*length(myuntyre)
	diag(precisionmat)[drivix]=priorScale$driver
	diag(precisionmat)[tyreix]=priorScale$tyre
	diag(precisionmat)[tyreLapix]=priorScale$tyreLap
	diag(precisionmat)[fuelix]=priorScale$fuel
	### same prior on everything - that should work to start with
	coefType = rep(NA, length(prior))
	coefType[1] = 'intercept'
	coefType[drivix] = 'driver'
	coefType[tyreix] = 'tyre'
	coefType[tyreLapix] = 'tyreLap'
	coefType[fuelix] = 'fuel'

	betavec = solve(t(xmat) %*% xmat + precisionmat) %*%
	(precisionmat%*%prior + t(xmat)%*%yvec)

	coefdf=tibble(coefname=c('intercept', myundriv, myuntyre, myuntyre, 'fuel'),
					coefType = coefType,
					prior=prior,
					coefvalue=as.vector(betavec))
	return(coefdf)
}

### step 1, get driver intercept and the tyreLap prior

TidyMleDF = function(myMleDF, mod30PriorDF) {

	# now extract the coefs into nice lists
	interceptCoef = with(myMleDF, coefvalue[coefType == 'intercept'])
	driverCoefDF = left_join(mod30PriorDF,
								myMleDF %>%
								filter(coefType == 'driver') %>%
								select(coefname, coefvalue) %>%
								dplyr::rename(driver = coefname),
								by = 'driver') %>%
					mutate(driverCoef = ifelse(!is.na(coefvalue), coefvalue, smoothDCoef)) %>%
					select(driver, driverCoef)

	tyreCoefDF = myMleDF %>%
					filter(coefType == 'tyre') %>%
					dplyr::rename(tyre = coefname, tyreCoef = coefvalue) %>%
					select(tyre, tyreCoef)
	tyreLapCoefDF = myMleDF %>%
					filter(coefType == 'tyreLap') %>%
					dplyr::rename(tyre = coefname, tyreLapCoef = coefvalue) %>%
					select(tyre, tyreLapCoef)
	fuelCoef = with(myMleDF, coefvalue[coefType == 'fuel'])

	return(list(intercept = interceptCoef,
				driver = driverCoefDF,
				fuel = fuelCoef,
				tyre = tyreCoefDF,
				tyreLap = tyreLapCoefDF))
}

JoinLblToCoef = function(thisRaceLbl, myMleList, samplei) {

	thisRaceLbl$interceptCoef[which(thisRaceLbl$inSample != samplei)] = myMleList$intercept
	thisRaceLbl$isOtherSample = with(thisRaceLbl, inSample != samplei)
	thisRaceLbl = subset_join(thisRaceLbl,
								myMleList$driver,
								by = 'driver',
								subsetBool = isOtherSample)
	thisRaceLbl = subset_join(thisRaceLbl,
								myMleList$tyre,
								by = 'tyre',
								subsetBool = isOtherSample)
	thisRaceLbl = subset_join(thisRaceLbl,
								myMleList$tyreLap,
								by = 'tyre',
								subsetBool = isOtherSample)
	thisRaceLbl$fuelCoef[which(thisRaceLbl$inSample != samplei)] = myMleList$fuel
	return(thisRaceLbl)
}

FitSingleRace = function(myRace, priorScale, phase12, crossValidStatus, returnPred = FALSE) {

	mod30PriorDF = rddf %>%
					filter(race==myRace & !is.na(smoothDCoef)) %>%
					select(driver, smoothDCoef)
	thisRaceLbl = lbl %>%
					filter(race == myRace & isGoodPreValidRace) %>%
					select(driver, fuel2, tyre, tyreLap2, sec, inSample) %>%
					semi_join(mod30PriorDF, 'driver') %>%
					mutate(interceptCoef = NA,
							driverCoef = NA,
							tyreCoef = NA,
							tyreLapCoef = NA,
							fuelCoef = NA)
	if (phase12 == 1) {
		thisRaceLbl$tyre = 'combinedTyre'
	}

	myFuelPrior = with(raceDF, fuelPrior[race == myRace])
	myTyrePrior = 0
	if (phase12 == 1) {
		myTyreLapPrior = 0.1
	}
	# NB if phase12 == 2, more complicated to determine what we should do (see below)

	if (crossValidStatus == 'cv') {
		sampleMleList = NULL
		for (samplei in 1:2) {
			if (phase12 == 2) {
				if (samplei == 1) {
					myTyreLapPrior = with(raceDF, tyreLapPrior1[race == myRace])
				}
				if (samplei == 2) {
					myTyreLapPrior = with(raceDF, tyreLapPrior2[race == myRace])
				}
			}
			priorValue = list(fuel = myFuelPrior, tyre = myTyrePrior, tyreLap = myTyreLapPrior)
			myLbl = thisRaceLbl %>%
					filter(inSample == samplei)

			myMleDF = SolveRaceMatrix(myLbl, mod30PriorDF, priorValue, priorScale, phase12)
			myMleList = TidyMleDF(myMleDF, mod30PriorDF)
			sampleMleList[[samplei]] = myMleList

			# match up those coefs to half they need to predict:
			thisRaceLbl = JoinLblToCoef(thisRaceLbl, myMleList, samplei)
		}

		thisRaceLbl = thisRaceLbl %>%
					mutate(predSec = interceptCoef + driverCoef + fuelCoef * fuel2 + tyreCoef + tyreLapCoef * tyreLap2)

		thisRaceLbl$sqDiff = with(thisRaceLbl, (predSec - sec)^2)
		meanSqDiff = mean(thisRaceLbl$sqDiff)
		if (!returnPred) {
			toReturn = list(meanSqDiff = meanSqDiff,
							sampleMleList = sampleMleList)
		}
		if (returnPred) {
			toReturn = list(meanSqDiff = meanSqDiff,
							sampleMleList = sampleMleList,
							thisRaceLbl = thisRaceLbl)
		}
	}

	if (crossValidStatus == 'no') {
		if (phase12 == 2) {
			myTyreLapPrior = with(raceDF, tyreLapPrior[race == myRace])
		}
		priorValue = list(fuel = myFuelPrior, tyre = myTyrePrior, tyreLap = myTyreLapPrior)
		myLbl = thisRaceLbl

		myMleDF = SolveRaceMatrix(myLbl, mod30PriorDF, priorValue, priorScale, phase12)
		myMleList = TidyMleDF(myMleDF, mod30PriorDF)

		toReturn = myMleList
	}

	return(toReturn)
}

FitAllRace = function(theta, phase12) {
	if (phase12 == 1) {
		priorScale = list(driver = exp(theta[1]), tyre = 0.1, tyreLap = 0.1, fuel = exp(theta[2]))
	}
	if (phase12 == 2) {
		priorScale = list(driver = exp(theta[1]), tyre = exp(theta[2]), tyreLap = exp(theta[3]), fuel = exp(theta[4]))
	}

	print('Prior scale:')
	print(paste(names(priorScale), as.numeric(priorScale), sep = ': '))

	if (any(exp(theta) > 10E7)) return(10E6)

	raceDF$meanSqDiff = NA
	for (ri in which(raceDF$isValidRace30)) {
		raceDF$meanSqDiff[ri] = FitSingleRace(raceDF$race[ri], priorScale, phase12, crossValidStatus = 'cv')$meanSqDiff
	}

	meanMeanSqDiff = with(raceDF, mean(meanSqDiff[isValidRace30]))
	cat('Squared diff:', meanMeanSqDiff, '\n')
	return(meanMeanSqDiff)
}

# this is the function that should go in f1laptimelm as a replcement to FitLapTimeLM, it's basically FitSingleRace but strips away the complicated cross validation step
FitLapTimePriored = function(myRace) {

	mod30PriorDF = rddf %>%
									filter(race == myRace & !is.na(smoothDCoef)) %>%
									select(driver, smoothDCoef)
	myFuelPrior = with(raceDF, fuelPrior[race == myRace])
	thisRaceLbl = lbl %>%
									filter(race == myRace & isGoodPreValidRace) %>%
									select(driver, fuel2, tyre, tyreLap2, sec) %>%
									semi_join(mod30PriorDF, 'driver') %>%
									mutate(interceptCoef = NA,
													driverCoef = NA,
													tyreCoef = NA,
													tyreLapCoef = NA,
													fuelCoef = NA)
	priorScale = list(driver = 0.0001, tyre = 0.001,tyreLap = 0.001, fuel = 0.001)

	# phase1, where we combine all the tyres just to get a tyreLap prior

	phase1RaceLbl = thisRaceLbl %>%
										mutate(tyre = 'combinedTyre')

	myTyrePrior = 0
	myTyreLapPrior = 0.1

	phase1PriorValue = list(fuel = myFuelPrior, tyre = myTyrePrior, tyreLap = myTyreLapPrior)

	phase1MleDF = SolveRaceMatrix(phase1RaceLbl, mod30PriorDF, phase1PriorValue, priorScale, 1)

	# all we actually need from that is the tyreLap coef
	myTyreLapPrior = with(phase1MleDF, coefvalue[coefType == 'tyreLap'])

	phase2PriorValue = list(fuel = myFuelPrior, tyre = myTyrePrior, tyreLap = myTyreLapPrior)

	phase2MleDF = SolveRaceMatrix(thisRaceLbl, mod30PriorDF, phase2PriorValue, priorScale, 2)

	return(phase2MleDF)
}

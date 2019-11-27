### this is all that you ought to need i think

LoadAllData()
dum = f1smoothing:::GetSmooth(qrToFit = 'qr', qrToPredict = 'r', fwbw = 'fwbw',
								modelChoice = 30, useStretch = TRUE)
rddf = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'smoothDCoef')

lbl = f1validity:::MakeIsGoodPreValidRace(30, lbl, raceDF)

### so we need one vector for 'how many laps behind safety car in this race until this point, which we will use for fuel

lbl = lbl %>%
		group_by(race, driver) %>%
		mutate(cumSafetyCarLapInRace = cumsum(isSafetyCar)) %>%
		ungroup()
lbl = lbl %>%
		group_by(race, driver, stint) %>%
		mutate(cumSafetyCarLapInStint = cumsum(isSafetyCar)) %>%
		ungroup()

### values it comes up with are basically 1 for tyres, and -0.64 for fuel
lbl$fuel2 = lbl$fuel + 0.64 * lbl$cumSafetyCarLapInRace
lbl$tyreLap2 = lbl$tyreLap - lbl$cumSafetyCarLapInStint
message('Tyre lap adjustment looks suspicious, investigate it')

sax1013 = with(raceDF,which(year <= 2013))
sax14plus = with(raceDF,which(year >= 2014))
mod1013 = lm(raceDF$mod30FuelCoef[sax1013] ~ raceDF$perim[sax1013])
mod14plus = lm(raceDF$mod30FuelCoef[sax14plus] ~ raceDF$perim[sax14plus])
raceDF$fuelPrior = rep(NA,nrace)
raceDF$fuelPrior[sax1013] = predict(mod1013, data.frame(raceDF[sax1013,'perim']))
raceDF$fuelPrior[sax14plus] = predict(mod14plus, data.frame(raceDF[sax14plus,'perim']))


SolveRaceMatrix = function(myLbl, mod30PriorDF, priorValue,  priorScale) {

	myundriv = unique(myLbl$driver)
	myuntyre = unique(myLbl$tyre)

	# first need to fudge the driver priors

	xmat.intercept = matrix(1, nrow = nrow(myLbl))
	xmat.driv = t(sapply(myLbl$driver, function(x) {myundriv == x}))
	xmat.tyre = t(sapply(myLbl$tyre, function(x) {myuntyre == x}))
	# but sapply is a dick when there's only one column...
	if (nrow(xmat.tyre) == 1) xmat.tyre = t(xmat.tyre)
	xmat.tyreLap = xmat.tyre[,1,drop = FALSE] * myLbl$tyreLap2
	if (length(myuntyre) > 1) {
		for (j in 2:length(myuntyre)) {
			xmat.tyreLap = cbind(xmat.tyreLap, xmat.tyre[,j, drop = FALSE] * myLbl$tyreLap2)
		}
	}
	xmat.fuel = myLbl$fuel2
	rownames(xmat.intercept) = NULL
	rownames(xmat.driv) = NULL
	rownames(xmat.tyre) = NULL
	rownames(xmat.tyreLap) = NULL
	rownames(xmat.fuel) = NULL

	xmat = as.matrix(cbind(xmat.intercept, xmat.driv, xmat.tyre, xmat.tyreLap, xmat.fuel))
	yvec = myLbl$sec

	### but we then get hold of priors for this race. drivers:
	interceptprior = -999
	driverprior = with(mod30PriorDF, smoothDCoef[match(myundriv, driver)])
	tyreprior = rep(priorValue$tyre, ncol(xmat.tyre))
	tyreLapprior = rep(priorValue$tyreLap, ncol(xmat.tyre))
	fuelprior = priorValue$fuel

	prior = c(interceptprior, driverprior, tyreprior, tyreLapprior, fuelprior)

	precisionmat = matrix(0, nrow = length(prior), ncol = length(prior))
	drivix = 2:(length(myundriv) + 1)
	tyreix = (length(myundriv) + 2):(length(myundriv) + 1 + length(myuntyre))
	tyreLapix = (length(myundriv) + length(myuntyre) + 2):(length(myundriv) + 1 + 2*length(myuntyre))
	fuelix = length(myundriv) + 2 + 2 * length(myuntyre)
	diag(precisionmat)[drivix] = priorScale$driver
	diag(precisionmat)[tyreix] = priorScale$tyre
	diag(precisionmat)[tyreLapix] = priorScale$tyreLap
	diag(precisionmat)[fuelix] = priorScale$fuel
	### same prior on everything - that should work to start with
	coefType = rep(NA, length(prior))
	coefType[1] = 'intercept'
	coefType[drivix] = 'driver'
	coefType[tyreix] = 'tyre'
	coefType[tyreLapix] = 'tyreLap'
	coefType[fuelix] = 'fuel'

	betavec = solve(t(xmat) %*% xmat + precisionmat) %*%
											(precisionmat %*% prior + t(xmat) %*% yvec)

	coefdf = tibble(coefName = c('intercept', myundriv, myuntyre, myuntyre, 'fuel'),
									coefType = coefType,
									prior = prior,
									coefValue = as.vector(betavec))
	return(coefdf)
}

ConvertMleDFToList = function(myMleDF) {
		# now extract the coefs into nice lists
		interceptCoef = with(myMleDF, coefValue[coefType == 'intercept'])
		driverCoefDF = myMleDF %>%
										filter(coefType == 'driver') %>%
										select(coefName, coefValue) %>%
										rename(driver = coefName,
														driverCoef = coefValue)

		tyreCoefDF = myMleDF %>%
						filter(coefType == 'tyre') %>%
						rename(tyre = coefName, tyreCoef = coefValue) %>%
						select(tyre, tyreCoef)
		tyreLapCoefDF = myMleDF %>%
						filter(coefType == 'tyreLap') %>%
						dplyr::rename(tyre = coefName, tyreLapCoef = coefValue) %>%
						select(tyre, tyreLapCoef)
		fuelCoef = with(myMleDF, coefValue[coefType == 'fuel'])

		myMleList = list(intercept = interceptCoef,
											driver = driverCoefDF,
											fuel = fuelCoef,
											tyre = tyreCoefDF,
											tyreLap = tyreLapCoefDF)

		return(myMleList)
}


LapTimeLikFunct = function(theta, myMleDF, thisRaceLbl, priorScale) {

    myMleDF$coefValue = theta    
		myMleList = ConvertMleDFToList(myMleDF)
		thisRaceLblPlusCoef = thisRaceLbl %>%
														left_join(myMleList$driver, by = 'driver') %>%
														left_join(myMleList$tyre, by = 'tyre') %>%
														left_join(myMleList$tyreLap, by = 'tyre')
		thisRaceLblPlusCoef$interceptCoef = myMleList$intercept
		thisRaceLblPlusCoef$fuelCoef = myMleList$fuel

		thisRaceLblPlusCoef$predSec = with(thisRaceLblPlusCoef, interceptCoef +
																						driverCoef +
																						fuelCoef * fuel2 +
																						tyreCoef +
																						tyreLapCoef * tyreLap2)

		#thisRaceSD = with(thisRaceLblPlusCoef, sd(sec - predSec))
		#dataLikelihood = with(thisRaceLblPlusCoef, log(dnorm(sec, predSec, thisRaceSD)))
    #sumDataLikelihood = sum(dataLikelihood)
		dataSqDiff = with(thisRaceLblPlusCoef, (sec - predSec)^2)
		sumDataSqDiff = sum(dataSqDiff)
		
    priorScaleDF = list_to_tibble(priorScale, 'priorScale', 'coefType')
    myMleDFPlus = left_join(myMleDF, priorScaleDF, 'coefType')
		myMleDFPlus$sqDiff = with(myMleDFPlus, priorScale * (coefValue - prior)^2)
		sumPriorSqDiff = sum(myMleDFPlus$sqDiff, na.rm = TRUE)
		#sumPriorLikelihood = with(myMleDFPlus, 0.5 * sum(sqDiff[coefType != 'intercept']))
		
		#sumLikelihood = sumDataLikelihood - sumPriorLikelihood
		sumSqDiff = sumDataSqDiff + sumPriorSqDiff
		
		# return(- sumLikelihood)
		return(sumSqDiff)
}

# it's correct but it's slow, think we can do better
myMleDF = phase2MleDF
priorScaleDF = list_to_tibble(priorScale, 'priorScale', 'coefType')
priorScaleDF = add_row(priorScaleDF, coefType = 'intercept', priorScale = 0)
myMleDF = left_join(myMleDF, priorScaleDF, 'coefType')

myMleList = ConvertMleDFToList(myMleDF)
thisRaceLbl$driverMap = match(thisRaceLbl$driver, myMleList$driver$driver)
thisRaceLbl$tyreMap = match(thisRaceLbl$tyre, myMleList$tyre$tyre)

FastLapTimeLikFunct = function(theta, myMleDF, myMleList, thisRaceLbl) {
  
  myIntercept = theta[which(myMleDF$coefType == 'intercept')]
  myMleList$driver$theta = theta[which(myMleDF$coefType == 'driver')]
  myMleList$tyre$theta = theta[which(myMleDF$coefType == 'tyre')]
  myMleList$tyreLap$theta = theta[which(myMleDF$coefType == 'tyreLap')]
  myFuelCoef = theta[which(myMleDF$coefType == 'fuel')]
  thisRaceLbl$interceptCoef = myIntercept
  thisRaceLbl$fuelCoef = myFuelCoef
  thisRaceLbl$driverCoef = myMleList$driver$theta[thisRaceLbl$driverMap]
  thisRaceLbl$tyreCoef = myMleList$tyre$theta[thisRaceLbl$tyreMap]
  thisRaceLbl$tyreLapCoef = myMleList$tyreLap$theta[thisRaceLbl$tyreMap]
  
  thisRaceLbl$predSec = with(thisRaceLbl, interceptCoef +
                                       driverCoef +
                                       fuelCoef * fuel2 +
                                       tyreCoef +
                                       tyreLapCoef * tyreLap2)
  
  dataSqDiff = with(thisRaceLbl, (sec - predSec)^2)
  sumDataSqDiff = sum(dataSqDiff)
  
  myMleDF$theta = theta
  myMleDF$sqDiff = with(myMleDF, priorScale * (theta - prior)^2)
  sumPriorSqDiff = sum(myMleDF$sqDiff, na.rm = TRUE)
  
  sumSqDiff = sumDataSqDiff + sumPriorSqDiff
  
  return(sumSqDiff)
}


# but is that correct? need to do nlm to be sure really and see if it gets optimum answer, or close to the solve solution

# this is the function that should go in f1laptimelm as a replcement to FitLapTimeLM, it's basically FitSingleRace but strips away the complicated cross validation step
FitLapTimePriored = function(myRace) {

	mod30PriorDF = rddf %>%
									filter(race == myRace & !is.na(smoothDCoef)) %>%
									select(driver, smoothDCoef)
	myFuelPrior = with(raceDF, fuelPrior[race == myRace])
	thisRaceLbl = lbl %>%
									filter(race == myRace & isGoodPreValidRace) %>%
									select(driver, fuel2, tyre, tyreLap2, sec) %>%
									semi_join(mod30PriorDF, 'driver')
	priorScale = list(driver = 0.0001, tyre = 0.001,tyreLap = 0.001, fuel = 0.001)
  # priorScale = list(driver = 10, tyre = 100,tyreLap = 1000, fuel = 1000)
	
	# phase1, where we combine all the tyres just to get a tyreLap prior

	phase1RaceLbl = thisRaceLbl %>%
										mutate(tyre = 'combinedTyre')

	myTyrePrior = 0
	myTyreLapPrior = 0.1

	phase1PriorValue = list(fuel = myFuelPrior, tyre = myTyrePrior, tyreLap = myTyreLapPrior)

	phase1MleDF = SolveRaceMatrix(phase1RaceLbl, mod30PriorDF, phase1PriorValue, priorScale)

	# all we actually need from that is the tyreLap coef
	myTyreLapPrior = with(phase1MleDF, coefValue[coefType == 'tyreLap'])

	phase2PriorValue = list(fuel = myFuelPrior, tyre = myTyrePrior, tyreLap = myTyreLapPrior)

	phase2MleDF = SolveRaceMatrix(thisRaceLbl, mod30PriorDF, phase2PriorValue, priorScale)

	return(phase2MleDF)
}

# how easily can we get number of laps and the standard error?

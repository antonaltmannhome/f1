### smoothing got bloated and went outside its scope, let's tidy up all the useful things into this setup file then crack on with trying out a few interesting models

LoadAllData()
source('project/validate via finpos/messy-race-funct.r')

rddf = lazy_left_join(rddf, raceDF, 'race', 'circuit')

rddf = MakeCleanRace(rddf, 30)

lbl = f1validity::MakeIsRogue(lbl)
lbl = f1validity::MakeInTraffic(30, lbl)
lbl = f1validity::MakeIsOutlier(30, lbl)
lbl = lazy_left_join(lbl, raceTyreDF, c('race', 'tyre'), 'isValidTyre30')
lbl$isOvertaking = with(lbl, didOt > 0 | gotOt > 0 | gotLap > 0)
lbl$isBlocked = with(lbl, (startRank > 1 & startTimeGap < 1.5) |
								(endRank > 1 & updateTimeGap > 0 & updateTimeGap < 1.5))
lbl = lazy_left_join(lbl, raceDF, 'race', 'isValidRace30')

GetIntercept = function(raceDF, rddf) {

	for (yi in 1:length(unYear)) {
		
		### let's normalise the qualifying and race coefs
		raceDF$isCurrentYear = (raceDF$year == unYear[yi])
		
		currentYearRaceCoef = rddf %>%
							filter(year == unYear[yi] & !is.na(rawDCoef))

		mod = lm(rawDCoef ~ factor(circuit) + factor(driverTeamYear) - 1,
					data = currentYearRaceCoef)
		
		### good, now let's redo the driver coefs with the circuits put in as offsets
		circcoef=coef(mod)[grep('factor\\(circuit\\)',names(coef(mod)))]
		names(circcoef)=gsub('factor\\(circuit\\)','\\2',names(circcoef))
		
		### however, first driver has been omitted and taken to be zero - so need to readjust
		dum=grep('factor\\(driverTeamYear\\)',names(coef(mod)))
		mydcoef=c(0,coef(mod)[dum])
		
		### but we want those to be zero centered, and in that case, circuit coefs have to adjust:
		
		adjcirccoef=circcoef + mean(mydcoef)

		circCoefDF = tibble::enframe(adjcirccoef,
										name = 'circuit',
										value = 'circuitIntercept')

		raceDF = subset_join(raceDF,
								circCoefDF,
								'circuit',
								isCurrentYear)
	}

	return(raceDF)
}

NormaliseRawDCoef = function(modelDF) {

	rddf = left_join(rddf, modelDF, c('race', 'driver'))
	raceDF = GetIntercept(raceDF, rddf)
	rddf = lazy_left_join(rddf, raceDF, 'race', 'circuitIntercept')
	normalisedDCoef = with(rddf, rawDCoef - circuitIntercept)
	return(normalisedDCoef)
}

SmoothAndCalculateSqDiff = function(rddf, dCoefName, predNValidName, expectedFinPosName, sqDiffName) {
	
	dum = setNames(c(dCoefName, predNValidName, expectedFinPosName, sqDiffName),
					c('dCoefName', 'predNValidName', 'expectedFinPosName', 'sqDiffName'))
	DebugDisplay(dum)
	
	mySmooth = f1smoothing:::GetSmooth(qrToFit = 'r', qrToPredict = 'rfinpos',
										useStretch = FALSE, fwbw = 'flat',
								customSmoothInfo = list(qualRace = 'race',
														dCoefName = dCoefName,
														predNValidName = predNValidName),
								expectedFinPosName = expectedFinPosName)
	rddf = lazy_left_join(rddf, mySmooth$smoothDF, c('race', 'driver'), expectedFinPosName)

	rddf = rddf %>%
		mutate(!!sqDiffName := cleanliness * (officialFinishingPosition - get(expectedFinPosName))^2)

	return(rddf)
}

CompareModel = function(rddf, expectedFinPos1Name, expectedFinPos2Name,
								sqDiff1Name, sqDiff2Name) {

	dum = c(expectedFinPos1Name, expectedFinPos2Name,
								sqDiff1Name, sqDiff2Name)
	names(dum) = c('expectedFinPos1Name', 'expectedFinPos2Name',
								'sqDiff1Name', 'sqDiff2Name')
	DebugDisplay(dum)

	plot(rddf %>% pull(get(expectedFinPos1Name)), rddf %>% pull(get(expectedFinPos2Name)))
								
	rddf = rddf %>%
			mutate(isComparisonValid = !is.na(get(expectedFinPos1Name)) & !is.na(get(expectedFinPos2Name)))

	RMSEComparison = rddf %>%
						filter(isComparisonValid) %>%
						summarise(RMSE1 = sqrt(mean(get(sqDiff1Name))),
									RMSE2 = sqrt(mean(get(sqDiff2Name))))
	print(RMSEComparison)
									
	sqDiffDelta = with(rddf, (get(sqDiff1Name) - get(sqDiff2Name))[isComparisonValid])
	myConfInt = aaConfInt(sqDiffDelta)
	
	return(myConfInt)
}

MakeCountisValidNameFromLabel = function(validityName) {
	countName = paste0('predNValid', ToCamel(validityName))
	isValidName = paste0('isValid', ToCamel(validityName))
	return(list(countName = countName, isValidName = isValidName))
}

OldMakePredNValidForLblValid = function(lblCondition, minLapThreshold, validityName, rddf, lbl) {
	# contains completely pointless filter for minimum lap threshold
	lbl$isTrue = lblCondition
	dum = MakeCountisValidNameFromLabel(validityName)
	countName = dum$countName
	isValidName = dum$isValidName
	
	raceDriverCount = lbl %>%
						group_by(race, driver) %>%
						filter(isTrue) %>%
						summarise(!!countName := n())
	lbl = lazy_left_join(lbl, raceDriverCount, c('race', 'driver'), countName)
	lbl[,isValidName] = with(lbl, isTrue & get(countName) >= minLapThreshold)
	lbl = remove_column(lbl, c('isTrue', countName))
	# we'll be using this here too, so join it
	rddf = lazy_left_join(rddf, raceDriverCount, c('race', 'driver'), countName)
	rddf[,countName][is.na(rddf[,countName]) | (rddf[,countName] < minLapThreshold)] = 0

	return(list(rddf = rddf, lbl = lbl))
}

MakePredNValidForLblValid = function(isValidName, rddf, lbl) {
	# contains completely pointless filter for minimum lap threshold
	raceDriverCount = lbl %>%
						group_by(race, driver) %>%
						filter(get(isValidName)) %>%
						summarise(myCount = n())
	# we'll be using this here too, so join it
	rddf = lazy_left_join(rddf, raceDriverCount, c('race', 'driver'), 'myCount')
	rddf$myCount[is.na(rddf$myCount)] = 0

	return(rddf$myCount)
}

FitModelByRace = function(myRace, modelName, isValidName, lbl) {
	
	myLbl = lbl %>%
			filter(race == myRace & get(isValidName))
	if (modelName == 'mean') {
		mod = lm(sec ~ factor(driver) - 1, data = myLbl)
	}
	if (modelName == 'fuel') {
		mod = lm(sec ~ factor(driver) + fuel - 1, data = myLbl)
	}
	if (modelName == 'fuelTyreLap') {
		mod = lm(sec ~ factor(driver) + fuel + tyreLap - 1, data = myLbl)
	}
	if (modelName == 'fuelTyreTypeAndAge') {
		numTyre = length(unique(myLbl$tyre))
		if (numTyre == 1) {
			mod = lm(sec ~ factor(driver) + fuel + tyreLap - 1, data = myLbl)
		}
		if (numTyre > 1) {
			mod = lm(sec ~ factor(driver) + fuel + factor(tyre) * tyreLap - 1, data = myLbl)
		}
	}
	if (modelName == 'fuelTyreTypeAndAgeDWLockedIn') {
		numTyre = length(unique(myLbl$tyre))
		if (numTyre == 1) {
			mod = lm(sec ~ factor(driver) + fuel + tyreLap - 1, weight = lapWeight, data = myLbl)
		}
		if (numTyre > 1) {
			mod = lm(sec ~ factor(driver) + fuel + factor(tyre) * tyreLap - 1, weight = lapWeight, data = myLbl)
		}
	}

	driverCoefDF = tibble::enframe(coef(mod)[grep('factor\\(driver\\)', names(coef(mod)))],
									name = 'driver',
									value = 'rawDCoef')
	driverCoefDF$driver = gsub('.+\\)', '', driverCoefDF$driver)
	driverCoefDF$race = myRace
	
	# missingDriver = setdiff(with(myLbl, driver[isValid0]), driverCoefDF$driver)
	return(driverCoefDF)
}

RunModelAllRace = function(modelName, validityName, rddf, lbl) {
	
	dum = MakeCountisValidNameFromLabel(validityName)
	countName = dum$countName
	isValidName = dum$isValidName
	sqDiffName = paste0('sqDiff', ToCamel(modelName), ToCamel(validityName))
	
	modelDF = purrr::map_df(with(raceDF, race[isValidRace30]),
								FitModelByRace,
								modelName,
								isValidName,
								lbl)
	rddf$dCoef  = NormaliseRawDCoef(modelDF)
	assign('rddf', rddf, envir = globalenv())
	rddf = SmoothAndCalculateSqDiff(rddf, 'dCoef', countName, 'efp', sqDiffName)

	return(rddf)
}

MakeIsRogue = function(lbl, inlapOutlapAreRogue = TRUE) {
	lbl = f1data:::DetectWetTyreOnDryTrack(lbl)
	stintDF$isShortStint = with(stintDF, endLap - startLap <=2)
	lbl = lazy_left_join(lbl, stintDF, c('race', 'driver', 'stint'), 'isShortStint')

	lbl$isRogue = with(lbl, isShortStint |
							isSafetyCar | isWet | isRed | lap == 1 | isSCRestart |
							isWetTyreOnDryTrack)
	if (inlapOutlapAreRogue) {
		lbl$isRogue[with(lbl, which(inlap | outlap))] = TRUE
	}

	return(lbl)
}

MakeInTraffic = function(modelchoice, lbl) {
	if (modelchoice == 4) {
		lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = TRUE)
		lbl$inTraffic = with(lbl, preDeltaDidOt > 0 | preDeltaGotOt >0 | preDeltaGotLap > 0 |
							(startRank > 1 & SOLGap < 1.5))
	}

	if (modelchoice == 30) {
		lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE)
		lbl$wasBlocked = with(lbl, (startRank > 1 & SOLGap < 1.5) |
								(endRank > 1 & EOLGap > 0 & EOLGap < 1.5))
		lbl$inTraffic = with(lbl, didOt > 0 | gotOt >0 | gotLap > 0 | wasBlocked)
	}
	return(lbl)
}

MakeIsOutlier = function(modelchoice, lbl) {

	if (modelchoice == 30) {

		if (!'inTraffic' %in% names(lbl)) {
			lbl = f1validity:::MakeInTraffic(30, lbl)
		}

		rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, 4, 'race')
		lbl$mod4PredSec = f1laptimelm::MakePredSecFromNormalisedDCoef(lbl, rddf, 4, 'mod4DCoef')
		dum = f1smoothing::GetSmooth(qrToFit = 'qr',
								qrToPredict = 'r',
								modelChoice = 4,
								useStretch = TRUE,
								fwbw = 'fwbw')
		rddf = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'smoothDCoef')
		lbl$smoothedMod4PredSec =
			f1laptimelm::MakePredSecFromNormalisedDCoef(lbl, rddf, 4, 'smoothDCoef')
		lbl$predSecToUse = with(lbl, ifelse(!is.na(mod4PredSec), mod4PredSec, smoothedMod4PredSec))
		lbl$isOutlier30 = FALSE
		lbl$isOutlier30[with(lbl, which(sec > predSecToUse + 2 & !inTraffic & !isCarProblem))] = TRUE
		lbl$isOutlier30[with(lbl, which(sec > predSecToUse + 6 & inTraffic & !isCarProblem))] = TRUE
	}

	return(lbl)
}

MakeIsGoodPreValidRace = function(modelchoice, lbl, raceDF) {

	if (modelchoice == 4) {

		lbl = f1validity::MakeIsRogue(lbl)
		lbl = f1validity:::MakeInTraffic(4, lbl)
		lbl$isGoodPreValidRace = with(lbl, !isRogue & !isOutlier0 & !inTraffic)
	}

	if (modelchoice == 'carProblem') {
		lbl = f1validity::MakeIsRogue(lbl, inlapOutlapAreRogue = FALSE)
		lbl = lazy_left_join(lbl, rddf,	c('race', 'driver'), 'mod4PredNValid')
		lbl$driverHasDCoef = (lbl$mod4PredNValid > 0)
		lbl = lazy_left_join(lbl, raceDF, 'race', 'isValidRace4')
		lbl$isGoodPreValidRace = with(lbl, !isRogue &
										driverHasDCoef &
										isValidRace4)
	}
	if (modelchoice %in% c(30)) {
		lbl = f1validity::MakeIsRogue(lbl, inlapOutlapAreRogue = TRUE)
		lbl = f1validity:::MakeInTraffic(30, lbl)
		lbl = lazy_left_join(lbl, raceDF, 'race', 'isValidRace4')

		lbl = f1validity:::MakeIsOutlier(30, lbl)

		if (FALSE) {
		lbl$isGoodPreValidRace = with(lbl, !isRogue &
											!inTraffic &
											!isCarProblem &
											isValidRace4 &
											!isOutlier30)
		}
		if (TRUE) {
		lbl$isGoodPreValidRace = with(lbl, !isRogue &
											!inTraffic &
											isValidRace4 &
											!isOutlier30)
		}
	}

	return(lbl)
}

UpdateTyreValidity = function(modelchoice) {

	LoadAllData()

	lbl = f1validity::MakeIsGoodPreValidRace(modelchoice, lbl, raceDF)

	modelLabel = f1data:::MakeModelToUseName(paste0('tyreValidity', modelchoice), modelchoice)

	raceToUpdate = with(raceDF, race[!get(modelLabel$doneTyreValidity)])

	if (length(raceToUpdate) > 0) {
		for (myRace in raceToUpdate) {
			numStintByTyre = f1validity::CalculateTyreValidity(myRace, lbl) %>%
								mutate(race = myRace)
			numStintByTyre = f1data:::RenameModelColumn(modelLabel, numStintByTyre)
			sqlLazyUpdate(numStintByTyre,
						'racetyre',
						c('race', 'tyre'),
						with(modelLabel, c(numStint, isValidTyre)))
			raceDF[which(raceDF$race == myRace), modelLabel$doneTyreValidity] = TRUE
			sqlLazyUpdate(raceDF[which(raceDF$race == myRace),],
						'race',
						'race',
						modelLabel$doneTyreValidity)
		}
	}

	return(NULL)
}

MakeIsValidTyre = function(modelchoice, lbl, raceDF) {

	if (modelchoice %in% c(4, 'carProblem')) {
		isValidTyreName = 'isValidTyre4'
	}
	if (modelchoice %in% c(30)) {
		isValidTyreName = 'isValidTyre30'
	}
	# NB this loks fiddly, it's because we want to store 'isValidTyre4' obviously, but want to use isValidTyre in the subsequent code
	raceTyreDF = raceTyreDF %>%
					mutate(isValidTyre = get(isValidTyreName))
	lbl = lazy_left_join(lbl, raceTyreDF, c('race', 'tyre'), 'isValidTyre')

	return(lbl)
}

MakeIsValidRace = function(lbl, raceDF) {

	anyTyreValid = lbl %>%
					group_by(race) %>%
					summarise(isValidRace = sum(isGoodPreValidRace & isValidTyre) > 0)
	raceDF = lazy_left_join(raceDF, anyTyreValid, 'race', 'isValidRace')

	return(raceDF = raceDF)
}

UpdateValidity = function(modelchoice) {
	### this is often confusing and complicated, so these are the steps:
	# 1. get isGoodPreValidRace: we can't know which races are going to be valid to start with, so we determine on a lap by lap basis
	# 2. from that, work out which tyres are valid
	# 3. from that, work out which races are valid
	# 4. combine everything, isGood = isGoodPreValidRace & isValidTyre & isValidRace
	### firstly the bits that are common to all processes:
	# NB broken down into different functions because otherwise this function would have to call itself, e.g. you want validrace4 for postDelta, yuk

	# the tyre validity calculations take a long time, and alos it's necessary to have available quite a lot of columns that aren't necessarily used elsewhere. for this reason they're stored and retrieved from the database rather than calcualted from scratch every time (which was the original plan)

	LoadAllData()

	if (modelchoice %in% c(4, 30)) {
		modelLabel = f1data:::MakeModelToUseName(paste0('validity', modelchoice), modelchoice)
	}
	if (modelchoice == 'carProblem') {
		modelLabel = f1data:::MakeModelToUseName('validityCarProblem', 'CarProblem')
	}

	raceToUpdate = with(raceDF, race[!get(modelLabel$doneValidity)])

	if ( (length(raceToUpdate) > 0) ) {
		message('Need to update validity for model ', modelchoice,' for the following race(s):')
		print(raceToUpdate)

		## step 1, get the pre-valid race valid laps

		if (modelchoice == 'outlier0') {
			lbl$isGood = with(lbl, !isRogue)
			raceDF$isValidRace = TRUE
		}

		if (modelchoice %in% c(4, 'carProblem', 30)) {
			lbl = f1validity::MakeIsGoodPreValidRace(modelchoice, lbl, raceDF)
			lbl = f1validity::MakeIsValidTyre(modelchoice, lbl, raceDF)
			raceDF = f1validity::MakeIsValidRace(lbl, raceDF)
			lbl = lazy_left_join(lbl, raceDF, 'race', 'isValidRace')
			if (modelchoice %in% c(4, 'carProblem')) {
				lbl$isGood = with(lbl, isGoodPreValidRace & isValidTyre & isValidRace)
			}
			if (modelchoice == 30) {
				lbl$isGood = with(lbl, isGoodPreValidRace &
										isValidTyre &
										isValidRace &
										!isCarProblem)
				lbl$isCarProblemButGood30 = with(lbl, isGoodPreValidRace &
														isValidTyre &
														isValidRace &
														isCarProblem)
			}

			for (myRace in raceToUpdate) {
				raceRaceIndex = which(raceDF$race == myRace)
				lblRaceIndex = which(lbl$race == myRace)
				raceDF[raceRaceIndex,modelLabel$isValidRace] = raceDF$isValidRace[raceRaceIndex]
				raceDF[raceRaceIndex,modelLabel$doneValidity] = TRUE
				lbl[lblRaceIndex,modelLabel$isGood] = lbl$isGood[lblRaceIndex]
				sqlLazyUpdate(raceDF[raceRaceIndex,],
						'race',
						'race',
						c(modelLabel$isValidRace, modelLabel$doneValidity))
				sqlLazyUpdate(lbl[lblRaceIndex,],
						'racedriverlap',
						c('race', 'driver', 'lap'),
						modelLabel$isGood)
				if (modelchoice == 30) {
					sqlLazyUpdate(lbl[lblRaceIndex,],
						'racedriverlap',
						c('race', 'driver', 'lap'),
						'isCarProblemButGood30')
				}
				message('Have updated validity for model ', modelchoice,' for ', myRace)
			}
			# but what do we return? if you don't want the intermeidate columns, all you get is isGoodX and isValidRaceX, otherwise
			# no, that's the wrong way to think about this. this just updates the database, nothing more
			# current thinking: you get those things when you need them, we'll see how often that happens and modify accordingly then. it's not hard to make additional functions that just return isRogue, inTraffic etc, doesn't require major overhaul to do that
			# maybe there's a case for a 'RetrieveIsGood' function, because we might want several isGoods eg for carproblem model 30, overtaking etc. don't want all available by default
		}
	}

	return(NULL)
}

CalculateTyreValidity = function(myRace, lbl) {

	numIsGoodPreValidRace = with(lbl, sum(isGoodPreValidRace[race == myRace]))
	if (numIsGoodPreValidRace == 0) {
		numStintByTyre = tibble(tyre = with(raceTyreDF, tyre[race == myRace]),
								numStint = 0,
								totalOverlap = 0,
								sumInternalContrast = 0,
								isValidTyre = FALSE)
	}

	if (numIsGoodPreValidRace > 0) {
		currentRaceDriverTyre = lbl %>%
									filter(race == myRace & isGoodPreValidRace) %>%
									group_by(driver, tyre, stint) %>%
									summarise(numGood = n(),
												numGoodProp = 1 - exp(-0.2 * numGood))

		numStintByDriverTyre = currentRaceDriverTyre %>%
								group_by(driver, tyre) %>%
								summarise(numGoodPropTyre = sum(numGoodProp))
		# but were there some good internal constrasts? very convoluted to detect this
		# firstly, restrict to drivers who did >=2 stints on same tyre, obviously
		# then if they've done >2, take longest two
		# then multiply them togehter, that's how good the comparison is
		# there might not be any situations where drivers have done more 1 stint though
		moreThanOneStintDriverTyre = currentRaceDriverTyre %>%
											count(driver, tyre) %>%
											filter(n >= 2)
		if (nrow(moreThanOneStintDriverTyre) == 0) {
			internalContrast = tibble(tyre = unique(currentRaceDriverTyre$tyre),
										sumInternalContrast = 0)
		}
		if (nrow(moreThanOneStintDriverTyre) > 0) {
			internalContrastByDriverTyre = inner_join(currentRaceDriverTyre,
											moreThanOneStintDriverTyre %>%
												select(driver, tyre),
												c('driver','tyre')) %>%
											group_by(driver, tyre) %>%
											arrange(-numGoodProp) %>%
											mutate(numGoodPropRank = 1:n()) %>%
											filter(numGoodPropRank <= 2) %>%
											summarise(internalContrast = prod(numGoodProp)) %>%
											ungroup()
			# but mising levels are a pain, let's stick em in
			internalContrastByDriverTyre = complete(internalContrastByDriverTyre,
													expand(currentRaceDriverTyre,
															nesting(driver, tyre)),
													fill = list(internalContrast = 0))

			internalContrast = internalContrastByDriverTyre %>%
								group_by(tyre) %>%
								summarise(sumInternalContrast = sum(internalContrast))
		}

		numStintByTyre = currentRaceDriverTyre %>%
							group_by(tyre) %>%
							summarise(numStint = sum(numGoodProp))

		if (nrow(numStintByTyre) == 1) {
			numStintByTyre$totalOverlap = 0
		}
		# now cycle through the pair to get each tyre's overlap quality
		if (nrow(numStintByTyre) > 1) {
			allTyreCombo = as_tibble(t(combn(numStintByTyre$tyre, 2)))
			allTyreCombo$numOverlap = NA
			for (ci in 1:nrow(allTyreCombo)) {
				currentTyre = unlist(allTyreCombo[ci,c('V1','V2')])
				currentTyreNumStint = currentRaceDriverTyre %>%
								filter(tyre %in% currentTyre) %>%
								group_by(driver, tyre) %>%
								summarise(numStint = n()) %>%
								ungroup()
				horizCurrentTyreNumStint = spread_multiple(currentTyreNumStint %>%
									select(driver, tyre, numStint),
									key = 'tyre', value = numStint)
				tyrecolname = names(horizCurrentTyreNumStint)[grep('^tyre', names(horizCurrentTyreNumStint))]
				horizCurrentTyreNumStint[,tyrecolname][is.na(horizCurrentTyreNumStint[,tyrecolname])]=0

				numOverlap = horizCurrentTyreNumStint %>%
									summarise(numOverlap = sum(get(tyrecolname[1])>0 &
																get(tyrecolname[2]) > 0))
				allTyreCombo$numOverlap[ci] = numOverlap$numOverlap
			}

			# right, let's compress our little rule to return whether any tyres and hence the race, are good
			numStintByTyre$totalOverlap = NA
			for (ti in 1:nrow(numStintByTyre)) {
				numStintByTyre$totalOverlap[ti] =
					with(allTyreCombo, sum(numOverlap[V1 == numStintByTyre$tyre[ti] |
														V2 == numStintByTyre$tyre[ti]]))
			}
		}

		numStintByTyre = left_join(numStintByTyre,
									internalContrast,
									'tyre') %>%
							mutate(isValidTyre = (numStint > 5 & totalOverlap > 5) |
													sumInternalContrast > 5)
	}

	return(numStintByTyre)
}

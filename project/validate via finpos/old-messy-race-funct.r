MakeRetirementLap = function(rddf, lbl) {

	# find drivers who retired in the race, then don't include their slow inlaps to the pits in the freak slow inlaps
	rddf = rddf %>%
			mutate(isTime = grepl('[0-9]{1,2}\\.[0-9]{1,3}', classification),
					isLappedAtFinish = grepl('[0-9] Lap', classification),
					isRetirement = !(!is.na(officialFinishingPosition) &
										officialFinishingPosition == 1) &
										!isTime &
										!isLappedAtFinish) %>%
			select(-c(isTime, isLappedAtFinish))
	retirementRddf = rddf %>%
					filter(isRetirement) %>%
					select(race, driver, maxLap) %>%
					rename(lap = maxLap)
	lbl = lbl %>%
			indicate_overlapping_combination(
					retirementRddf,
					c('race', 'driver', 'lap'),
					'isRetirementLap')

	return(list(rddf = rddf, lbl = lbl))
}

MakeMFDelta = function(lbl) {

	# mmm, we really need the simulations for this
	rdlFile = paste0(USERPATH, 'project/validate via finpos/race-driver-lap-simulation.csv')
	raceDriverLapSimulation = read.csv(rdlFile, as.is = TRUE) %>%
								rename(race = racename) %>%
								rename(meanFinPos = mod34meanfinpos)

	lbl = left_join(lbl, raceDriverLapSimulation, c('race', 'driver', 'lap'))
	### ok, now we might have more joy with that

	dum = lbl %>%
			group_by(race, driver) %>%
			arrange(lap) %>%
			mutate(mfDelta = c(diff(meanFinPos),NA))

	lbl = lazy_left_join(lbl, dum, c('race','driver','lap'), 'mfDelta')

	return(lbl)
}

MakeInterrruptedPitStop = function() {
	interruptedPitStop = read.csv(file = paste0(USERPATH, 'project/validate via finpos/interrupted-stint.csv'), as.is = TRUE) %>%
	dplyr::rename(race = racename) %>%
	dplyr::rename(startLap = startlap)

	# so we want unscheduled stops that were not safety cars
	# no, data is bad there
}

DetectVerySlowPitStop = function(pitStopDF, raceDF, rddf) {

	ppitStopDF = lazy_left_join(stintDF, yearGuideDF, 'year', 'couldChangeTyresAndServePenalty')
	pitStopDF = lazy_left_join(pitStopDF, raceDF, 'race', 'medianPitStopTime')
	pitStopDF$isSlowPitStop = with(pitStopDF, pitStopTime > medianPitStopTime + 10)
	rddf = indicate_overlapping_combination(rddf,
											pitStopDF %>%
											filter(isSlowPitStop),
											c('race', 'driver'),
											'hadSlowPitStop')
	return(rddf)
}

DetectVerySlowLap = function(rddf, lbl, modelChoice) {
	if (modelChoice == 4) {
		lbl$myPredSec = lbl$mod4PredSec
	}
	if (modelChoice == 30) {
		lbl$myPredSec = lbl$mod30PredSec
	}
	lbl$isSlowInLap = with(lbl, inlap &
							impsec > myPredSec + 5 &
							(!isRetirementLap & !isSafetyCar & !isRed & !isWet))
	# why restrict to inlaps? any terrible lap is surely enough
	lbl$isVerySlowLap = with(lbl, !isRogue & impsec > myPredSec + 10)
	rddf = indicate_overlapping_combination(rddf,
											lbl %>% filter(isVerySlowLap),
											c('race', 'driver'),
											'hadVerySlowLap')
	rddf = rddf %>%
			mutate(hadVerySlowLap = hadVerySlowLap & !isRetirement)

	return(rddf)
}

DetectFirstLapIssue = function(rddf, lbl) {

	### lap 1 nightmares:
	lbl = lbl %>%
			mutate(isFirstLapDisaster = lap == 1 &
										((startRank < 15 & inlap) |
										(endRank - startRank > 10)))
	lbl = lbl %>%
			mutate(isFirstLapProblem = lap == 1 &
										(between(endRank - startRank, 6, 10)))
	rddf = indicate_overlapping_combination(rddf,
										lbl %>%
										filter(isFirstLapDisaster),
										c('race', 'driver'),
										'hadFirstLapDisaster')
	rddf = indicate_overlapping_combination(rddf,
										lbl %>%
										filter(isFirstLapProblem),
										c('race', 'driver'),
										'hadFirstLapProblem')

	return(rddf)
}

AlignCarProblem = function(rddf, carProblemDF) {
	rddf = indicate_overlapping_combination(rddf,
										carProblemDF,
										c('race', 'driver'),
										'hadCarProblem')
	return(rddf)
}

DetectSimulationIssue = function(rddf, lbl) {
	lbl = MakeMFDelta(lbl)

	## don't want to pick up examples of drivers retiring, that's boring
	lbl$isDisaster = with(lbl, mfDelta > 3 & !isRetirement)

	rddf = indicate_overlapping_combination(rddf,
										lbl %>% filter(isDisaster),
										c('race', 'driver'),
										'simIndicatesDisaster')

	return(rddf)
}

DetectBadQualifying = function(rddf, modelChoice) {

	if (modelChoice == 4) {
		qualiModelChoice = 'qual'
		myqrtofit = 'q'
	}
	if (modelChoice == 30) {
		myqrtofit = 'qr'
		qualiModelChoice = 4
	}
	dum = f1smoothing:::GetSmooth(qrToFit = myqrtofit,
									qrToPredict = 'q',
									useStretch = TRUE,
									fwbw = 'fwbw',
									modelChoice = qualiModelChoice,
									smoothDCoefName = 'qualSmoothDCoef')
	rddf = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'qualSmoothDCoef')
	rddf = rddf %>%
			group_by(race) %>%
			mutate(expectedQualPos = rank(qualSmoothDCoef)) %>%
			ungroup() %>%
			mutate(qualDelta = startingGrid - expectedQualPos) %>%
			select(-c(qualSmoothDCoef, expectedQualPos))

	return(rddf)
}

MakeCleanRace = function(rddf, modelChoice, includeIntermediateColumn = FALSE) {

	lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE)
	lbl = f1gaptrafficpitstop::AdjustLapTimeForPitStop(lbl)
	lbl = f1laptimelm:::MakePredSec(lbl, modelChoice)
	lbl = f1validity::MakeIsRogue(lbl)

	dum = MakeRetirementLap(rddf, lbl)
	rddf = dum$rddf
	lbl = dum$lbl
	lbl = lazy_left_join(lbl, rddf, c('race', 'driver'), 'isRetirement')

	rddf = DetectSlowPitStop(pitStopDF, rddf)
	rddf = DetectVerySlowLap(rddf, lbl, modelChoice)
	rddf = DetectFirstLapIssue(rddf, lbl)
	rddf = AlignCarProblem(rddf, carProblemDF)
	rddf = DetectSimulationIssue(rddf, lbl)
	rddf = DetectBadQualifying(rddf, modelChoice)
	if (modelChoice == 4) {
		rddf = lazy_left_join(rddf, raceDF, 'race', 'isValidRace4') %>%
				rename(isValidRace = isValidRace4)
	}
	if (modelChoice == 30) {
		rddf = lazy_left_join(rddf, raceDF, 'race', 'isValidRace30') %>%
				rename(isValidRace = isValidRace30)
	}

	messinessColumn = c('qualMessiness', 'slowPitStopMessiness', 'carProblemMessiness',
						'verySlowLapMessiness', 'simIndicatesDisasterMessiness',
						'firstLapMessiness', 'retirementMessiness', 'invalidRaceMessiness')
	rddf = rddf %>%
		mutate(qualMessiness = case_when(
					qualDelta < 5 ~ 0,
					between(qualDelta, 5, 7) ~ 0.5,
					qualDelta > 7 ~ 1),
				slowPitStopMessiness = 0.5 * hadSlowPitStop,
				carProblemMessiness = 1 * hadCarProblem,
				verySlowLapMessiness = 0.5*hadVerySlowLap,
				simIndicatesDisasterMessiness = 1 * simIndicatesDisaster,
				firstLapMessiness = case_when(
									hadFirstLapDisaster ~ 1,
									hadFirstLapProblem ~ 0.5,
									TRUE ~ 0),
				retirementMessiness = 1 * isRetirement,
				invalidRaceMessiness = 1 * !isValidRace)
	rddf$cleanliness = 1 - apply(rddf[,messinessColumn], 1, max)

	# don't think you would ever want all the individual messiness columns so remove those in all cases
	rddf = remove_column(rddf, messinessColumn)

	if (!includeIntermediateColumn) {
		problemColumn = c('qualDelta', 'hadSlowPitStop', 'hadCarProblem', 'hadVerySlowLap',
						'simIndicatesDisaster', 'hadFirstLapDisaster', 'hadFirstLapProblem',
						'isRetirement', 'isValidRace')
		rddf = remove_column(rddf, problemColumn)
	}

	return(rddf)
}

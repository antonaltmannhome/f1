	# if race is totally wet, or if majority of drivers only have one complete stint on dry tyres, then race is not valid
	stintLengthCutoff = 5 # a stint must have that many clear laps to be valid
	numGT1StintCutoff = 8 # we want >= that number of drivers to have done two stints. that was 10, i've made it 8 just so that azerbaijan 2018 makes it in
	lbl$isGood = with(lbl, !inlap & !outlap &
							!isSafetyCar & !isWet & !isRed &
							lap != 1 & !isSCRestart)
	if (modelchoice %in% c(4, 'tyrevalidity4')) {
		CheckRequiredColumnsInDF(lbl, 'isOutlier0')
		lbl$isGood = with(lbl, isGood & !isOutlier0)
	}
	if (modelchoice == 30) {
		lbl = f1laptimelm::GetLMFitOutlier(lbl, 4)
		lbl$isGood = with(lbl, isGood & !isOutlier4)
	}
	raceDriverStintLength = lbl %>%
								group_by(race, driver, stint) %>%
								summarise(stintLength = sum(isGood)) %>%
								ungroup() %>%
								mutate(isLongEnoughStint = stintLength >= stintLengthCutoff)
	raceDriverNumGoodStint = raceDriverStintLength %>%
									group_by(race, driver) %>% 
									summarise(numStint = sum(isLongEnoughStint))
	numGT1GoodStint = raceDriverNumGoodStint %>%
					group_by(race) %>%
					summarise(numGoodStint = sum(numStint > 1),
								isValidRace = numGoodStint >= numGT1StintCutoff)
								
	raceDF = lazy_left_join(raceDF, numGT1GoodStint, 'race')

### slightly less old:	
	
MakeIsValidRace = function(modelchoice,
							raceDF,
							lbl = NULL) {

	CheckRequiredColumnsInDF(lbl, 'isPreValidRaceGood')
	
	if (modelchoice == 'outlier0') {
		raceDF$isValidRace = TRUE
	}
	
	if (modelchoice %in% c('tyrevalidity4', 'tyrevalidity30')) {
		stintLengthCutoff = 5 # a stint must have that many clear laps to be valid
		numGT1StintCutoff = 8 # we want >= that number of drivers to have done two stints. that was 10, i've made it 8 just so that azerbaijan 2018 makes it in
		raceDriverStintLength = lbl %>%
									group_by(race, driver, stint) %>%
									summarise(stintLength = sum(isPreValidRaceGood)) %>%
									ungroup() %>%
									mutate(isLongEnoughStint = stintLength >= stintLengthCutoff)
		raceDriverNumGoodStint = raceDriverStintLength %>%
										group_by(race, driver) %>% 
										summarise(numStint = sum(isLongEnoughStint))
		numGT1GoodStint = raceDriverNumGoodStint %>%
						group_by(race) %>%
						summarise(numGoodStint = sum(numStint > 1),
									isNumGoodStintValid = numGoodStint >= numGT1StintCutoff)
		raceDF = lazy_left_join(raceDF, numGT1GoodStint, 'race')

		raceDF$isValidRace = raceDF$isNumGoodStintValid
		raceDF = raceDF %>%
					select(-isNumGoodStintValid)
	}
	if (modelchoice == 4) {
		anyTyreValid = lbl %>%
						group_by(race) %>%
						summarise(isValidRace = sum(isPreValidRaceGood) > 0)
		raceDF = lazy_left_join(raceDF, anyTyreValid, 'race', 'isValidRace')
	}
	
	return(raceDF)
}


MakeIsGood = function(modelchoice, lbl, raceDF) {
	
	### firstly the bits that are common to all processes:
	lbl = f1data:::DetectWetTyreOnDryTrack(lbl)
	stintDF$isShortStint = with(stintDF, endLap - startLap <=2)
	lbl = lazy_left_join(lbl, stintDF, c('race', 'driver', 'stint'), 'isShortStint')
	lbl$isRogue = with(lbl, inlap | outlap | isShortStint |
							isSafetyCar | isWet | isRed | lap == 1 | isSCRestart |
							isWetTyreOnDryTrack)

	if (modelchoice == 'outlier0') {
		lbl$isPreValidRaceGood = with(lbl, !isRogue)
	}
	
	### then traffic calculation
	if (modelchoice == 'tyrevalidity4' | modelchoice == 4) {
		lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = TRUE)
		lbl$inTraffic = with(lbl, preDeltaDidOt > 0 | preDeltaGotOt >0 | preDeltaGotLap > 0 |
								(startRank > 1 & startTimeGap < 1.5))
	}
	if (modelchoice %in% c('postDelta', 'tyrevalidity30', 30)) {
		# we've not got impsec or overtaking stats for the races which weren't valid for model 4 so need to get hold of model 4 isvalidrace to tell us that
		CheckRequiredColumnsInDF(raceDF, 'isValidRace4')
		lbl = lazy_left_join(lbl, raceDF, 'race', 'isValidRace4')
		
		lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE)
		lbl$inTraffic = with(lbl, didOt > 0 | gotOt >0 | gotLap > 0 |
								(startRank > 1 & (startTimeGap < 1.5 | updateTimeGap < 1.5)))
	}

	### we determine validrace after the isGood, but also need a valid race before we can do that so in fact we need two version of isvalidrace
	if (modelchoice == 'tyrevalidity4') {
		CheckRequiredColumnsInDF(lbl, 'isOutlier0')
		lbl$isPreValidRaceGood = with(lbl, !isRogue & !isOutlier0 & !inTraffic)
	}
	if (modelchoice == 4) {
		CheckRequiredColumnsInDF(lbl, 'isOutlier0')
		lbl = lazy_left_join(lbl, raceTyreDF, c('race', 'tyre'), 'isValidTyre4')
		lbl$isPreValidRaceGood = with(lbl, !isRogue & !isOutlier0 & !inTraffic &
											isValidTyre4)
	}
	if (modelchoice == 'postDelta') {
		CheckRequiredColumnsInDF(lbl, 'isOutlier0')
		lbl$isGood = with(lbl, !isRogue & !isOutlier0 &	!inTraffic)
	}
	if (modelchoice == 'tyrevalidity30') {
		CheckRequiredColumnsInDF(lbl, 'isOutlier0')
		lbl$isPreValidRaceGood = with(lbl, !isRogue &
											!isOutlier0 &
											!inTraffic &
											!isCarProblem &
											isValidRace4)
	}
	if (modelchoice == 30) {
		lbl = lazy_left_join(lbl, raceTyreDF, c('race', 'tyre'), 'isValidTyre30')
		lbl$isPreValidRaceGood = with(lbl, !isRogue & !isOutlier0 & !inTraffic &
											isValidTyre4)
		CheckRequiredColumnsInDF(lbl, c('startTimeGap', 'updateTimeGap', 'didOt', 'gotOt', 'gotLap'))
		lbl$inTraffic = with(lbl, didOt > 0 | gotOt >0 | gotLap > 0 |
								(startRank > 1 & (startTimeGap < 1.5 | updateTimeGap < 1.5)))
		lbl = f1laptimelm::GetLMFitOutlier(lbl, 4)
		lbl$isGood = with(lbl, !inlap & !outlap & !isShortStint &
								!isSafetyCar & !isWet & !isRed &
								lap != 1 & !isSCRestart &
								!isOutlier4 &
								didOt == 0 & gotOt == 0 & gotLap == 0 &
								(startRank == 1 | (startRank > 1 & startTimeGap > 1.5 & updateTimeGap > 1.5)) &
								!isWetTyreOnDryTrack &
								!isCarProblem)
	}
	raceDF = MakeIsValidRace(modelchoice, raceDF, lbl)
	lbl = lazy_left_join(lbl, raceDF, 'race', 'isValidRace')
	lbl$isGood = with(lbl, isPreValidRaceGood & isValidRace)
	lbl = lbl %>% select(-c(isPreValidRaceGood, isValidRace))
	return(list(lbl = lbl,
				raceDF = raceDF))
}

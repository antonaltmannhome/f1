source('project/model hierarchy/model-hierarchy-setup.r')

### so let's work through a few models:

### all non-rogue laps:

lbl = f1validity::MakeIsRogue(lbl)
lbl = f1validity::MakeInTraffic(30, lbl)
lbl = lazy_left_join(lbl, raceDF, 'race', 'isValidRace30')

# exclude silly ones where driver did <5 laps
raceDriverNonRogueCount = lbl %>%
							group_by(race, driver) %>%
							filter(!isRogue & isValidRace30) %>%
							summarise(nonRogueCount = n())
lbl = left_join(lbl, raceDriverNonRogueCount, c('race', 'driver'))
lbl$isValidMeanNonRogue = with(lbl, !isRogue & nonRogueCount >= 5)
# we'll be using this here too, so join it
rddf = left_join(rddf, raceDriverNonRogueCount, c('race', 'driver'))
rddf$nonRogueCount[is.na(rddf$nonRogueCount)] = 0

modelDF = lbl %>%
			filter(isValidMeanNonRogue) %>%
			group_by(race, driver) %>%
			summarise(rawDCoef = mean(sec)) %>%
			ungroup()
rddf$dCoefMeanNonRogue = NormaliseRawDCoef(modelDF)
rddf$predNValidMeanNonRogue = with(rddf, ifelse(nonRogueCount >=5, nonRogueCount, 0))

rddf = SmoothAndCalculateSqDiff(rddf,
								'dCoefMeanNonRogue',
								'predNValidMeanNonRogue',
								'expectedFinPosMeanNonRogue',
								'sqDiffMeanNonRogue')

### what if we excldue car problem laps?

# exclude silly ones where driver did <5 laps
raceDriverNonRogueCarProblemCount = lbl %>%
							group_by(race, driver) %>%
							filter(!isRogue & isValidRace30 &!isCarProblem) %>%
							summarise(nonRogueCarProblemCount = n())
lbl = left_join(lbl, raceDriverNonRogueCarProblemCount, c('race', 'driver'))
lbl$isValidMeanNonRogueCarProblem = with(lbl, !isRogue & nonRogueCarProblemCount >= 5)
# we'll be using this here too, so join it
rddf = left_join(rddf, raceDriverNonRogueCarProblemCount, c('race', 'driver'))
rddf$nonRogueCarProblemCount[is.na(rddf$nonRogueCarProblemCount)] = 0

modelDF = lbl %>%
			filter(isValidMeanNonRogueCarProblem) %>%
			group_by(race, driver) %>%
			summarise(rawDCoef = mean(sec)) %>%
			ungroup()
rddf$dCoefMeanNonRogueCarProblem = NormaliseRawDCoef(modelDF)
rddf$predNValidMeanNonRogueCarProblem = 
	with(rddf, ifelse(nonRogueCarProblemCount >=5, nonRogueCarProblemCount, 0))

rddf = SmoothAndCalculateSqDiff(rddf,
								'dCoefMeanNonRogueCarProblem',
								'predNValidMeanNonRogueCarProblem',
								'expectedFinPosMeanNonRogueCarProblem',
								'sqDiffMeanNonRogueCarProblem')

# CompareModel(rddf, 'expectedFinPosMeanNonRogue', 'expectedFinPosMeanNonRogueCarProblem', 'sqDiffMeanNonRogue', 'sqDiffMeanNonRogueCarProblem') # not significantly better

# let's exclude all the blocked laps then, we know this gets worse:

# exclude silly ones where driver did <5 laps
raceDriverClearLapCount = lbl %>%
							group_by(race, driver) %>%
							filter(!isRogue & !inTraffic & isValidRace30) %>%
							summarise(clearLapCount = n())
lbl = left_join(lbl, raceDriverClearLapCount, c('race', 'driver'))
lbl$isValidMeanClearLap = with(lbl, !isRogue & !inTraffic & clearLapCount >= 5)
# we'll be using this here too, so join it
rddf = left_join(rddf, raceDriverClearLapCount, c('race', 'driver'))
rddf$clearLapCount[is.na(rddf$clearLapCount)] = 0

modelDF = lbl %>%
			filter(isValidMeanClearLap) %>%
			group_by(race, driver) %>%
			summarise(rawDCoef = mean(sec)) %>%
			ungroup()
rddf$dCoefMeanClearLap = NormaliseRawDCoef(modelDF)
rddf$predNValidMeanClearLap = with(rddf, ifelse(clearLapCount >=5, clearLapCount, 0))

rddf = SmoothAndCalculateSqDiff(rddf,
								'dCoefMeanClearLap',
								'predNValidMeanClearLap',
								'expectedFinPosMeanClearLap',
								'sqDiffMeanClearLap')
								
# CompareModel(rddf, 'expectedFinPosMeanNonRogue', 'expectedFinPosMeanClearLap', 'sqDiffMeanNonRogue', 'sqDiffMeanClearLap') # much worse

# now we add adjustment for fuel to both


FitFuelModelByRace = function(myRace, isValidName) {
	
	myLbl = lbl %>%
			filter(race == myRace & get(isValidName))
	mod = lm(sec ~ factor(driver) + fuel - 1, data = myLbl)
	driverCoefDF = tibble::enframe(coef(mod)[grep('factor\\(driver\\)', names(coef(mod)))],
									name = 'driver',
									value = 'rawDCoef')
	driverCoefDF$driver = gsub('.+\\)', '', driverCoefDF$driver)
	driverCoefDF$race = myRace
	
	# missingDriver = setdiff(with(myLbl, driver[isValid0]), driverCoefDF$driver)
	return(driverCoefDF)
}

### all non-rogue laps first:
modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelModelByRace, isValidName = 'isValidMeanNonRogue')
rddf$dCoefFuelNonRogue = NormaliseRawDCoef(modelDF)
rddf = SmoothAndCalculateSqDiff(rddf, 'dCoefFuelNonRogue', 'predNValidMeanNonRogue', 'efpFuelNonRogue', 'sqDiffFuelNonRogue')

### then filter down to clear laps
modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelModelByRace, isValidName = 'isValidMeanClearLap')
rddf$dCoefFuelClearLap = NormaliseRawDCoef(modelDF)

rddf = SmoothAndCalculateSqDiff(rddf, 'dCoefFuelClearLap', 'predNValidMeanClearLap', 'efpFuelClearLap', 'sqDiffFuelClearLap')



FitFuelTyreLapModelByRace = function(myRace, isValidName) {
	
	myLbl = lbl %>%
			filter(race == myRace & get(isValidName))
	mod = lm(sec ~ factor(driver) + fuel + tyreLap - 1, data = myLbl)
	driverCoefDF = tibble::enframe(coef(mod)[grep('factor\\(driver\\)', names(coef(mod)))],
									name = 'driver',
									value = 'rawDCoef')
	driverCoefDF$driver = gsub('.+\\)', '', driverCoefDF$driver)
	driverCoefDF$race = myRace
	
	# missingDriver = setdiff(with(myLbl, driver[isValid0]), driverCoefDF$driver)
	return(driverCoefDF)
}

modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelTyreLapModelByRace, 'isValidMeanNonRogue')
rddf$dCoefFuelTyreLapNonRogue = NormaliseRawDCoef(modelDF)
rddf = SmoothAndCalculateSqDiff(rddf, 'dCoefFuelTyreLapNonRogue', 'predNValidMeanNonRogue', 'efpFuelTyreLapNonRogue', 'sqDiffFuelTyreLapNonRogue')

modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelTyreLapModelByRace, 'isValidMeanClearLap')
rddf$dCoefFuelTyreLapClearLap = NormaliseRawDCoef(modelDF)
rddf = SmoothAndCalculateSqDiff(rddf, 'dCoefFuelTyreLapClearLap', 'predNValidMeanClearLap', 'efpFuelTyreLapClearLap', 'sqDiffFuelTyreLapClearLap')

FitFuelTyreTypeAndAgeModelByRace = function(myRace, isValidName) {
	
	myLbl = lbl %>%
			filter(race == myRace & get(isValidName))
	numTyre = length(unique(myLbl$tyre))
	if (numTyre == 1) {
		mod = lm(sec ~ factor(driver) + fuel + tyreLap - 1, data = myLbl)
	}
	if (numTyre > 1) {
		mod = lm(sec ~ factor(driver) + fuel + factor(tyre) * tyreLap - 1, data = myLbl)
	}
	driverCoefDF = tibble::enframe(coef(mod)[grep('factor\\(driver\\)', names(coef(mod)))],
									name = 'driver',
									value = 'rawDCoef')
	driverCoefDF$driver = gsub('.+\\)', '', driverCoefDF$driver)
	driverCoefDF$race = myRace
	
	# missingDriver = setdiff(with(myLbl, driver[isValid0]), driverCoefDF$driver)
	return(driverCoefDF)
}

modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelTyreTypeAndAgeModelByRace, 'isValidMeanNonRogue')
rddf$dCoefFuelTyreTypeAndAgeNonRogue = NormaliseRawDCoef(modelDF)
rddf = SmoothAndCalculateSqDiff(rddf, 'dCoefFuelTyreTypeAndAgeNonRogue', 'predNValidMeanNonRogue', 'efpFuelTyreTypeAndAgeNonRogue', 'sqDiffFuelTyreTypeAndAgeNonRogue')

modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelTyreTypeAndAgeModelByRace, 'isValidMeanClearLap')
rddf$dCoefFuelTyreTypeAndAgeClearLap = NormaliseRawDCoef(modelDF)
rddf = SmoothAndCalculateSqDiff(rddf, 'dCoefFuelTyreTypeAndAgeClearLap', 'predNValidMeanClearLap', 'efpFuelTyreTypeAndAgeClearLap', 'sqDiffFuelTyreTypeAndAgeClearLap')

# then actually model 30
modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelTyreTypeAndAgeModelByRace, 'isGood30')
rddf$dCoef30b = NormaliseRawDCoef(modelDF)
rddf = SmoothAndCalculateSqDiff(rddf, 'dCoef30b', 'mod30PredNValid', 'efp30b', 'sqDiff30b')

### what about actually model 30, which i think is what i've just done
rddf = f1laptimelm:::MakeNormDriverCoef(rddf, raceDF, 30, 'race')
rddf = SmoothAndCalculateSqDiff(rddf, 'mod30DCoef', 'mod30PredNValid', 'efp30', 'sqDiff30')

### what about we do include blocked laps but we don't include overtaking laps?

lbl = lazy_left_join(lbl, raceDF, 'race', 'isValidRace4')
lbl = f1validity:::MakeIsOutlier(30, lbl)
lbl = f1validity::MakeIsValidTyre(30, lbl, raceDF)
lbl$isOverTaking = with(lbl, didOt > 0 | gotOt > 0 | gotLap > 0)
lbl$isBlocked = with(lbl, (startRank > 1 & startTimeGap < 1.5) |
								(endRank > 1 & updateTimeGap > 0 & updateTimeGap < 1.5))

lbl$isBlockedOrClear = with(lbl, !isRogue & !isCarProblem & !isOutlier30 &
									isValidRace30 & isValidTyre &
									!isOverTaking)
raceDriverBOCCount = lbl %>%
							group_by(race, driver) %>%
							filter(isBlockedOrClear) %>%
							summarise(BOCCount = n())
lbl = left_join(lbl, raceDriverBOCCount, c('race', 'driver'))
lbl$isValidBOC = with(lbl, isBlockedOrClear & BOCCount >= 5)
# we'll be using this here too, so join it
rddf = left_join(rddf, raceDriverBOCCount, c('race', 'driver'))
rddf$BOCCount[is.na(rddf$BOCCount)] = 0
rddf$predNValidBOC = with(rddf, ifelse(BOCCount >=5, BOCCount, 0))

							
modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelTyreTypeAndAgeModelByRace, 'isValidBOC')
rddf$dCoefBOC = NormaliseRawDCoef(modelDF)
rddf = SmoothAndCalculateSqDiff(rddf, 'dCoefBOC', 'predNValidBOC', 'efpBOC', 'sqDiffBOC')
## but here are the raceNumobWgtCoef, crazy: c(2.42800898536614, 0.954667465625104, 1)

CompareModel(rddf, 'efpBOC', 'efpFuelTyreTypeAndAgeNonRogue', 'sqDiffBOC', 'sqDiffFuelTyreTypeAndAgeNonRogue')
# expectedFinPos1Name= "efpBOC"
# expectedFinPos2Name= "efpFuelTyreTypeAndAgeNonRogue"
# sqDiff1Name= "sqDiffBOC"
# sqDiff2Name= "sqDiffFuelTyreTypeAndAgeNonRogue"
# # A tibble: 1 x 2
    # RMSE1   RMSE2
    # <dbl>   <dbl>
# 1 1.97103 1.99339
# [1] "(-0.1619146, -0.0269051)"

# significantly better, nice

### ok, that's tons of models, now let's get the ranking:

allModelName = paste0('sqDiff', c('MeanNonRogue', 'MeanNonRogueCarProblem', 'MeanClearLap', 'FuelNonRogue', 'FuelClearLap', 'FuelTyreLapNonRogue', 'FuelTyreLapClearLap', 'FuelTyreTypeAndAgeNonRogue', 'FuelTyreTypeAndAgeClearLap', '30', '30b', 'BOC'))

rddf$isComparisonValid = complete.cases(rddf[,allModelName])

tibble::enframe(sort(colMeans(rddf[rddf$isComparisonValid, allModelName])))
# A tibble: 11 x 2
   name                               value
   <chr>                              <dbl>
 1 sqDiffFuelTyreTypeAndAgeNonRogue 3.97360
 2 sqDiff30                         3.99774
 3 sqDiff30b                        4.00097
 4 sqDiffFuelTyreLapNonRogue        4.01064
 5 sqDiffFuelTyreTypeAndAgeClearLap 4.05867
 6 sqDiffFuelNonRogue               4.06731
 7 sqDiffFuelTyreLapClearLap        4.07547
 8 sqDiffMeanNonRogueCarProblem     4.14301
 9 sqDiffMeanNonRogue               4.15702
10 sqDiffFuelClearLap               4.19324
11 sqDiffMeanClearLap               4.56767

### that was if you do fwbw downweight. if we do no time downweight (eg fwbw = 'flat'):

# A tibble: 11 x 2
   name                               value
   <chr>                              <dbl>
 1 sqDiffBOC                        4.00231
 2 sqDiffFuelTyreTypeAndAgeNonRogue 4.04791
 3 sqDiffFuelTyreLapNonRogue        4.08048
 4 sqDiff30                         4.10005
 5 sqDiff30b                        4.10285
 6 sqDiffFuelNonRogue               4.12602
 7 sqDiffFuelTyreTypeAndAgeClearLap 4.14174
 8 sqDiffFuelTyreLapClearLap        4.15638
 9 sqDiffMeanNonRogueCarProblem     4.18928
10 sqDiffMeanNonRogue               4.19545
11 sqDiffFuelClearLap               4.25756
12 sqDiffMeanClearLap               4.60132
> 
# so this suggests we're excluding too many laps. including all non=rogue laps is better than excluding traffic ones. but what about excluding just the overtaking laps then?
# although i would like to know what the difference is between 30 and sqDiffFuelTyreTypeAndAgeClearLap
# they ought to be the same, right? instead there's quite a big gain

# but what if we were to put less weight on the <1.5 seconds laps?

FitFuelTyreTypeAndAgeModelByRace = function(myRace, isValidName, blockWgt) {
	
	myLbl = lbl %>%
			filter(race == myRace & get(isValidName)) %>%
			mutate(lapWeight = ifelse(!isBlocked, 1, blockWgt))
	numTyre = length(unique(myLbl$tyre))
	if (numTyre == 1) {
		mod = lm(sec ~ factor(driver) + fuel + tyreLap - 1, weights = lapWeight, data = myLbl)
	}
	if (numTyre > 1) {
		mod = lm(sec ~ factor(driver) + fuel + factor(tyre) * tyreLap - 1, weights = lapWeight, data = myLbl)
	}
	driverCoefDF = tibble::enframe(coef(mod)[grep('factor\\(driver\\)', names(coef(mod)))],
									name = 'driver',
									value = 'rawDCoef')
	driverCoefDF$driver = gsub('.+\\)', '', driverCoefDF$driver)
	driverCoefDF$race = myRace
	
	# missingDriver = setdiff(with(myLbl, driver[isValid0]), driverCoefDF$driver)
	return(driverCoefDF)
}
							
modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelTyreTypeAndAgeModelByRace, 'isValidBOC', blockWgt = 1)
rddf$dCoefBOCWgt = NormaliseRawDCoef(modelDF)
rddf = SmoothAndCalculateSqDiff(rddf, 'dCoefBOCWgt', 'predNValidBOC', 'efpBOCWgt', 'sqDiffBOCWgt')

# just gets worse if you set the weight to anything < 1. in fact 1.5 is marginally better

# let's change the threshold to 1 second for blockage

blockThreshold = 0.3
lbl$isBlocked = with(lbl, (startRank > 1 & startTimeGap < blockThreshold) |
								(endRank > 1 & updateTimeGap > 0 & updateTimeGap < blockThreshold))
lbl$isBlockedOrClear = with(lbl, !isRogue & !isCarProblem & !isOutlier30 &
									isValidRace30 & isValidTyre &
									!isOverTaking & !isBlocked)
raceDriverBOCCount = lbl %>%
							group_by(race, driver) %>%
							filter(isBlockedOrClear) %>%
							summarise(BOCCount = n())
lbl = lazy_left_join(lbl, raceDriverBOCCount, c('race', 'driver'), 'BOCCount')
lbl$isValidBOC = with(lbl, isBlockedOrClear & BOCCount >= 5)
# we'll be using this here too, so join it
rddf = lazy_left_join(rddf, raceDriverBOCCount, c('race', 'driver'), 'BOCCount')
rddf$BOCCount[is.na(rddf$BOCCount)] = 0
rddf$predNValidBOC = with(rddf, ifelse(BOCCount >=5, BOCCount, 0))
modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelTyreTypeAndAgeModelByRace, 'isValidBOC', blockWgt = 1)
rddf$dCoefBOCWgt = NormaliseRawDCoef(modelDF)
rddf = SmoothAndCalculateSqDiff(rddf, 'dCoefBOCWgt', 'predNValidBOC', 'efpBOCWgt', 'sqDiffBOCWgt')

### no, that wants to be nothing, wants to include everything

## i'm getting a little bored with this. nothing is working as expected.
## let's either switch to downweighting the low incentive laps, or trying out the likelihood including blocked laps.
## simulations is higher up the queue, and i suspect is the more likely winner

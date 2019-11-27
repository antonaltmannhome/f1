### something i'm interested in, we'll revisit once we've ironed out a few of the problems with the model. but let's compare the various possible models at predicting finpos


LoadAllData()
suppressWarnings(library(data.table))
source('project/cleansmooth/smoothing-plus-finpos-opt-numobwgt.r')
source('project/cleansmooth/smoothing-admin-funct.r')
source('project/validate via finpos/messy-race-funct.r')

# NB there is some dodgy logic in here, e.g. using the smooth to predict messy qualifying, even though it's the smooth we're doing it to help

rddf = lazy_left_join(rddf, raceDF, 'race', 'circuit')

rddf = MakeCleanRace(rddf, 30)

smooth30 = f1smoothing:::GetSmooth('r', 'rfinpos', useStretch = FALSE, fwbw = 'fwbw',
							modelChoice = 30, myRace = '2018abudhabi',
							expectedFinPosName = 'expectedFinPos30')
rddf = lazy_left_join(rddf, smooth30$smoothDF, c('race', 'driver'), 'expectedFinPos30')
rddf = rddf %>%
		mutate(sqDiff30 = cleanliness * (officialFinishingPosition - expectedFinPos30)^2)

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
	
	mySmooth = f1smoothing:::GetSmooth(qrToFit = 'r', qrToPredict = 'rfinpos', useStretch = FALSE, fwbw = 'fwbw',
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

### model 1, just take mean of non-rogue lap times

lbl = f1validity::MakeIsRogue(lbl)
lbl = lazy_left_join(lbl, raceDF, 'race', 'isValidRace30')
# exclude silly ones where driver did <5 laps
raceDriverNonRogueCount = lbl %>%
							group_by(race, driver) %>%
							filter(!isRogue & isValidRace30) %>%
							summarise(nonRogueCount = n())
lbl = left_join(lbl, raceDriverNonRogueCount, c('race', 'driver'))
lbl$isValid0 = with(lbl, !isRogue & nonRogueCount >= 5)
# we'll be using this here too, so join it
rddf = left_join(rddf, raceDriverNonRogueCount, c('race', 'driver'))
rddf$nonRogueCount[is.na(rddf$nonRogueCount)] = 0

modelDF = lbl %>%
			filter(isValid0) %>%
			group_by(race, driver) %>%
			summarise(rawDCoef = mean(sec)) %>%
			ungroup()
rddf$dCoef0 = NormaliseRawDCoef(modelDF)
rddf$predNValid0 = with(rddf, ifelse(nonRogueCount >=5, nonRogueCount, 0))

rddf = SmoothAndCalculateSqDiff(rddf, 'dCoef0', 'predNValid0', 'expectedFinPos0', 'sqDiff0')

# hold on, some of these are 0 when they should be NA

CompareModel(rddf, 'expectedFinPos0', 'expectedFinPos30', 'sqDiff0', 'sqDiff30')
### ok, proper model is now better now numob is looking more reliable

# want to see this too (put tidily somewhere later)
rddf = rddf %>%
		mutate(expectedFinPos0 = ifelse(messiness < 1, expectedFinPos0, NA),
				expectedFinPos30 = ifelse(messiness < 1, expectedFinPos30, NA),
				delta0 = officialFinishingPosition - expectedFinPos0,
				delta30 = officialFinishingPosition - expectedFinPos30)
myXLim = range(c(rddf$delta0, rddf$delta30),na.rm=T)
par(mfrow = c(2, 1))
hist(rddf$delta0, br = 20, col = 'red', xlim = myXLim)
hist(rddf$delta30, br = 20, col = 'red', xlim = myXLim)
# no, they look the same
# t.test(abs(rddf$delta0) - abs(rddf$delta30)) # returns 0.0003307

# what about if we get rid of traffic laps too?
lbl = f1validity::MakeInTraffic(30, lbl)

modelDF = lbl %>%
			filter(isValid0 & !inTraffic) %>%
			group_by(race, driver) %>%
			summarise(rawDCoef = mean(sec),
						numClearLap = n()) %>%
			ungroup()
rddf$dCoef1 = NormaliseRawDCoef(modelDF)
rddf = lazy_left_join(rddf, modelDF, c('race', 'driver'), 'numClearLap')
rddf$numClearLap[is.na(rddf$numClearLap)] = 0
	

rddf = SmoothAndCalculateSqDiff(rddf, 'dCoef1', 'numClearLap', 'expectedFinPos1', 'sqDiff1')

CompareModel(rddf, 'expectedFinPos1', 'expectedFinPos30', 'sqDiff1', 'sqDiff30')
# that's terrible. maybe we're filtering out too many laps then? or there's something subtle i'm missing

### let's try qualifying now

rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, 30, 'qual')

mySmooth = GetSmooth(qrToFit = 'r', qrToPredict = 'rfinpos', useStretch = FALSE, fwbw = 'fwbw',
				customSmoothInfo = list(qualRace = 'race',
										dCoefName = 'mod30QualDCoef',
										predNValidName = 'modQualPredNValid'),
				expectedFinPosName = 'efpQual')
rddf = lazy_left_join(rddf, mySmooth$smoothDF, c('race', 'driver'), 'efpQual')
rddf = rddf %>%
		mutate(sqDiffQual := (1 - messiness) * (officialFinishingPosition - efpQual)^2)
CompareModel(rddf, 'efpQual', 'expectedFinPos30', 'sqDiffQual', 'sqDiff30')
# so we're better than qualifying, woohoo

### now let's do just the fuel adjustment - again only filtering rogues

FitFuelModelByRace = function(myRace) {
	
	myLbl = lbl %>%
			filter(race == myRace & isValid0)
	mod = lm(sec ~ factor(driver) + fuel - 1, data = myLbl)
	driverCoefDF = tibble::enframe(coef(mod)[grep('factor\\(driver\\)', names(coef(mod)))],
									name = 'driver',
									value = 'rawDCoef')
	driverCoefDF$driver = gsub('.+\\)', '', driverCoefDF$driver)
	driverCoefDF$race = myRace
	
	# missingDriver = setdiff(with(myLbl, driver[isValid0]), driverCoefDF$driver)
	return(driverCoefDF)
}

modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelModelByRace)
rddf$dCoefFuel = NormaliseRawDCoef(modelDF)

rddf = SmoothAndCalculateSqDiff(rddf, 'dCoefFuel', 'predNValid0', 'efpFuel', 'sqDiffFuel')

CompareModel(rddf, 'efpFuel', 'expectedFinPos30', 'sqDiffFuel', 'sqDiff30')


FitFuelTyreLapModelByRace = function(myRace) {
	
	myLbl = lbl %>%
			filter(race == myRace & isValid0)
	mod = lm(sec ~ factor(driver) + fuel + tyreLap - 1, data = myLbl)
	driverCoefDF = tibble::enframe(coef(mod)[grep('factor\\(driver\\)', names(coef(mod)))],
									name = 'driver',
									value = 'rawDCoef')
	driverCoefDF$driver = gsub('.+\\)', '', driverCoefDF$driver)
	driverCoefDF$race = myRace
	
	# missingDriver = setdiff(with(myLbl, driver[isValid0]), driverCoefDF$driver)
	return(driverCoefDF)
}

modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelTyreLapModelByRace)
rddf$dCoefFuelTyreLap = NormaliseRawDCoef(modelDF)

rddf = SmoothAndCalculateSqDiff(rddf, 'dCoefFuelTyreLap', 'predNValid0', 'efpFuelTyreLap', 'sqDiffFuelTyreLap')

CompareModel(rddf, 'efpFuelTyreLap', 'expectedFinPos30', 'sqDiffFuelTyreLap', 'sqDiff30')

# fascintaing stuff. but will need to redo once we've got the proper numob downweighting in

## now let's get hold of supertimes
sessionTimeDF = read.csv(paste0(USERPATH, 'project/validate via finpos/session-time.csv'),
							as.is = TRUE)
# now join in race fastest lap

### oh but then there's possibly outliers, forget it for now

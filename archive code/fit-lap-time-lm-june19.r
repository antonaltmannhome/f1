
CleanLMCoef = function(myMod,
						driverCoefName = 'driver',
						fuelCoefName = 'fuel',
						tyreCoefName = 'tyre',
						tlapCoefName = 'tyreLap',
						numberOfTyre,
						onlyTyre = NULL) {

	# don't remove the intercept
	if (any(grepl('^\\(Intercept\\)$', names(coef(myMod))))) {
		stop('You have included the intercept, get rid of it!\n')
	}

	# lm output is always horrible, i do this all the time, let's have clean way of getting coefs out
	uglyDriverCoef = coef(myMod)[grep(paste('factor\\(',driverCoefName, sep = ''), names(coef(myMod)))]
	names(uglyDriverCoef) = gsub('^.+\\)', '', names(uglyDriverCoef))

	driverCoefDF = tibble::enframe(uglyDriverCoef, name = 'driver', value = 'coef')
	driverCoefDF = driverCoefDF %>% arrange(coef)

	if (numberOfTyre == 1) {
		myTyre = onlyTyre
		tyreCoefInt = 0
		tyreCoefSlope = coef(myMod)[['tyreLap']]
	}

	if (numberOfTyre > 1) {
		tyrePhrase = paste('factor\\(',tyreCoefName, '\\)[a-z]+$', sep = '')
		tlapPhrase = paste('factor\\(', tyreCoefName, '\\)[a-z]+:', tlapCoefName, '$', sep = '')
		myTyre = myMod$xlevels[[paste('factor(', tyreCoefName,')', sep = '')]]
		uglyTyreCoef = coef(myMod)[grep(tyrePhrase, names(coef(myMod)))]
		names(uglyTyreCoef) = gsub('^.+\\)', '', names(uglyTyreCoef))
		uglyTlapReferenceCoef = coef(myMod)[grep(paste('^', tlapCoefName, '$', sep = ''), names(coef(myMod)))]
		uglyTlapCoef = coef(myMod)[grep(tlapPhrase, names(coef(myMod)))]
		names(uglyTlapCoef) = gsub('(factor\\(tyre[^\\)]*\\))([^:]+)(:.+$)','\\2',names(uglyTlapCoef))
		tyreCoefInt = c(0, as.numeric(uglyTyreCoef))
		tyreCoefSlope = as.numeric(c(uglyTlapReferenceCoef, uglyTlapReferenceCoef + uglyTlapCoef))
	}
	tyreCoefDF = tibble(tyre = myTyre, int = tyreCoefInt, slo = tyreCoefSlope)

	fuelCoef = as.numeric(coef(myMod)[grep(fuelCoefName, names(coef(myMod)))])

	return(list(driverCoefDF = driverCoefDF,
				fuelCoef = fuelCoef,
				tyreCoefDF = tyreCoefDF))
}

MakeNormDriverCoef = function(rddf, raceDF, modelchoice, qualOrRace) {

	if (qualOrRace == 'qual') {
		columnName = paste0('mod', ToCamel(modelchoice), 'QualDCoef')
		interceptName = paste0('mod', ToCamel(modelchoice), 'QualIntercept')
		rawCoefName = 'modQualRawDCoef'
	}
	if (qualOrRace == 'race') {
		columnName = paste0('mod', ToCamel(modelchoice), 'DCoef')
		interceptName = paste0('mod', ToCamel(modelchoice), 'Intercept')
		rawCoefName = paste0('mod', ToCamel(modelchoice), 'RawDCoef')
	}

	rddf = lazy_left_join(rddf, raceDF, 'race', interceptName)
	rddf = rddf %>%
			mutate(!!columnName := get(rawCoefName) - get(interceptName))

	# can't get dplyr to do this more readably unfortunately
	rddf = rddf[,setdiff(names(rddf), interceptName)]

	return(rddf)
}

CalculateFuelTyreEffect = function(lbl, modelchoice, includeIntermediateColumn = FALSE) {
	fuelCoefName = paste0('mod', modelchoice, 'FuelCoef')
	tyreIntCoefName = paste0('mod', modelchoice, 'TyreInt')
	tyreSlopeCoefName = paste0('mod', modelchoice, 'TyreSlope')

	lbl = lazy_left_join(lbl, raceDF, 'race', fuelCoefName)
	lbl = lazy_left_join(lbl, raceTyreDF, c('race', 'tyre'), c(tyreIntCoefName, tyreSlopeCoefName))

	lbl$fuelEffect = with(lbl, get(fuelCoefName) * fuel)
	lbl$tyreEffect = with(lbl, get(tyreIntCoefName) + get(tyreSlopeCoefName) * tyreLap)
	lbl$fuelTyreEffect = with(lbl, fuelEffect + tyreEffect)

	if (!includeIntermediateColumn) {
		lbl = lbl %>%
				remove_column(c(fuelCoefName, tyreIntCoefName, tyreSlopeCoefName, 'tyreEffect', 'fuelEffect'))
	}

	return(lbl)
}

MakePredSec = function(lbl, modelchoice, adjustForCarProblem = TRUE, includeIntermediateColumn = FALSE) {
	lbl = f1laptimelm::CalculateFuelTyreEffect(lbl, modelchoice, includeIntermediateColumn = includeIntermediateColumn)
	driverCoefName = paste0('mod', modelchoice, 'RawDCoef')
	predSecName = paste0('mod', modelchoice, 'PredSec')
	if (modelchoice == 4 | (modelchoice == 30 & !adjustForCarProblem) ) {
		lbl = lazy_left_join(lbl, rddf, c('race', 'driver'), driverCoefName)
	}

	if (modelchoice == 30 & adjustForCarProblem) {
		lbl = subset_join(lbl,
							rddf %>%
								select(race, driver, driverCoefName),
							c('race', 'driver'),
							!isCarProblem)
		carProblemDF = f1laptimelm::DeriveCarProblemCoef(carProblemDF, modelchoice)
		lbl = subset_join(lbl,
							carProblemDF %>%
								select(race, driver, driverCoefName),
							c('race', 'driver'),
							isCarProblem)
	}
	lbl = lbl %>%
			mutate(!!predSecName := get(driverCoefName) + fuelTyreEffect)

	if (!includeIntermediateColumn) {
		lbl = lbl %>%
				remove_column(c(driverCoefName, 'fuelTyreEffect'))
	}

	return(lbl)
}

MakePredSecFromNormalisedDCoef = function(lbl, rddf, modelchoice, driverCoefName, includeIntermediateColumn = FALSE) {
	# MakePredSec is for the most common operation, ie making fitted lap times from raw model
	# but sometimes you might have mixed things together to get a normalised coef, in that case you need to add the race intercepts back on, hence this function
	# why do you need 'modelchoice'? to provide intercepts/fuel/tyre coefs
	# note this function just returns the prediction column, not a lbl with a named column like its sister function
	lbl = f1laptimelm::CalculateFuelTyreEffect(lbl,
								modelchoice,
								includeIntermediateColumn = includeIntermediateColumn)
	lbl = lazy_left_join(lbl, rddf, c('race', 'driver'), driverCoefName)
	interceptName = paste0('mod', modelchoice, 'Intercept')
	lbl = lazy_left_join(lbl, raceDF, 'race', interceptName)
	lbl = lbl %>%
			mutate(predSec = get(interceptName) + get(driverCoefName) + fuelTyreEffect)

	if (!includeIntermediateColumn) {
		lbl = lbl %>%
				remove_column(c(interceptName, driverCoefName, 'fuelTyreEffect'))
	}

	return(lbl$predSec)
}

GetLapTimeLMIntercept = function(modelchoice) {

	LoadAllData()

	doneModelName = paste0('doneModel', modelchoice)
	rawDCoefName = paste0('mod', modelchoice, 'RawDCoef')
	raceInterceptName = paste0('mod', modelchoice, 'Intercept')
	predNValidName = paste0('mod', modelchoice, 'PredNValid')
	qualInterceptName = paste0('mod', modelchoice, 'QualIntercept')

	rddf = lazy_left_join(rddf, raceDF, 'race', 'circuit')

	### now we want to get an intercept using races that have taken place so far this season, and use that to create normalised driver coefficients (normalised throughout the season so can be compared from race to race)
	### but we want them to be comparable to the qualifying coefs - not trivial, since there won't be the same participants in each race as in each qualifying session - so we need to model them together

	myYear=with(raceDF, unique(year[!get(doneModelName)]))

	for (yi in 1:length(myYear)) {

		### let's normalise the qualifying and race coefs
		raceDF$isCurrentYear = (raceDF$year == myYear[yi])

		currentYearRaceCoef = rddf %>%
							filter(year == myYear[yi] & get(predNValidName) > 0) %>%
							select(rr, driverTeamYear, circuit, rawDCoefName) %>%
							mutate(dcoef = get(rawDCoefName),
									rqcircuit = paste(circuit, 'r', sep = '-')) %>%
							remove_column(rawDCoefName)
		currentYearQualCoef = rddf %>%
							filter(year == myYear[yi] & modQualPredNValid > 0) %>%
							select(rr, driverTeamYear, circuit, modQualRawDCoef) %>%
							mutate(dcoef = modQualRawDCoef,
									rqcircuit = paste(circuit, 'q', sep = '-')) %>%
							select(-modQualRawDCoef)
		currentYearCombinedCoef = bind_rows(currentYearRaceCoef, currentYearQualCoef)

		mod = lm(dcoef ~ factor(rqcircuit) + factor(driverTeamYear) - 1,
					data = currentYearCombinedCoef)

		### good, now let's redo the driver coefs with the circuits put in as offsets
		qcirccoef=coef(mod)[grep('factor\\(rqcircuit\\).+q$',names(coef(mod)))]
		names(qcirccoef)=gsub('(factor\\(rqcircuit\\))([^-]+)(-q)','\\2',names(qcirccoef))
		rcirccoef=coef(mod)[grep('factor\\(rqcircuit\\).+r$',names(coef(mod)))]
		names(rcirccoef)=gsub('(factor\\(rqcircuit\\))([^-]+)(-r)','\\2',names(rcirccoef))

		### however, first driver has been omitted and taken to be zero - so need to readjust
		dum=grep('factor\\(driverTeamYear\\)',names(coef(mod)))
		mydcoef=c(0,coef(mod)[dum])

		### but we want those to be zero centered, and in that case, circuit coefs have to adjust:

		adjqcirccoef=qcirccoef + mean(mydcoef)
		adjrcirccoef=rcirccoef + mean(mydcoef)

		circCoefDF = full_join(tibble::enframe(adjqcirccoef, name = 'circuit', value = qualInterceptName),
								tibble::enframe(adjrcirccoef, name = 'circuit', value = raceInterceptName),
								by = 'circuit')

		raceDF = subset_join(raceDF,
								circCoefDF,
								'circuit',
								isCurrentYear)

		raceDF[which(raceDF$isCurrentYear), doneModelName] = TRUE
		sqlLazyUpdate(raceDF[which(raceDF$isCurrentYear),], 'race', 'race', doneModelName)
		if (FALSE) {
		sqlUpdate_multikey('race',
							doneModelName,
							'race',
							raceDF[which(raceDF$isCurrentYear), doneModelName],
							raceDF[which(raceDF$isCurrentYear), 'race'])
		}

		hasValidRaceInterceptIndex = with(raceDF, which(isCurrentYear & !is.na(get(raceInterceptName))))
		hasValidQualInterceptIndex = with(raceDF, which(isCurrentYear & !is.na(get(qualInterceptName))))
		sqlLazyUpdate(raceDF[hasValidRaceInterceptIndex,], 'race', 'race', raceInterceptName)
		sqlLazyUpdate(raceDF[hasValidQualInterceptIndex,], 'race', 'race', qualInterceptName)
		if (FALSE) {
		sqlUpdate_multikey('race',
							raceInterceptName,
							'race',
							raceDF[hasValidRaceInterceptIndex, raceInterceptName],
							raceDF[hasValidRaceInterceptIndex, 'race'])
		sqlUpdate_multikey('race',
							qualInterceptName,
							'race',
							raceDF[hasValidQualInterceptIndex, qualInterceptName],
							raceDF[hasValidQualInterceptIndex, 'race'])
		}
	}
}

FitLapTimeLM = function(modelchoice) {
	if (modelchoice == 4) {
		LoadAllData()
	}
	if (modelchoice == 30) {
		LoadAllData(omitPreDelta = TRUE)
		lbl = f1gaptrafficpitstop::AdjustLapTimeForPitStop(lbl)
	}

	modelLabel = f1data:::MakeModelToUseName(paste0('rawModel', modelchoice),
												modelchoice)
	isValidLabel = f1data:::MakeModelToUseName(paste0('validity', modelchoice),
												modelchoice)

	if (FALSE) {
	doneModelName = paste0('doneRawModel', modelchoice)
	modCoefName = paste0('mod', modelchoice, 'RawDCoef')
	predNValidName = paste0('mod', modelchoice, 'PredNValid')
	fuelCoefName = paste0('mod', modelchoice, 'FuelCoef')
	tyrePredNValidName = paste0('mod', modelchoice, 'TyrePredNValid')
	tyreIntCoefName = paste0('mod', modelchoice, 'TyreInt')
	tyreSlopeCoefName = paste0('mod', modelchoice, 'TyreSlope')
	isGoodName = paste0('isGood', modelchoice)
	isValidRaceName = paste0('isValidRace', modelchoice)
	}

	rrToModel = with(raceDF, rr[!get(modelLabel$doneRawModel)])

	if (length(rrToModel)>0) {

		for (ri in rrToModel) {

			# these help subset_join which likes conditions to be columns within the DF
			rddf$isCurrentRace = with(rddf, rr == ri)
			lbl$isCurrentRace = with(lbl, rr == ri)
			raceTyreDF$isCurrentRace = with(raceTyreDF, rr == ri)
			currentRaceIsValid = raceDF[ri,] %>% pull(isValidLabel$isValidRace)
			if (!currentRaceIsValid) {
				rddf = rddf %>%
						mutate_cond(isCurrentRace,
									!!modelLabel$modPredNValid := 0)
				raceTyreDF = raceTyreDF %>%
						mutate_cond(isCurrentRace,
									!!modelLabel$modTyrePredNValid := 0)
			}

			if (currentRaceIsValid) {

				if (modelchoice == 4) {
					myLbl = lbl %>% filter(isCurrentRace & get(isValidLabel$isGood))
				}
				if (modelchoice == 30) {
					# change this once we're happy to remove carproblem bit
					myLbl = lbl %>%
								filter(isCurrentRace & (isGood30 | isCarProblemButGood30))
				}
				myTyre = unique(myLbl$tyre)
				numberOfTyre = length(myTyre)

				if (modelchoice == 30) {
						myLbl$driverCarProb = with(myLbl, ifelse(!isCarProblem, driver, paste(driver, 'carprob')))
				}

				if (numberOfTyre == 1) {
					if (modelchoice == 4) {
						mod0 = lm(sec ~ factor(driver) + fuel + tyreLap - 1,
									data = myLbl)
					}
					if (modelchoice == 30) {
						mod0 = lm(sec ~ factor(driverCarProb) + fuel + tyreLap - 1,
									data = myLbl)
					}
					tidyCoef = f1laptimelm::CleanLMCoef(mod0,
														numberOfTyre = numberOfTyre,
														onlyTyre = myTyre)
				}

				if (numberOfTyre > 1) {
					if (modelchoice == 4) {
						mod0 = lm(sec ~ factor(driver) + fuel + factor(tyre) * tyreLap - 1,
									data = myLbl)
					}
					if (modelchoice == 30) {
						mod0 = lm(sec ~ factor(driverCarProb) + fuel + factor(tyre) * tyreLap - 1,
									data = myLbl)
					}

					tidyCoef0 = f1laptimelm::CleanLMCoef(mod0, numberOfTyre = numberOfTyre)

					# however, we want the fastest new tyre to have coef 0, so let's change the levels so that is the case

					fastestNewTyre = with(tidyCoef0$tyre, tyre[which.min(int)])
					tyreLevels = c(fastestNewTyre, setdiff(tidyCoef0$tyre$tyre, fastestNewTyre))

					if (modelchoice == 4) {
						mod = lm(sec ~ factor(driver) + fuel + factor(tyre, levels = tyreLevels) * tyreLap - 1, data = myLbl)
					}
					if (modelchoice == 30) {
						mod = lm(sec ~ factor(driverCarProb) + fuel + factor(tyre, levels = tyreLevels) * tyreLap - 1, data = myLbl)
					}

					tidyCoef = f1laptimelm::CleanLMCoef(mod,
											tyreCoefName = "tyre, levels = tyreLevels",
											numberOfTyre = numberOfTyre)
				}

				rddf = subset_join(rddf,
									tidyCoef$driver %>%
									mutate(!!modelLabel$modRawDCoef := coef) %>%
									select(-coef),
									'driver',
									isCurrentRace)

				### next step: add number of observations, can do that for isGood laps for tyres too and add a oclumn to racetyre for that e.g mod30prednvalid, mod4prednvalid

				if (modelchoice == 4) {
					numPredNValidLapByDriver = myLbl %>%
											group_by(driver) %>%
											summarise(!!modelLabel$modPredNValid := n())
				}
				if (modelchoice == 30) {
					# ghastly bit of code due to legacy of carproblem, fix when possible
					numPredNValidLapByDriver = myLbl %>%
											group_by(driverCarProb) %>%
											summarise(!!modelLabel$modPredNValid := n()) %>%
											rename(driver = driverCarProb)
				}
				numPredNValidLapByTyre = myLbl %>%
											group_by(tyre) %>%
											summarise(!!modelLabel$modTyrePredNValid := n())

				rddf = subset_join(rddf,
									numPredNValidLapByDriver,
									'driver',
									isCurrentRace)
				rddf = rddf %>%
						mutate_cond(isCurrentRace & !driver %in% tidyCoef$driver$driver,
									!!modelLabel$modPredNValid := 0)

				raceTyreDF = subset_join(raceTyreDF,
										numPredNValidLapByTyre,
										'tyre',
										isCurrentRace)
				raceTyreDF = raceTyreDF %>%
								mutate_cond(isCurrentRace & !tyre %in% tidyCoef$tyreCoefDF$tyre,
									!!modelLabel$modTyrePredNValid := 0)

				raceTyreDF = subset_join(raceTyreDF,
										tidyCoef$tyreCoefDF %>%
										mutate(!!modelLabel$modTyreInt := int,
												!!modelLabel$modTyreSlope := slo) %>%
										select(-c(int, slo)),
										'tyre',
										isCurrentRace)

				raceDF[ri, modelLabel$modFuelCoef] = tidyCoef$fuelCoef

				message('Have got coefficients for ', raceDF$race[ri])
			}
			raceDF[ri, modelLabel$doneRawModel] = TRUE
			print(ri)
		}
		sqlLazyUpdate(raceDF[rrToModel,], 'race', 'race', modelLabel$doneRawModel)
		sqlLazyUpdate(raceDF[rrToModel,] %>%
						filter(get(isValidLabel$isValidRace)),
						'race', 'race', modelLabel$modFuelCoef)

		haveDriverCoefIndex = with(rddf, which(rr %in% rrToModel & get(modelLabel$modPredNValid) > 0))
		sqlLazyUpdate(rddf[haveDriverCoefIndex,], 'racedriver', c('race', 'driver'), modelLabel$modRawDCoef)

		haveDriverCoefIndex = with(rddf, which(rr %in% rrToModel))
		sqlLazyUpdate(rddf[haveDriverCoefIndex,], 'racedriver', c('race', 'driver'), modelLabel$modPredNValid)

		tyreYesCoefIndex = with(raceTyreDF, which(rr %in% rrToModel & get(modelLabel$modTyrePredNValid) > 0))
		sqlLazyUpdate(raceTyreDF[tyreYesCoefIndex,], 'racetyre', c('race', 'tyre'), c(modelLabel$modTyreInt, modelLabel$modTyreSlope))
		if (FALSE) {
		sqlUpdate_multikey('racetyre',
							c(tyreIntCoefName, tyreSlopeCoefName),
							c('race', 'tyre'),
							raceTyreDF[tyreYesCoefIndex, c(tyreIntCoefName, tyreSlopeCoefName)],
							raceTyreDF[tyreYesCoefIndex, c('race', 'tyre')])
		}
		tyreAllCoefIndex = with(raceTyreDF, which(rr %in% rrToModel))
		sqlLazyUpdate(raceTyreDF[tyreAllCoefIndex,], 'racetyre', c('race', 'tyre'), modelLabel$modTyrePredNValid)
		if (FALSE) {
		sqlUpdate_multikey('racetyre',
							tyrePredNValidName,
							c('race', 'tyre'),
							raceTyreDF[tyreAllCoefIndex, tyrePredNValidName],
							raceTyreDF[tyreAllCoefIndex, c('race', 'tyre')])
		}
	}
}
# NB v helpful for debugging:	helpfulcol=c('race','driver','mod4qualdcoef','qualprednvalid','mod4qualsmdcoef','qualsmdnumob','mod4dcoef','mod4prednvalid','mod4smdcoef','mod4smdnumob')
### right, now we want to smooth them, both race and qualifying

GetLMFitOutlier = function(lbl, modelchoice) {

	isOutlierName = paste0('isOutlier', modelchoice)
	predSecName = paste0('mod', modelchoice, 'PredSec')
	lbl = MakePredSec(lbl, modelchoice)
	lbl[,isOutlierName] = with(lbl, !is.na(get(predSecName)) & sec - get(predSecName) > 2)
	return(lbl)
}

DeriveCarProblemCoef = function(carProblemDF, modelchoice) {
	### NB will need to modify this when a driver has two separate carproblem stints
	sumMultipleCarProblemStint = carProblemDF %>%
									count(race, driver) %>%
									summarise(sumMultipleStint = sum(n > 1)) %>%
									pull(sumMultipleStint)
	if (sumMultipleCarProblemStint > 0) {
		stop('DeriveCarProblemCoef can only handle single car problem stints for a race/driver combination, you need to modify the function\n')
	}
	
	rawModelLabel = f1data:::MakeModelToUseName(paste0('rawModel', modelchoice), modelchoice)
	modelLabel = f1data:::MakeModelToUseName(paste0('model', modelchoice), modelchoice)
	isValidLabel = f1data:::MakeModelToUseName(paste0('validity', modelchoice), modelchoice)
	modDCoefName = paste0('mod', modelchoice, 'DCoef')

	carProblemLbl = lbl %>%
					filter(isCarProblemButGood30)

	carProblemLbl = f1laptimelm:::CalculateFuelTyreEffect(carProblemLbl, modelchoice)
	carProblemDriverCoef = carProblemLbl %>%
							group_by(race, driver) %>%
							summarise(!!rawModelLabel$modRawDCoef := mean(sec - fuelTyreEffect)) %>%
							ungroup()
	# but we want the normalised one quite conceivably too
	carProblemDriverCoef = lazy_left_join(carProblemDriverCoef,
											raceDF,
											'race',
											modelLabel$modIntercept) %>%
							mutate(!!modDCoefName := get(rawModelLabel$modRawDCoef) -
													get(modelLabel$modIntercept))

	carProblemDF = lazy_left_join(carProblemDF,
									carProblemDriverCoef,
									c('race', 'driver'),
									c(rawModelLabel$modRawDCoef, modDCoefName))
	carProblemIsGoodCount = carProblemLbl %>%
							group_by(race, driver) %>%
							summarise(predNValid = n()) %>%
							ungroup %>%
							complete(
								expand(carProblemDF, nesting(race, driver)),
									fill = list(predNValid = 0)) %>%
							mutate(!!rawModelLabel$modPredNValid := predNValid) %>%
							select(-predNValid)

	carProblemDF = lazy_left_join(carProblemDF,
								carProblemIsGoodCount,
								c('race', 'driver'),
								rawModelLabel$predNValid)

	return(carProblemDF)
}

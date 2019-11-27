
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
	doneRawModelName = paste0('doneRawModel', modelchoice)
	rawDCoefName = paste0('mod', modelchoice, 'RawDCoef')
	raceInterceptName = paste0('mod', modelchoice, 'Intercept')
	predNValidName = paste0('mod', modelchoice, 'PredNValid')
	qualInterceptName = paste0('mod', modelchoice, 'QualIntercept')

	rddf = lazy_left_join(rddf, raceDF, 'race', 'circuit')

	### now we want to get an intercept using races that have taken place so far this season, and use that to create normalised driver coefficients (normalised throughout the season so can be compared from race to race)
	### but we want them to be comparable to the qualifying coefs - not trivial, since there won't be the same participants in each race as in each qualifying session - so we need to model them together

	rrToModel = with(raceDF, unique(rr[!get(doneModelName) & get(doneRawModelName)]))

	for (ri in rrToModel) {

		### let's normalise the qualifying and race coefs
		raceDF$isCurrentYear = (raceDF$year == raceDF$year[ri])

		currentYearRaceCoefSoFar = rddf %>%
							filter(year == raceDF$year[ri] & rr <= ri & get(predNValidName) > 0) %>%
							select(rr, driverTeamYear, circuit, rawDCoefName) %>%
							mutate(dcoef = get(rawDCoefName),
									rqcircuit = paste(circuit, 'r', sep = '-')) %>%
							remove_column(rawDCoefName)
		currentYearQualCoefSoFar = rddf %>%
							filter(year == raceDF$year[ri] & rr <= ri  & modQualPredNValid > 0) %>%
							select(rr, driverTeamYear, circuit, modQualRawDCoef) %>%
							mutate(dcoef = modQualRawDCoef,
									rqcircuit = paste(circuit, 'q', sep = '-')) %>%
							select(-modQualRawDCoef)
		currentYearCombinedCoefSoFar = bind_rows(currentYearRaceCoefSoFar,
																							currentYearQualCoefSoFar)

		mod = lm(dcoef ~ factor(rqcircuit) + factor(driverTeamYear) - 1,
					data = currentYearCombinedCoefSoFar)

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

		raceDF[which(with(raceDF, isCurrentYear & rr <= ri)), doneModelName] = TRUE
		sqlLazyUpdate(raceDF[which(raceDF$isCurrentYear),], 'race', 'race', doneModelName)

		hasValidRaceInterceptIndex = with(raceDF, which(isCurrentYear & !is.na(get(raceInterceptName))))
		hasValidQualInterceptIndex = with(raceDF, which(isCurrentYear & !is.na(get(qualInterceptName))))
		sqlLazyUpdate(raceDF[hasValidRaceInterceptIndex,], 'race', 'race', raceInterceptName)
		sqlLazyUpdate(raceDF[hasValidQualInterceptIndex,], 'race', 'race', qualInterceptName)
	}
	return(NULL)
}

FitLapTimeWithPrior = function(myRace, modelchoice, raceDF, rddf, lbl) {

  if (modelchoice == 4) {
    thisRaceLbl = lbl %>%
      filter(race == myRace & isGoodPreValidRace) %>%
      select(driver, fuel, tyre, tyreLap, sec) %>%
      mutate(isCarProblem = FALSE,
              driverCarProblem = paste(driver, 'FALSE'))
  }
  if (modelchoice == 30) {
    thisRaceLbl = lbl %>%
      filter(race == myRace & isGoodPreValidRace) %>%
      select(driver, isCarProblem, fuel, tyre, tyreLap, sec) %>%
      mutate(driverCarProblem = paste(driver, isCarProblem))
  }

  priorScale = list(driver = 0.1, tyre = 0.001, tyreLap = 50, fuel = 0.001)
  # priorScale = list(driver = 10, tyre = 100,tyreLap = 1000, fuel = 1000)

  thisRaceTyre = thisRaceLbl %>%
                  count(tyre) %>%
                  arrange(-n) %>%
                  pull(tyre)
  priorList = f1laptimelm:::MakePriorDF(myRace, thisRaceLbl, thisRaceTyre, raceDF, rddf, priorScale, 1)

  thisRaceLbl$tyreMap = match(thisRaceLbl$tyre, priorList$tyre$tyre)
  thisRaceLbl$driverMap = match(thisRaceLbl$driverCarProblem, priorList$driver$driverCarProblem)

  thisRaceLbl = lazy_left_join(thisRaceLbl, priorList$driver, 'driverCarProblem', 'prior') %>%
                  rename(driverPrior = prior)

  theta = c(0.05,
              rep(log(mean(thisRaceLbl$sec)), nrow(priorList$tyre)),
              0.1)

  phase1MaxInfo = optim(par = theta,
											f = f1laptimelm:::FixedDriverLikFunct,
											gr = f1laptimelm:::FixedDriverGradFunct,
                      priorList = priorList, thisRaceLbl = thisRaceLbl,
                      method = 'BFGS')
  phase1Coef = f1laptimelm:::ExtractCoefFromTheta(phase1MaxInfo$par, priorList, 1)
  intercept = phase1Coef$tyre[1]

  priorList = f1laptimelm:::MakePriorDF(myRace, thisRaceLbl, thisRaceTyre, raceDF, rddf, priorScale, 2)

  priorList$tyreLap$prior = phase1Coef$tyreLap

  # right, now we move the intercept term into priorList
  # why are we doing this? it's because for the driver priors to be in the right location, they need to have the appropriate intercept. so, we obtain the intercept that is appropriate for the driver priors in phase 1, then effectively force that to be the intercept for phase 2. thus the driver priors are in the right location in phase 2
  # but we also obtain the tyreLap prior into the bargain by doing this, which is useful for the occasional race where e.g a driver does 20 laps, almost all in traffic, on a tyre that has very few clear laps on it thus its tyreLap coef is liable to be wild. e.g mexico 2018. that would be a problem for the simulations


  theta = c(rep(0, nrow(priorList$driver)),
            0.05,
            rep(0, nrow(priorList$tyre) - 1),
            rep(0.1, nrow(priorList$tyre)))

  # nlm is sturggling with the maximisation, think we need to switch to optim
  phase2MaxInfo = optim(par = theta,
												f = f1laptimelm:::LapTimeLikFunct,
												gr = f1laptimelm:::LapTimeGradFunct,
                        priorList = priorList, thisRaceLbl = thisRaceLbl,
                        intercept = intercept,
                        method = 'BFGS')
  phase2MaxCoef = f1laptimelm:::ExtractCoefFromTheta(phase2MaxInfo$par, priorList, 2, intercept = intercept)
  phase2Hessian = numDeriv::hessian(LapTimeLikFunct, x = phase2MaxInfo$par,
                                    priorList = priorList,
                                    thisRaceLbl = thisRaceLbl,
                                    intercept = intercept)
  tempStdError = sqrt(diag(solve(phase2Hessian)))
  phase2StandardError = f1laptimelm:::ExtractCoefFromTheta(tempStdError, priorList, 2, intercept = 0)

  driverCoefDF = tibble(driverCarProblem = priorList$driver$driverCarProblem,
                        coef = phase2MaxCoef$driver)
  # but we want the intercept added back in for now, which is silly but it would require a major overhaul to avoid
  driverCoefDF$coef = driverCoefDF$coef + intercept
  driverCoefDF$stdError = phase2StandardError$driver
  driverCoefDF = left_join(driverCoefDF,
                           thisRaceLbl %>% count(driverCarProblem),
                           'driverCarProblem')
	driverCoefDF = driverCoefDF %>%
									tidyr::separate(driverCarProblem,
																	c('driver', 'carProblem'),
																	convert = TRUE)
  # the standard errors are bigger than with lm. but that might not be a bad thing, lm might be doing some nonsense to do with comparison to the base level
  # as long as we consider them in relative terms maybe it's not such a problem
  # we've yet to ever actually use it
	# just to get on with things, we'll now ditch the standard errors so that the join to rddf is easier later. also, we get carproblem coefs separately anyway later
	driverCoefDF = driverCoefDF %>%
									filter(!carProblem) %>%
									select(driver, coef, n)

  fuelCoef = phase2MaxCoef$fuel
  tyreCoefDF = tibble(tyre = priorList$tyre$tyre,
                      int = phase2MaxCoef$tyre - intercept,
                      slo = phase2MaxCoef$tyreLap) %>%
								left_join(thisRaceLbl %>% count(tyre), 'tyre')

	# but one final thing...we want the fastest new tyre to have intercept of 0, so need to adjust then and driver coefs appropriately
	tyreOffset = min(tyreCoefDF$int)
	tyreCoefDF$int = tyreCoefDF$int - tyreOffset
	driverCoefDF$coef = driverCoefDF$coef + tyreOffset

  return(list(driverCoefDF = driverCoefDF,
                fuelCoef = fuelCoef,
                tyreCoefDF = tyreCoefDF))
}

RenameColumnForModel = function(modelOutput, modelLabel) {
	modelOutput$driverCoefDF = modelOutput$driverCoefDF %>%
															rename(!!modelLabel$modRawDCoef := coef,
																			!!modelLabel$modPredNValid := n)
	modelOutput$tyreCoefDF = modelOutput$tyreCoefDF %>%
															rename(!!modelLabel$modTyreInt := int,
																			!!modelLabel$modTyreSlope := slo,
																			!!modelLabel$modTyrePredNValid := n)
	return(modelOutput)
}

UpdateDatabaseForRawModel = function(ri, raceDF, raceTyreDF, rddf,
																			currentRaceIsValid, isValidLabel, modelLabel) {

		sqlLazyUpdate(raceDF[ri,], 'race', 'race', modelLabel$doneRawModel)

		thisRaceDriverIndex = with(rddf, which(rr == ri))
		sqlLazyUpdate(rddf[thisRaceDriverIndex,], 'racedriver', c('race', 'driver'),
																		modelLabel$modPredNValid)

		if (currentRaceIsValid) {
			sqlLazyUpdate(raceDF[ri,] %>%
						filter(get(isValidLabel$isValidRace)),
						'race', 'race', modelLabel$modFuelCoef)

			haveDriverCoefIndex = with(rddf, which(rr == ri & get(modelLabel$modPredNValid) > 0))
			sqlLazyUpdate(rddf[haveDriverCoefIndex,], 'racedriver', c('race', 'driver'), modelLabel$modRawDCoef)

			thisRaceTyreIndex = with(raceTyreDF, which(rr == ri & get(modelLabel$modTyrePredNValid) > 0))
			sqlLazyUpdate(raceTyreDF[thisRaceTyreIndex,], 'racetyre', c('race', 'tyre'), c(modelLabel$modTyreInt, modelLabel$modTyreSlope,	modelLabel$modTyrePredNValid))
		}
}

ProcessLapTimeModel = function(modelchoice) {
	if (modelchoice == 4) {
		LoadAllData()
	}
	if (modelchoice == 30) {
		LoadAllData(omitPreDelta = TRUE)
	  # do you need to actualyl do this? i thought they all got filtered out
		lbl = f1gaptrafficpitstop::AdjustLapTimeForPitStop(lbl)
	}

	modelLabel = f1data:::MakeModelToUseName(paste0('rawModel', modelchoice),
												modelchoice)
	isValidLabel = f1data:::MakeModelToUseName(paste0('validity', modelchoice),
												modelchoice)

	# need to run the smoother for all races that have been done so far

	# need to know final race for which the model has been fit
	#finalFittedRace = with(raceDF, race[which.max(daynum[which(get(modelLabel$doneRawModel))])])
	lbl = f1validity:::MakeIsGoodPreValidRace(modelchoice, lbl, raceDF)
	raceDF = f1laptimelm:::MakeFuelPrior(modelchoice, lbl, raceDF, rddf)

	rrToModel = with(raceDF, rr[!get(modelLabel$doneRawModel)])

	if (length(rrToModel)>0) {

		for (ri in rrToModel) {

			# driver priors aren't amssively important except in extreme edge cases, we rely on qualifying to get them. could be clever with adjline correction/stretch etc, it's just not important enough to worry about though
			rddf = f1laptimelm:::GetDriverPrior(rddf, raceDF$year[ri])
			rddf$isCurrentRace = with(rddf, rr == ri)
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
				modelOutput = f1laptimelm:::FitLapTimeWithPrior(raceDF$race[ri], modelchoice, raceDF, rddf, lbl)

				modelOutput = f1laptimelm:::RenameColumnForModel(modelOutput, modelLabel)

				rddf = subset_join(rddf,
														modelOutput$driverCoefDF,
														'driver',
														isCurrentRace)
				rddf = rddf %>%
						mutate_cond(isCurrentRace & !driver %in% modelOutput$driverCoefDF$driver,
									!!modelLabel$modPredNValid := 0)

				raceTyreDF = subset_join(raceTyreDF,
										modelOutput$tyreCoefDF,
										'tyre',
										isCurrentRace)
				# could this every actually happen? i think yes, if a tyre is exclusively used in !isGood laps
				raceTyreDF = raceTyreDF %>%
								mutate_cond(isCurrentRace & !tyre %in% modelOutput$tyreCoefDF$tyre,
									!!modelLabel$modTyrePredNValid := 0)

				raceDF[ri, modelLabel$modFuelCoef] = modelOutput$fuelCoef

				message('Have got coefficients for ', raceDF$race[ri])
			}
			raceDF[ri, modelLabel$doneRawModel] = TRUE

			f1laptimelm:::UpdateDatabaseForRawModel(ri, raceDF, raceTyreDF, rddf,
																currentRaceIsValid, isValidLabel, modelLabel)

			### we need to update the intercept right way because the next race will require smoothing of the current one
			#raceDF = GetLapTimeLMIntercept(modelchoice, raceDF, rddf)
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

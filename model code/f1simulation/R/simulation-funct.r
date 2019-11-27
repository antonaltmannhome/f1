
GetSimulationConstant = function() {

	blockedOvertakingModelCoef = f1blockedovertakingmodel:::GetBlockedOvertakingModelCoef()
	overtakingCoefDF = blockedOvertakingModelCoef$overtakingCoefDF
	overtakingOverlapCoef = with(overtakingCoefDF, mle[coefType == 'overlap'])
	# these ones might look unnecessary but save having lots of very long variable names:
	didOvertakeCost = blockedOvertakingModelCoef$didOvertakeCost
	gotOvertakenCost = blockedOvertakingModelCoef$gotOvertakenCost
	blockedMeanCoef = blockedOvertakingModelCoef$blockedMeanCoef
	blockedVarCoef = blockedOvertakingModelCoef$blockedVarCoef

	dum = f1simulation:::GetInRunningWeightCoef()
	dCoefSourceWeightCoef = dum$dCoefSourceWeightCoef
	varByTotalWeightCoef = dum$varByTotalWeightCoef

	return(list(overtakingCoefDF = overtakingCoefDF,
							overtakingOverlapCoef = overtakingOverlapCoef,
							didOvertakeCost = didOvertakeCost,
							gotOvertakenCost = gotOvertakenCost,
							blockedMeanCoef = blockedMeanCoef,
							blockedVarCoef = blockedVarCoef,
							dCoefSourceWeightCoef = dCoefSourceWeightCoef,
							varByTotalWeightCoef = varByTotalWeightCoef))
}

MakeDataForRace = function(myRace, raceDF, lbl, simConst) {

	circuitOvertakingCoef = f1simulation:::MakeCircuitOvertakingCoef(myRace, simConst)

	fuelCoef = raceDF$mod30FuelCoef[raceDF$race == myRace]

	tyreCoefDF = raceTyreDF %>%
								filter(race == myRace) %>%
								select(tyre, mod30TyreInt, mod30TyreSlope) %>%
								lazy_left_join(tyreDF, 'tyre', 'isDry')

	# however there might be missing coefs so fill them in if necessary
	tyreCoefDF = f1simulation:::ImputeMissingTyreCoef(tyreCoefDF)

	numberOfLaps = raceDF$nlap[raceDF$race == myRace]

	## will also need the standard error of lap times
	goodLapIndex = with(lbl, which(race == myRace & isGood30))
	secStandardError = with(lbl[goodLapIndex,], sd( impsec - mod30PredSec))

	lapTimeIntercept = raceDF$mod30Intercept[raceDF$race == myRace]

	pitStopLoss = with(raceDF, (medianPitStopTime + inlapDelta + outlapDelta)[race == myRace])

	return(list(circuitOvertakingCoef = circuitOvertakingCoef,
							fuelCoef = fuelCoef,
							tyreCoefDF = tyreCoefDF,
							numberOfLaps = numberOfLaps,
							secStandardError = secStandardError,
							lapTimeIntercept = lapTimeIntercept,
							pitStopLoss = pitStopLoss))
}

AugmentLblWithUpdatedDCoef = function(myLbl, startOfLap, dCoefSourceWeightCoef, varByTotalWeightCoef) {
	### want to be able to take the start of any lap in an actual race, then prepare the tibble required by simulator to simulate from there

	myLbl = f1simulation:::MakeSingleRaceInRunningData(myLbl)
	myLbl = f1simulation:::MakeWithinRacePredictedSec(dCoefSourceWeightCoef, myLbl)
	myLbl = f1simulation:::OverrideFirstCarProblemLap(myLbl)
	myLbl$varOfUpdatingDCoef =
     f1simulation:::CalculateVarForTotalWeight(varByTotalWeightCoef, myLbl$totalWeight)

	return(myLbl)
}

MakeTyreStopDF = function(myRace, startOfLap) {

	alternativeStrategyFile = MakeRaceFile(myRace, 'alternative-strategy.csv')
	alternativeStrategyDF = read.csv(alternativeStrategyFile)
	# want to narrow down to the world that is defined by startOfLap
	alternativeStrategyDF$isCurrentUniverse = with(alternativeStrategyDF,
																									startOfLap >= vantageStartLap &
																									startOfLap <= vantageEndLap)
	alternativeStrategyDF$isStillToCome = with(alternativeStrategyDF,inlap >= startOfLap)

	tyreStopDF = alternativeStrategyDF %>%
								filter(isCurrentUniverse & isStillToCome) %>%
								select(driver, inlap, newTyre)

	return(tyreStopDF)
}

JoinNextStopInformation = function(multiRaceStatusDF, tyreStopDF, currentLap) {
	nextStop = tyreStopDF %>%
				filter(inlap >= currentLap) %>%
				group_by(driver) %>%
				filter(inlap == min(inlap))

	multiRaceStatusDF = left_join(multiRaceStatusDF, nextStop, 'driver')
	# fiddly having NAs for the inlap, set the inlap to something ludicrous if it's NA
	multiRaceStatusDF$inlap = with(multiRaceStatusDF, ifelse(!is.na(inlap), inlap, 999))

	return(multiRaceStatusDF)
}

ImputeMissingTyreCoef = function(tyreCoefDF) {
	# do average of nearest tyres by hardness
	needsImputingIndex = with(tyreCoefDF, which(is.na(mod30TyreInt) & isDry))
	if (length(needsImputingIndex) > 0) {
		tyreCoefDF = lazy_left_join(tyreCoefDF, tyreDF, 'tyre', 'hardness')
		for (ti in needsImputingIndex) {
			tyreCoefDF$hardnessDelta = with(tyreCoefDF, abs(hardness - hardness[ti]))
			tyreCoefDF$hardnessDelta[ti] = 1E6
			tyreCoefDF$isNearest = with(tyreCoefDF, isDry & hardnessDelta == min(hardnessDelta[isDry]))
			tyreCoefDF$mod30TyreInt[ti] = with(tyreCoefDF, mean(mod30TyreInt[isNearest]))
			tyreCoefDF$mod30TyreSlope[ti] = with(tyreCoefDF, mean(mod30TyreSlope[isNearest]))
		}
		tyreCoefDF = remove_column(tyreCoefDF, c('hardness', 'hardnessDelta', 'isNearest'))
	}
	# and also the vergne at monaco adjustment
	wetTyreIndex = with(tyreCoefDF, which(!isDry))
	if (length(wetTyreIndex) > 0) {
		tyreCoefDF[wetTyreIndex, c('mod30TyreInt', 'mod30TyreSlope')] = c(10, 1)
	}
	return(tyreCoefDF)
}

MakeFuelTyreAdjustment = function(multiRaceStatusDF, dataForRace) {

		multiRaceStatusDF$fuelCoef = dataForRace$fuelCoef
		multiRaceStatusDF = lazy_left_join(multiRaceStatusDF,
																	dataForRace$tyreCoefDF,
																	'tyre',
																	c('mod30TyreInt', 'mod30TyreSlope'))
		multiRaceStatusDF$fuelTyreAdjustment =
			with(multiRaceStatusDF, fuelCoef * fuel + mod30TyreInt + mod30TyreSlope * tyreLap)

		multiRaceStatusDF = within(multiRaceStatusDF, rm(fuelCoef, mod30TyreInt, mod30TyreSlope))

		return(multiRaceStatusDF)
}

MakeCircuitOvertakingCoef = function(myRace, simConst) {
	myCircuit = with(raceDF, circuit[race == myRace])

	circuitOvertakingCoef = with(simConst$overtakingCoefDF,
											mle[coefType == 'intercept'] +
											 mle[coefType == 'circuit' & coefName == myCircuit])

	### remember to make year specific adjustment coefs if necessary
	myRaceYear = with(raceDF, year[race == myRace])
	myRaceYearOtLabel = paste('year', myRaceYear, sep = '')
	currentYearAdjustment = with(simConst$overtakingCoefDF,
																	mle[coefType == myRaceYearOtLabel])
	if (length(currentYearAdjustment) > 0) {
		circuitOvertakingCoef = circuitOvertakingCoef + currentYearAdjustment
	}
	return(circuitOvertakingCoef)
}

GenerateDcoef = function(multiRaceStatusDF) {
	dcoef = with(multiRaceStatusDF, rnorm(nrow(multiRaceStatusDF), updatingDCoef, sqrt(varOfUpdatingDCoef)))
	return(dcoef)
}

CalculateIncentiveWeight = function(modalFinPosProb) {
	lapWeight = ifelse(modalFinPosProb < 0.95, 1, 1 - 20 * (modalFinPosProb - 0.95))
	lapWeight[which(lapWeight < 0.01)] = 0.01
	lapWeight[which(lapWeight > 1)] = 1
	lapWeight[is.na(lapWeight)] = 1
	return(lapWeight)
}

MakeMultiRaceStatusDF = function(raceStatusDF, numberOfSimulations) {
	multiRaceStatusDF = do.call(rbind, replicate(numberOfSimulations, raceStatusDF, simplify=FALSE))
	multiRaceStatusDF$simi = rep(1:numberOfSimulations, rep(nrow(raceStatusDF), numberOfSimulations))
	multiRaceStatusDF = multiRaceStatusDF %>% select(simi,everything())

	return(multiRaceStatusDF)
}

SimulateOvertaking = function(multiRaceStatusDF,
																numDriver, numberOfSimulations,
																dataForRace, simConst) {

	### right, now we have to simulate overtaking
	multiRaceStatusDF$predEndTimeElapsed = with(multiRaceStatusDF, startTimeElapsed + predSec)

	### we'll put a nice wrapper around it to make it less ugly at some point.
	## NB multiRaceStatusDF has to be in this order to run
	multiRaceStatusDF = multiRaceStatusDF[order(multiRaceStatusDF$simi, multiRaceStatusDF$startRank),]

	simMatrix = RcppGetOvertakingSummary(multiRaceStatusDF$predEndTimeElapsed,
												numberOfSimulations,
												numDriver,
												dataForRace$circuitOvertakingCoef, simConst$overtakingOverlapCoef)
	overtakeSummary = tibble(endRank = simMatrix[,1] + 1L,
								didOvertake = simMatrix[,2],
								gotOvertake = simMatrix[,3],
								simi = rep(1:numberOfSimulations, rep(numDriver, numberOfSimulations)))
	overtakeSummary$driver = multiRaceStatusDF$driver

	multiRaceStatusDF = left_join(multiRaceStatusDF, overtakeSummary, by = c('simi','driver'))

	return(multiRaceStatusDF)
}

FastRankCalculation = function(x, y, numDriver, numberOfSimulations) {
	rankPlusSimi = order(order(x, y))
  rankToSubtract = numDriver * (rep(0:(numberOfSimulations - 1), rep(numDriver, numberOfSimulations)))
  correctRank = rankPlusSimi - rankToSubtract

	return(correctRank)
}

SimulateLapTime = function(multiRaceStatusDF,
														numDriver, numberOfSimulations,
														dataForRace, simConst,
														startOfLap, currentLap) {

	multiRaceStatusDF = multiRaceStatusDF[order(multiRaceStatusDF$simi, multiRaceStatusDF$endRank),]

	multiRaceStatusDF$endTimeElapsed = with(multiRaceStatusDF,
							RcppSimulateLap(startRank, endRank, startTimeElapsed, predSec,
											didOvertake, gotOvertake,
											simConst$blockedMeanCoef, simConst$blockedVarCoef,
											simConst$didOvertakeCost, dataForRace$secStandardError))

	### check that endTelapse order matches endRank
	multiRaceStatusDF$checkEndRank =
		f1simulation:::FastRankCalculation(multiRaceStatusDF$simi, multiRaceStatusDF$endTimeElapsed,
																						numDriver, numberOfSimulations)

	rankingCheck = with(multiRaceStatusDF, identical(endRank, checkEndRank))
	if (!rankingCheck) {
		errorMessage = cat('\nstartofLap: ', startOfLap,'\ncurrentLap: ', currentLap, '\nrankings aren\'t correct\n')
		stop(errorMessage)
	}

	return(multiRaceStatusDF)
}

AdjustSimulationsForTyreStops = function(multiRaceStatusDF,
																					numDriver, numberOfSimulations,
																					dataForRace, tyreStopDF,
																					currentLap) {

	## bind on info about next stop for each driver
	multiRaceStatusDF = f1simulation:::JoinNextStopInformation(multiRaceStatusDF, tyreStopDF, currentLap)

	multiRaceStatusDF = mutate_cond(multiRaceStatusDF,
																	inlap == currentLap,
																	endTimeElapsed = endTimeElapsed + dataForRace$pitStopLoss,
																	tyre = newTyre,
																	tyreLap = 0)

	### that requires an update of endRank
	multiRaceStatusDF$endRank =
		f1simulation:::FastRankCalculation(multiRaceStatusDF$simi, multiRaceStatusDF$endTimeElapsed,
																						numDriver, numberOfSimulations)

	return(multiRaceStatusDF)
}

SummariseSingleLapSimulationOutput = function(startOfLap, numberOfLaps, numberOfSimulations,
																							raceStatusDF, raceStatusStore) {
	## is it actually worth storing the intermediate files? only use the final one surely - they're pretty quickly to make if you need them again

	finalLapStore = raceStatusStore[[numberOfLaps - startOfLap + 1]] %>%
									group_by(simi) %>%
									mutate(finPos = rank(endTimeElapsed)) %>%
									ungroup()
	finPosProb = finalLapStore %>%
									count(driver, finPos) %>%
									complete(driver = raceStatusDF$driver,
												finPos = 1:nrow(raceStatusDF),
												fill = list(n = 0)) %>%
									mutate(probability = n / numberOfSimulations) %>%
									select(-n) %>%
									mutate(lap = startOfLap)

	finPosSummary = finPosProb %>%
									group_by(driver) %>%
									summarise(meanFinPos = sum(probability * finPos),
														modalFinPosProb = max(probability)) %>%
									mutate(lap = startOfLap)

	# NB might want to return more things than this, maybe change to a list later
	return(list(finPosProb = finPosProb,
							finPosSummary = finPosSummary))
}

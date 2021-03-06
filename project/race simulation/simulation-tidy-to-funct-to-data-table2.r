### phase 1 is getting ti to run in new world
### phase 2 is getting it to store stuff
### phase 3 is converting to a library

### problems encoutered so far:
### we don't have otparam
### we don't have mod34upddcoef, mod34updsdcoef
### you might think we don't need MakeRaceTyreCoef but it does have the thing where we fill in missing tyre estimates
### we don't have alternative guessed tyre strategies
### hmmm. that is so much work. will take a long time to do before start of season. let's cheat. let's steal the work from old world

### early signs are that this takes about 70% the time that the dplyr version does - don't think that's worth the extra complexity - although there might be extra speed ups available, eg. setting a key, makybe there's a faster ranking algorithm (tried frank, but it was much slower, can't see what i was doing wrong)
# so next step is making sure we're exporting all the output that is sensible - i think the grid of prob of finishing in each position is a good idea, we currently don't export that
# then put into package

source('model code/model-startup.r')
source('project/race simulation/coef-mixing-funct.r')
source('project/race simulation/simulation-funct-data-table.r')

blockedOvertakingModelCoef = f1blockedovertakingmodel:::GetBlockedOvertakingModelCoef()
overtakingOverlapCoef =
	with(blockedOvertakingModelCoef$overtakingCoefDF, mle[coefType == 'overlap'])
didOvertakeCost = blockedOvertakingModelCoef$didOvertakeCost
gotOvertakenCost = blockedOvertakingModelCoef$gotOvertakenCost
blockedMeanCoef = blockedOvertakingModelCoef$blockedMeanCoef
blockedVarCoef = blockedOvertakingModelCoef$blockedVarCoef

raceDF = f1gaptrafficpitstop::CalculateMedianPitStopTime(raceDF)
#dum = f1simulation:::SetUpUpdatingCoef(lbl)

dum = GetInRunningWeightCoef()
dCoefSourceWeightCoef = dum$dCoefSourceWeightCoef
varByTotalWeightCoef = dum$varByTotalWeightCoef

lbl = InitialisePreRaceCoef(rddf, lbl)
lbl = PrepareLblForCoefMixing(lbl)

raceLapDF = lbl %>%
						group_by(race, leadLap) %>%
						summarise(isWet = any(isWet),
											isRed = any(isRed)) %>%
						ungroup()

suppressWarnings(library(Rcpp))
sourceCpp("c:/research/f1/project/race simulation/simulate-overtaking.cpp")
sourceCpp("c:/research/f1/project/race simulation/driver-loop.cpp")

SimulateWholeRace = function(myRace, numberOfSimulations) {

	# myRace = '2019australia'; startOfLap = 1; numberOfSimulations = 1000; set.seed(100)
	dataForRace = MakeDataForRace(myRace)
	tyreCoefDT = data.table::data.table(dataForRace$tyreCoefDF %>% select(tyre, mod30TyreInt, mod30TyreSlope))

	numberOfLaps = dataForRace$numberOfLaps

	myLbl = lbl %>%
					filter(race == myRace)
	myLbl$miscLapWeight = 1
	myLbl[,c('meanFinPos', 'modalFinPosProb')] = NA

	lapToSimulate = with(raceLapDF, leadLap[race == myRace & !isWet & !isRed])

	for (startOfLap in lapToSimulate) {
		myLbl = AugmentLblWithUpdatedDCoef(myLbl, startOfLap, dCoefSourceWeightCoef, varByTotalWeightCoef)
		raceStatusDF = myLbl %>%
										filter(lap == startOfLap) %>%
										select(driver, startRank, startTimeElapsed, tyre, tyreLap, fuel,
														updatingDCoef, varOfUpdatingDCoef) %>%
										arrange(startRank)
		# NB this extracts tyre stops as veiwed from the vantage point of startLap
		tyreStopDF = MakeTyreStopDF(myRace, startOfLap)

		numDriver = nrow(raceStatusDF)

		# need to duplicate the starting status of the race nsim times
		multiRaceStatusDT = MakeMultiRaceStatusDF(raceStatusDF, numberOfSimulations) %>%
												data.table::data.table()

		## need to simulate a driver coef given mean and sd of estimated one
		multiRaceStatusDT$dCoef = GenerateDcoef(multiRaceStatusDT)

		### want to store the results lap by lap
		allSimLap = startOfLap:numberOfLaps
		raceStatusStore =  vector("list", length(allSimLap))

		for (currentLap in startOfLap:numberOfLaps) {

			## so now estimate the impact of fuel and tyre wear
			multiRaceStatusDT = MakeFuelTyreAdjustment(multiRaceStatusDT, dataForRace, tyreCoefDT)

			multiRaceStatusDT$predSec =
				dataForRace$lapTimeIntercept + with(multiRaceStatusDT, dCoef + fuelTyreAdjustment)

			multiRaceStatusDT = SimulateOvertaking(multiRaceStatusDT, numDriver,
																						dataForRace$circuitOvertakingCoef, overtakingOverlapCoef,
																						numberOfSimulations)

			### now can simulate the lap times in light of the new running order

			multiRaceStatusDT = SimulateLapTime(multiRaceStatusDT, dataForRace$secStandardError, startOfLap, currentLap)

			### then need to rearrange the data in new ranking order - also this overwriting thing isn't ideal - could store the status each lap
			raceStatusStore[[match(currentLap, allSimLap)]] =
						multiRaceStatusDF %>% select(simi, driver, startTimeElapsed, endTimeElapsed)

			### then need to cover pit stops, then update tyreLap and fuel
			multiRaceStatusDF = AdjustSimulationsForTyreStops(multiRaceStatusDF,
																												tyreStopDF, dataForRace$pitStopLoss,
																												currentLap)

			### now update tyreLap and fuellap
			multiRaceStatusDF$tyreLap = multiRaceStatusDF$tyreLap + 1
			multiRaceStatusDF$fuel = multiRaceStatusDF$fuel - 1

			## now prepare the sims for the next lap
			multiRaceStatusDF = multiRaceStatusDF %>%
								select(simi, driver, tyre, tyreLap, fuel, dCoef, endRank, endTimeElapsed) %>%
								dplyr::rename(startRank = endRank, startTimeElapsed = endTimeElapsed) %>%
								group_by(simi) %>%
								arrange(startRank)

			numberOfLapBlockToUpdate = 20 # maybe 5 or 10 if you're not debugging
			if ( (currentLap %% numberOfLapBlockToUpdate) == 0) {
				message('Have simulated lap ',currentLap,' from vantage lap ', startOfLap, ' for ', myRace)
			}
		}

		### but then to summarise all of the sims into something nice
		## is it actually worth storing the intermediate files? only use the final one surely - they're pretty quickly to make if you need them again

		myLbl = SummariseSingleLapSimulationOutput(startOfLap, myLbl, numberOfLaps,
																							raceStatusDF, raceStatusStore,
																							numberOfSimulations)

		print(myLbl %>%
					filter(lap == startOfLap) %>%
					arrange(startTimeElapsed) %>%
					select(driver, startRank, startTimeElapsed, updatingDCoef,
									meanFinPos, modalFinPosProb, miscLapWeight))

		if (FALSE) {
			horizFinPosCount = spread(finPosCount, key = finPos, value = n)
			winProbSummary = finposCount %>%
										filter(finPos == 1) %>%
										group_by(driver) %>%
										summarise(winProb = sum(n) / numberOfSimulations)
		}
		message('Have simulated everything for startOfLap ', startOfLap, ', ', myRace)
	}
	return(myLbl)
}

# check it runs all the way through without crashing
# no we can't of course, we need to substitute the unestimated tyres
lbl[,c('updatingDCoef', 'meanFinPos', 'modalFinPosProb')] = NA

for (ri in 1:nrace) {
	if (raceDF$isValidRace30[ri]) {
		set.seed(100)
		myRace = raceDF$race
		numberOfSimulations = 100
		simOut = SimulateWholeRace(raceDF$race[ri], 100)
		lbl = join_on_overlap(lbl,
													simOut %>%
														select(race, driver, lap, updatingDCoef, meanFinPos, modalFinPosProb),
													c('race', 'driver', 'lap'))
	}
}

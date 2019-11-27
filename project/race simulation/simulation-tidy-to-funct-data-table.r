### phase 1 is getting ti to run in new world
### phase 2 is getting it to store stuff
### phase 3 is converting to a library

### problems encoutered so far:
### we don't have otparam
### we don't have mod34upddcoef, mod34updsdcoef
### you might think we don't need MakeRaceTyreCoef but it does have the thing where we fill in missing tyre estimates
### we don't have alternative guessed tyre strategies
### hmmm. that is so much work. will take a long time to do before start of season. let's cheat. let's steal the work from old world

source('model code/model-startup.r')
source('project/race simulation/coef-mixing-funct.r')
source('project/race simulation/simulation-funct.r')

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

suppressWarnings(library(Rcpp))
sourceCpp("c:/research/f1/project/race simulation/simulate-overtaking.cpp")
sourceCpp("c:/research/f1/project/race simulation/driver-loop.cpp")

SimulateWholeRace = function(myRace, numberOfSimulations) {

	# myRace = '2019australia'; startOfLap = 1
	dataForRace = MakeDataForRace(myRace)

	numberOfLaps = dataForRace$numberOfLaps

	myLbl = lbl %>%
					filter(race == myRace) %>%
					data.table::data.table()
	myLbl$miscLapWeight = 1
	myLbl[,c('meanFinPos', 'modalFinPosProb')] = NA

	for (startOfLap in 1:numberOfLaps) {
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
		multiRaceStatusDF = MakeMultiRaceStatusDF(raceStatusDF, numberOfSimulations)

		## need to simulate a driver coef given mean and sd of estimated one
		multiRaceStatusDF$dCoef = GenerateDcoef(multiRaceStatusDF)

		### want to store the results lap by lap
		allSimLap = startOfLap:numberOfLaps
		raceStatusStore =  vector("list", length(allSimLap))

		for (currentLap in startOfLap:numberOfLaps) {

			## so now estimate the impact of fuel and tyre wear
			multiRaceStatusDF = MakeFuelTyreAdjustment(multiRaceStatusDF, dataForRace)

			multiRaceStatusDF$predSec =
				dataForRace$lapTimeIntercept + with(multiRaceStatusDF, dCoef + fuelTyreAdjustment)

			multiRaceStatusDF = SimulateOvertaking(multiRaceStatusDF, numDriver,
																						dataForRace$circuitOvertakingCoef, overtakingOverlapCoef,
																						numberOfSimulations)

			### now can simulate the lap times in light of the new running order

			multiRaceStatusDF = SimulateLapTime(multiRaceStatusDF, dataForRace$secStandardError)

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


			if ( (currentLap %% 5) == 0) {
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
	}
	return(myLbl)
}

# check it runs all the way through without crashing
# no we can't of course, we need to substitute the unestimated tyres
lbl[,c('updatingDCoef', 'meanFinPos', 'modalFinPosProb')] = NA

for (ri in 1:nrace) {
	if (raceDF$isValidRace30[ri]) {
		simOut = SimulateWholeRace(raceDF$race[ri], 100)
		lbl = join_on_overlap(lbl,
													simOut %>%
														select(race, driver, lap, updatingDCoef, meanFinPos, modalFinPosProb),
													c('race', 'driver', 'lap'))
	}
}

### phase 1 is getting ti to run in new world
### phase 2 is getting it to store stuff
### phase 3 is converting to a library

### problems encoutered so far:
### we don't have otparam
### we don't have mod34upddcoef, mod34updsdcoef
### you might think we don't need MakeRaceTyreCoef but it does have the thing where we fill in missing tyre estimates
### we don't have alternative guessed tyre strategies
### hmmm. that is so much work. will take a long time to do before start of season. let's cheat. let's steal the work from old world

SimulateRace = function(myRace, numberOfSimulations, lbl, raceDF, raceLapDF, simConst) {

	# myRace = '2019australia'; startOfLap = 1; numberOfSimulations = 1000; set.seed(100)

	dataForRace = f1simulation:::MakeDataForRace(myRace, raceDF, lbl, simConst)
	numberOfLaps = dataForRace$numberOfLaps

	myLbl = lbl %>%
					filter(race == myRace)
	myLbl$miscLapWeight = 1
	myLbl[,c('meanFinPos', 'modalFinPosProb')] = NA

	lapToSimulate = with(raceLapDF, leadLap[race == myRace & !isWet & !isRed])
	myLbl$isSimulationValid = with(myLbl, leadLap %in% lapToSimulate)

	finPosProbList = vector("list", numberOfLaps)

	for (startOfLap in lapToSimulate) {
		myLbl = f1simulation:::AugmentLblWithUpdatedDCoef(myLbl, startOfLap,
																				simConst$dCoefSourceWeightCoef,
																				simConst$varByTotalWeightCoef)
		raceStatusDF = myLbl %>%
										filter(lap == startOfLap) %>%
										select(driver, startRank, startTimeElapsed, tyre, tyreLap, fuel,
														updatingDCoef, varOfUpdatingDCoef) %>%
										arrange(startRank)
		# NB this extracts tyre stops as veiwed from the vantage point of startLap
		tyreStopDF = f1simulation:::MakeTyreStopDF(myRace, startOfLap)

		numDriver = nrow(raceStatusDF)

		# need to duplicate the starting status of the race nsim times
		multiRaceStatusDF = f1simulation:::MakeMultiRaceStatusDF(raceStatusDF, numberOfSimulations)

		## need to simulate a driver coef given mean and sd of estimated one
		multiRaceStatusDF$dCoef = f1simulation:::GenerateDcoef(multiRaceStatusDF)

		### want to store the results lap by lap
		allSimLap = startOfLap:numberOfLaps
		raceStatusStore =  vector("list", length(allSimLap))

		for (currentLap in startOfLap:numberOfLaps) {

			## so now estimate the impact of fuel and tyre wear
			multiRaceStatusDF = f1simulation:::MakeFuelTyreAdjustment(multiRaceStatusDF, dataForRace)

			multiRaceStatusDF$predSec =
				dataForRace$lapTimeIntercept + with(multiRaceStatusDF, dCoef + fuelTyreAdjustment)

			multiRaceStatusDF = f1simulation:::SimulateOvertaking(multiRaceStatusDF,
																							numDriver, numberOfSimulations,
																							dataForRace, simConst)

			### now can simulate the lap times in light of the new running order

			multiRaceStatusDF = f1simulation:::SimulateLapTime(multiRaceStatusDF,
																						numDriver, numberOfSimulations,
																						dataForRace, simConst,
																						startOfLap, currentLap)

			### then need to rearrange the data in new ranking order - also this overwriting thing isn't ideal - could store the status each lap
			raceStatusStore[[match(currentLap, allSimLap)]] =
						multiRaceStatusDF %>% select(simi, driver, startTimeElapsed, endTimeElapsed)

			### then need to cover pit stops, then update tyreLap and fuel
			multiRaceStatusDF = f1simulation:::AdjustSimulationsForTyreStops(multiRaceStatusDF,
																												numDriver, numberOfSimulations,
																												dataForRace, tyreStopDF,
																												currentLap)

			### now update tyreLap and fuellap
			multiRaceStatusDF$tyreLap = multiRaceStatusDF$tyreLap + 1
			multiRaceStatusDF$fuel = multiRaceStatusDF$fuel - 1

			## now prepare the sims for the next lap
			multiRaceStatusDF = multiRaceStatusDF %>%
								select(simi, driver, tyre, tyreLap, fuel, dCoef, endRank, endTimeElapsed) %>%
								dplyr::rename(startRank = endRank, startTimeElapsed = endTimeElapsed)

			multiRaceStatusDF = multiRaceStatusDF[order(multiRaceStatusDF$simi, multiRaceStatusDF$startRank),]

			numberOfLapBlockToUpdate = 100 # maybe 5 or 10 if you're not debugging
			if ( (currentLap %% numberOfLapBlockToUpdate) == 0) {
				message('Have simulated lap ',currentLap,' from vantage lap ', startOfLap, ' for ', myRace)
			}
		}

		### but then to summarise all of the sims into something nice
		## is it actually worth storing the intermediate files? only use the final one surely - they're pretty quickly to make if you need them again

		lapSimulationSummary =
			f1simulation:::SummariseSingleLapSimulationOutput(startOfLap, numberOfLaps, numberOfSimulations,
																							raceStatusDF, raceStatusStore)

		lapSimulationSummary$finPosProb$race = myRace
		finPosProbList[[startOfLap]] = lapSimulationSummary$finPosProb
		# myLbl needs to know those, to know how it should downweight the current lap from now on
		myLbl = join_on_overlap(myLbl,
												lapSimulationSummary$finPosSummary,
												c('lap', 'driver'))
		myLbl = myLbl %>%
						mutate_cond(lap == startOfLap,
												miscLapWeight = CalculateIncentiveWeight(modalFinPosProb))

		print(myLbl %>%
					filter(lap == startOfLap) %>%
					arrange(startTimeElapsed) %>%
					select(driver, startRank, startTimeElapsed, updatingDCoef,
									meanFinPos, modalFinPosProb, miscLapWeight))
		message('Have simulated everything for startOfLap ', startOfLap, ' out of ', numberOfLaps, ', ', myRace)
	}
	finPosProbDF = bind_rows(finPosProbList)
	return(list(myLbl = myLbl,
							finPosProbDF = finPosProbDF))
}

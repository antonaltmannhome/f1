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

numberOfSimulations = 1000

suppressWarnings(library(Rcpp))
sourceCpp("c:/research/f1/project/race simulation/simulate-overtaking.cpp")
sourceCpp("c:/research/f1/project/race simulation/driver-loop.cpp")


SimulateWholeRace = function(myRace, numberOfSimulations) {

	myRace = '2019australia'
	startOfLap = 1
	dataForRace = MakeDataForRace(myRace)

	circuitOvertakingCoef = dataForRace$circuitOvertakingCoef
	fuelCoef = dataForRace$fuelCoef
	tyreCoefDF = dataForRace$tyreCoefDF
	numberOfLaps = dataForRace$numberOfLaps
	secStandardError = dataForRace$secStandardError
	lapTimeIntercept = dataForRace$lapTimeIntercept
	pitStopLoss = dataForRace$pitStopLoss

	myLbl = lbl %>%
					filter(race == myRace)
	myLbl$miscLapWeight = 1
	myLbl[,c('meanFinPos', 'modalFinPosProb')] = NA

	for (startOfLap in 1:dataForRace$numberOfLaps) {
		myLbl = AugmentLblWithUpdatedDCoef(myLbl, startOfLap)
		raceStatusDF = myLbl %>%
										filter(lap == startOfLap) %>%
										select(driver, startRank, startTimeElapsed, tyre, tyreLap, fuel,
														updatingDCoef, varOfUpdatingDCoef) %>%
										arrange(startRank)
		# NB this extracts tyre stops as veiwed from the vantage point of startLap
		tyreStopDF = MakeTyreStopDF(myRace, startOfLap)

		numDriver = nrow(raceStatusDF)

		# need to duplicate the starting status of the race nsim times
		multiRaceStatusDF = do.call(rbind, replicate(numberOfSimulations, raceStatusDF, simplify=FALSE))
		multiRaceStatusDF$simi = rep(1:numberOfSimulations, rep(nrow(raceStatusDF), numberOfSimulations))
		multiRaceStatusDF = multiRaceStatusDF %>% select(simi,everything())

		## need to simulate a driver coef given mean and sd of estimated one
		multiRaceStatusDF$dCoef = GenerateDcoef(multiRaceStatusDF)

		### want to store the results lap by lap
		allSimLap = startOfLap:numberOfLaps
		raceStatusStore =  vector("list", length(allSimLap))

		for (currentLap in startOfLap:numberOfLaps) {

			## so now estimate the impact of fuel and tyre wear
			multiRaceStatusDF = MakeFuelTyreAdjustment(multiRaceStatusDF, dataForRace)

			multiRaceStatusDF$predSec =
				lapTimeIntercept + with(multiRaceStatusDF, dCoef + fuelTyreAdjustment)

			### right, now we have to simulate overtaking
			multiRaceStatusDF$predEndTimeElapsed = with(multiRaceStatusDF, startTimeElapsed + predSec)

			### we'll put a nice wrapper around it to make it less ugly at some point.
			## NB multiRaceStatusDF has to be in this order to run
			multiRaceStatusDF = multiRaceStatusDF %>%
								arrange(simi, startRank)
			simMatrix = RcppGetOvertakingSummary(multiRaceStatusDF$predEndTimeElapsed,
														numberOfSimulations,
														numDriver,
														circuitOvertakingCoef, overtakingOverlapCoef)
			overtakeSummary = tibble(endRank = simMatrix[,1] + 1L,
										didOvertake = simMatrix[,2],
										gotOvertake = simMatrix[,3],
										simi = rep(1:numberOfSimulations, rep(numDriver, numberOfSimulations)))
			overtakeSummary$driver = multiRaceStatusDF$driver

			multiRaceStatusDF = left_join(multiRaceStatusDF, overtakeSummary, by = c('simi','driver'))

			### now can simulate the lap times in light of the new running order
			### NB this loop looks like a good candidate for rcpp

			multiRaceStatusDF = multiRaceStatusDF %>%
								arrange(simi, endRank)
			multiRaceStatusDF$endTimeElapsed = with(multiRaceStatusDF,
									RcppSimulateLap(startRank, endRank, startTimeElapsed, predSec,
													didOvertake, gotOvertake,
													blockedMeanCoef, blockedVarCoef,
													didOvertakeCost, secStandardError))

			### check that endTelapse order matches endRank
			multiRaceStatusDF = multiRaceStatusDF %>%
													group_by(simi) %>%
													mutate(checkEndRank = as.integer(rank(endTimeElapsed)))
			rankingCheck = with(multiRaceStatusDF, identical(endRank, checkEndRank))
			if (!rankingCheck) {
				stop('Error: rankings aren\'t correct\n')
			}

			### then need to rearrange the data in new ranking order - also this overwriting thing isn't ideal - could store the status each lap
			raceStatusStore[[match(currentLap, allSimLap)]] =
						multiRaceStatusDF %>% select(simi, driver, startTimeElapsed, endTimeElapsed)

			## bind on info about next stop for each driver
			multiRaceStatusDF = JoinNextStopInformation(multiRaceStatusDF, tyreStopDF, currentLap)

			multiRaceStatusDF = mutate_cond(multiRaceStatusDF,
																			inlap == currentLap,
																			endTimeElapsed = endTimeElapsed + pitStopLoss,
																			tyre = newTyre,
																			tyreLap = 0)

			### that requires an update of endRank
			multiRaceStatusDF = multiRaceStatusDF %>%
									group_by(simi) %>%
									mutate(endRank = rank(endTimeElapsed))

			### now update tyreLap and fuellap
			multiRaceStatusDF$tyreLap = multiRaceStatusDF$tyreLap + 1
			multiRaceStatusDF$fuel = multiRaceStatusDF$fuel - 1

			## now prepare the sims for the next lap
			multiRaceStatusDF = multiRaceStatusDF %>%
								select(simi, driver, tyre, tyreLap, fuel, dCoef, endRank, endTimeElapsed) %>%
								dplyr::rename(startRank = endRank, startTimeElapsed = endTimeElapsed) %>%
								group_by(simi) %>%
								arrange(startRank)

			### then need to cover pit stops, then update tyreLap and fuel

			if ( (currentLap %% 5) == 0) {
				message('Have simulated lap ',currentLap,' from vantage lap ', startOfLap)
			}
		}

		### but then to summarise all of the sims into something nice
		## is it actually worth storing the intermediate files? only use the final one surely - they're pretty quickly to make if you need them again

		finalLapStore = raceStatusStore[[numberOfLaps - startOfLap + 1]] %>%
										group_by(simi) %>%
										mutate(finPos = rank(endTimeElapsed)) %>%
										ungroup()
		finPosCount = finalLapStore %>%
										count(driver, finPos) %>%
										complete(driver = raceStatusDF$driver,
													finPos = 1:nrow(raceStatusDF),
													fill = list(n = 0))

		finPosSummary = finPosCount %>%
										group_by(driver) %>%
										summarise(meanFinPos = sum(n * finPos)/numberOfSimulations,
															modalFinPosProb = max(n)/numberOfSimulations)

		# myLbl needs to know those, to know how it should downweight the current lap from now on
		myLbl = subset_join(myLbl,
												finPosSummary,
												'driver',
												lap == startOfLap)
		myLbl = myLbl %>%
						mutate_cond(lap == startOfLap,
												miscLapWeight = CalculateIncentiveWeight(modalFinPosProb))

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

# to compare old v new:
if (FALSE) {
	lbl = getfinposprob(1, 34)
	yrToCheck = 2017:2018 # if it agree just for those I'm happy
	raceToCheck = with(raceDF, race[yr %in% yrToCheck & isvalidrace30==1])
	lbl$newfinpos1 = NA
	for (myRace in raceToCheck) {
		for (myStartLap in 1:raceDF$nlap[raceDF$race==myRace]) {
			dataForRaceLap = MakeDataForRaceLap(myRace, myStartLap)
			newWinProb = SimulateRace(dataForRaceLap, myStartLap, 1000) %>%
							dplyr::rename(newfinpos1 = winProb)
			lbl = subset_join(lbl,
								newWinProb,
								'driver',
								race == myRace & lap == myStartLap)
			message('Have processed ',myRace, ', lap ', myStartLap)
		}
		# but there is surely something important we're not doing, which is the silverstone 2014 issue
	}

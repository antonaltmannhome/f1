
ProcessSimulation = function() {

	SetUpModel()
	raceDF = f1gaptrafficpitstop::CalculateMedianPitStopTime(raceDF)

	rddf = f1smoothing:::InitialisePreRaceCoef(rddf, 30, 'postLMFit')
	lbl = f1simulation:::AlignPreRaceCoefWithLbl(rddf, lbl)
	lbl = f1simulation:::PrepareLblForCoefMixing(lbl)

	raceLapDF = lbl %>%
							group_by(race, leadLap) %>%
							summarise(isWet = any(isWet),
												isRed = any(isRed)) %>%
							ungroup()

	simConst = f1simulation:::GetSimulationConstant()

	suppressWarnings(library(Rcpp))

	sourceCpp(paste0(RCPPPATH, "simulate-overtaking.cpp"), cacheDir = RCPPPATH)
	sourceCpp(paste0(RCPPPATH, "driver-loop.cpp"), cacheDir = RCPPPATH)

	numberOfSimulations = 1000
	rrToUpdate = with(raceDF, rr[which(!doneSimulation)])

	lbl$isSimulationValid = NA
	for (ri in rrToUpdate) {
		if (raceDF$isValidRace30[ri]) {
			raceSimulationOutput = f1simulation:::SimulateRace(raceDF$race[ri], numberOfSimulations, lbl, raceDF, raceLapDF, simConst)
			lbl = join_on_overlap(lbl,
														raceSimulationOutput$myLbl %>%
														select(race, driver, lap, isSimulationValid, updatingDCoef, meanFinPos, modalFinPosProb),
														c('race', 'driver', 'lap'))
			# NB not everything will be filled in due to invalid laps, haven't really
			sqlLazyUpdate(lbl %>% filter(rr == ri & isSimulationValid),
										'racedriverlap',
										c('race', 'driver', 'lap'),
										c('updatingDCoef', 'meanFinPos', 'modalFinPosProb'))
			message('Writing the finishing position probabilites to database, this takes 30 seconds or so...')
			sqlInsert_usedf('finishingpositionprobability',
											raceSimulationOutput$finPosProbDF)
			message('Have done all the simulation work for ', raceDF$race[ri], '!')
		}
	  raceDF$doneSimulation[ri] = TRUE
		sqlLazyUpdate(raceDF[ri,], 'race', 'race', 'doneSimulation')
	}
}

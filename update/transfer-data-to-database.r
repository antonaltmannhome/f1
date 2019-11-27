source(paste0(UPDATECODEPATH, 'reshape-raw-data-funct.r'))
source(paste0(UPDATECODEPATH, 'transfer-data-to-database-funct.r'))
source(paste0(SQLFUNCTPATH, 'sqlfunct.r'))

# this all runs, but it's not tidy and the process still isn't solid, need to work out what happens in what order

TransferDataToDatabase = function(myYear) {

	rawDataStatus = GetRawDataStatus(myYear)
	raceToWriteToDatabase = with(rawDataStatus, race[fetched & augmented & !writtenToDatabase])

	for (myRace in raceToWriteToDatabase) {
		WriteRaceToDatabase(myRace)
		
		WriteDriverToDatabase()
		
		WriteRaceDriverToDatabase(myRace)
		
		lbl = InitialiseLbl(myRace)
		lbl = MakeLeaderTimeElapsed(lbl)
		
		pitStopDF = MakePitStopDF(myRace)
		stintDF = MakeStintDF(lbl, pitStopDF)
		
		lbl = MakeInlapOutlap(lbl, pitStopDF)
		lbl = AlignStintTyre(lbl, stintDF, pitStopDF)
		
		lbl = AlignSafetyCarWetPeriod(lbl, myRace, 'safetycar')
		lbl = AlignSafetyCarWetPeriod(lbl, myRace, 'wetperiod')
		lbl = AlignRedFlagLap(lbl, myRace)
		lbl = f1data:::MakeSCRestart(lbl)

		overtakingDF = f1gaptrafficpitstop::FindAllOvertakingMove(lbl, myRace)
		lbl = f1gaptrafficpitstop::AlignOvertaking(lbl, overtakingDF, isPreDelta = TRUE)

		raceTyreDF = MakeRaceTyreDF(lbl)

		qualifyingSessionDF = MakeQualifyingSessionDF(myRace)
		qualifyingDF = MakeQualifyingDF(myRace)

		## need a MakeRaceDriver function
		## and i don't think we need all the 'my' prefixes, these dfs never coexist with the full data
		## processredflag should be interactive, allowing user to check that they agree

		sqlInsert_usedf('racetyre', raceTyreDF)

		sqlInsert_usedf('racedriverlap', lbl %>%
											select(race, driver, lap, sec,leadLap,
													inlap, outlap, stint, tyre, tyreLap,
													preDeltaDidOt, preDeltaGotOt,
													preDeltaDidLap, preDeltaGotLap,
													isSafetyCar, isWet, isRed))

		sqlInsert_usedf('stint', stintDF %>%
									select(race, driver, stint, startLap, endLap,
											usedOrNew, tyre))	

		sqlInsert_usedf('pitstop', pitStopDF %>%
									select(race, driver, endOfLap, isRedFlagStop,
											pitStopTime, penalty, usedOrNew, replaceTyre, tyre))	

		sqlInsert_usedf('qualifyingsession', qualifyingSessionDF)

		sqlInsert_usedf('qualifying', qualifyingDF %>%
										filter(!is.na(sec)) %>%
										select(race, driver, session, sec))
		
		# but why would we ever use this:
		# sqlInsert_usedf('predeltaovertaking', overtakingDF)
		# any analysis we ever do would surely be post-delta
		
		### update rawDataStatus to let it know we've done all of this
		rawDataStatus$writtenToDatabase[rawDataStatus$race == myRace] = TRUE
	}
	
	UpdateRawDataStatus(myYear, rawDataStatus)
}


MakeLeaderTimeElapsed = function(lbl) {
	lbl = lbl %>%
				group_by(driver) %>%
				arrange(lap) %>%
				mutate(endTimeElapsed = cumsum(sec),
						startTimeElapsed = endTimeElapsed - sec) %>%
				ungroup()
	lbl = lbl %>%
				group_by(lap) %>%
				mutate(isLeader = endTimeElapsed == min(endTimeElapsed)) %>%
				ungroup()
	# want to add in what lap the leader is one too
	leaderLapTimeElapsed = lbl %>%
							filter(isLeader) %>%
							select(lap, endTimeElapsed)
	lbl$leadLap = with(leaderLapTimeElapsed, lap[findInterval(lbl$endTimeElapsed, endTimeElapsed)])
	return(lbl)
}

MakeInlapOutlap = function(lbl, pitStopDF) {

	pitStopDF = pitStopDF %>%
				dplyr::rename(lap = endOfLap)
	
	lbl = indicate_overlapping_combination(lbl,
											pitStopDF %>%
												filter(lap != 0 & 
														!isRedFlagStop),
											c('lap', 'driver'),
											'inlap')
	lbl = lbl %>%
				group_by(driver) %>%
				arrange(lap) %>%
				mutate(outlap = lag(inlap, 1, default = FALSE)) %>%
						#stint = cumsum(as.integer(outlap)) + 1L)
				ungroup()
	
	return(lbl)
}

AlignStintTyre = function(lbl, stintDF, pitStopDF) {

	lbl = indicate_overlapping_combination(
				lbl,
				stintDF %>%
				dplyr::rename(lap = startLap),
				c('driver', 'lap'),
				'isStartOfStint')
	
	lbl= lbl %>%
			group_by(driver) %>%
			arrange(lap) %>%
			mutate(stint = cumsum(isStartOfStint)) %>%
			ungroup()

	lbl = lazy_left_join(lbl,
						stintDF %>%
						dplyr::rename(lap = startLap),
						c('driver', 'stint'), 'tyre')
	
	# however, to get number of laps on the tyre, need to factor in that tyres might not be new at start of stint in case of a red flag
	
	tyreStintDF = lazy_left_join(stintDF,
								pitStopDF %>%
									rename(endLap = endOfLap),
								c('driver', 'endLap'),
								'replaceTyre') %>%
								rename(replaceTyreAtEndOfStint = replaceTyre) %>%
								group_by(driver) %>%
								arrange(startLap) %>%
								mutate(laggedRTAEOS = lag(replaceTyreAtEndOfStint),
										tyreStint = c(1, 1 + cumsum(laggedRTAEOS[-1]))) %>%
								ungroup()
	
	# now want to squash non-changing stints together
	squashedTyreStintDF = tyreStintDF %>%
							group_by(driver, tyreStint) %>%
							summarise(startLap = min(startLap),
									endLap = max(endLap)) %>%
							ungroup()
								
	lbl = indicate_overlapping_combination(
				lbl,
				squashedTyreStintDF %>%
				dplyr::rename(lap = startLap),
				c('driver', 'lap'),
				'isStartOfTyreStint')
	
	lbl= lbl %>%
			group_by(driver) %>%
			arrange(lap) %>%
			mutate(tyreStint = cumsum(isStartOfTyreStint)) %>%
			ungroup()

	lbl = lbl %>%
			group_by(driver, tyreStint) %>%
			arrange(lap) %>%
			mutate(tyreLap = 1:n()) %>%
			ungroup()

	return(lbl)
}

InitialiseLbl = function(myRace) {
	lapTimeFile = MakeRaceFile(myRace, 'laptime.csv')
	lbl = ReadF1Data(lapTimeFile, 'lapTimeDF')
	lbl$race = myRace
	lbl = lbl %>% select(race, driver, lap, sec)
	
	return(lbl)
}

MakeQualifyingSessionDF = function(myRace) {
	qualifyingSessionDF = tibble(race = myRace, session = 1L:3L)
	wetQualifyingFile = MakeRaceFile(myRace, 'wet-qualifying.csv')
	wetQualifyingSessionDF = ReadF1Data(wetQualifyingFile, 'wetQualifyingSessionDF')
	qualifyingSessionDF = indicate_overlapping_combination(qualifyingSessionDF,
														wetQualifyingSessionDF,
														'session',
														'isWet')
	
	# but have some sessions been abandoned?
	abandonedQualifyingSessionFile = paste0(OUTPUTPATH, 'abandoned-qualifying-session.csv')
	abandonedQualifyingSessionDF = ReadF1Data(abandonedQualifyingSessionFile, 'abandonedQualifyingSessionDF')
	qualifyingSessionDF = indicate_overlapping_combination(qualifyingSessionDF,
														abandonedQualifyingSessionDF,
														c('race', 'session'),
														'isAbandoned')
														
	qualifyingSessionDF = qualifyingSessionDF %>%
							filter(!isAbandoned)
	
	return(qualifyingSessionDF)
}

MakeQualifyingDF = function(myRace) {
	entryFile = MakeRaceFile(myRace, 'entryetc.csv')
	entryDF = ReadF1Data(entryFile, 'entryDF')
	qualifyingDF = entryDF %>%
						mutate(race = myRace) %>%
						select(race, driver, q1, q2, q3) %>%
						gather(session, sec, -c(race, driver)) %>%
						mutate(session = as.integer(gsub('q', '', session)))

	qualifyingSessionDF = MakeQualifyingSessionDF(myRace)
	
	qualifyingDF = left_join(qualifyingDF, qualifyingSessionDF, c('race', 'session'))
	
	return(qualifyingDF)
}

FindFastestDryQualifyingTime = function(myRace) {
	qualifyingDF = MakeQualifyingDF(myRace)
	
	validQualifyingDF = qualifyingDF %>%
								filter(!isWet & !is.na(sec))

	fastestQualifyingSec = NA
	if (nrow(validQualifyingDF) > 0) {
		fastestQualifyingSec = min(validQualifyingDF$sec)
	}

	return(fastestQualifyingSec)
}

AlignSafetyCarWetPeriod = function(lbl, myRace, safetyCarOrWetPeriod) {
	if (safetyCarOrWetPeriod == 'safetycar') {
		safetycarfile=MakeRaceFile(myRace, 'safety-car.csv')
		timePeriodDF = ReadF1Data(safetycarfile, 'SafetyCarDF') %>%
							mutate(startEnd = c('start', 'end')[match(inout, c('out', 'in'))]) %>%
							select(-inout)
	}
	if (safetyCarOrWetPeriod == 'wetperiod') {
		wetperiodfile=MakeRaceFile(myRace, 'wet-period.csv')
		timePeriodDF = ReadF1Data(wetperiodfile, 'wetPeriodDF')
	}
	lbl$isTrue = FALSE
	if (nrow(timePeriodDF) > 0) {
		numTimePeriod = as.integer(nrow(timePeriodDF) / 2)
		timePeriodDF = timePeriodDF %>%
						mutate(timePeriodNumber = rep(1:numTimePeriod, rep (2, numTimePeriod)))
		horizTimePeriodDF = spread(timePeriodDF, key = startEnd, value = timeElapsed, sep = '')
		for (ti in 1:numTimePeriod) {
			lbl = lbl %>%
					mutate_cond(between(endTimeElapsed,
								horizTimePeriodDF$startEndstart[ti],
								horizTimePeriodDF$startEndend[ti]),
							isTrue = TRUE)
		}
	}
	if (safetyCarOrWetPeriod == 'safetycar') {
		lbl = lbl %>%
				dplyr::rename(isSafetyCar = isTrue)
	}
	if (safetyCarOrWetPeriod == 'wetperiod') {
		lbl = lbl %>%
				dplyr::rename(isWet = isTrue)
	}
	return(lbl)
}

AlignRedFlagLap = function(lbl, myRace) {
	redflagfile=MakeRaceFile(myRace, 'red-flag.csv')
	redFlagDF = ReadF1Data(redflagfile, 'redFlagDF')
	if (nrow(redFlagDF) == 0) {
		lbl$isRed = FALSE
	}
	if (nrow(redFlagDF) > 0) {
		lbl = indicate_overlapping_combination(lbl,
													redFlagDF,
													c('race', 'driver', 'lap'),
													'isRed')
	}
	return(lbl)
}

MakePitStopDF = function(myRace) {
	pitstopfile = MakeRaceFile(myRace, 'pitstop-corrected.csv')
	pitStopDF = ReadF1Data(pitstopfile, 'pitStopDF')

	pitStopDF = pitStopDF %>%
				group_by(driver) %>%
				arrange(lap) %>%
				mutate(race = myRace) %>%
				dplyr::rename(endOfLap = lap,
								replaceTyre = replaceTyre,
								pitStopTime = time,
								usedOrNew = UN) %>%
				ungroup() %>%
				select(race, everything())
	
	return(pitStopDF)
}

MakeStintDF = function(lbl, pitStopDF) {
	
	pitStopDF = lazy_left_join(pitStopDF,
								lbl %>%
								group_by(driver) %>%
								summarise(maxLap = as.integer(max(lap))),
								'driver')
	
	## nasty issue with red flags, a driver can be classed 
	
	stintDF = pitStopDF %>%
				filter( (endOfLap == 0 | replaceTyre | isRedFlagStop) & maxLap > endOfLap & !is.na(maxLap)) %>%
				mutate(startLap = endOfLap + 1L) %>%
				group_by(driver) %>%
				arrange(startLap) %>%
				mutate(endLap = case_when(
						startLap < max(startLap) ~ lead(endOfLap),
						startLap == max(startLap) ~ maxLap),
						stint = 1:n()) %>%
				select(race, driver, stint, startLap, endLap, tyre, usedOrNew) %>%
				ungroup()
				
	return(stintDF)
}

MakeRaceTyreDF = function(lbl) {
	raceTyreDF = lbl %>%
					group_by(race, tyre) %>%
					summarise(numberOfLap = n()) %>%
					ungroup()
	return(raceTyreDF)
}


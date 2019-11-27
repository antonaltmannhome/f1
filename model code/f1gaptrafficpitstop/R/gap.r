.CheckGapAheadAlreadyDone = function(lbl, coreColumnMade, intermediateColumnMade, includeIntermediateColumn) {
	alreadyDone = FALSE
	if (!includeIntermediateColumn) {
		if (all(coreColumnMade %in% names(lbl))) {
			alreadyDone = TRUE
		}
	}
	if (includeIntermediateColumn) {
		if (all(c(coreColumnMade, intermediateColumnMade) %in% names(lbl))) {
			alreadyDone = TRUE
		}
	}
	return(alreadyDone)
}

GetGapAhead = function(lbl, isPreDelta, includeIntermediateColumn = FALSE) {

	# this takes a while to run, so check you don't already have the columns that it makes
	coreColumnMade = c('impsec', 'startTimeElapsed', 'endTimeElapsed', 'startRank', 'endRank', 'SOLGap', 'EOLGap', 'secLimit', 'overlap')
	intermediateColumnMade = c('SOLDA', 'SOLDASec', 'SOLGapToSOLDA', 'EOLGapToSOLDA',
								'EOLDA', 'EOLDASec', 'SOLGapToEOLDA', 'EOLGapToEOLDA')

	alreadyDone = f1gaptrafficpitstop:::.CheckGapAheadAlreadyDone(lbl, coreColumnMade, intermediateColumnMade, includeIntermediateColumn)
	if (alreadyDone) {
		return(lbl)
	}

	# use impsec when you have it, otherwise sec, even though it's a bit rubbish
	lbl = lazy_left_join(lbl, rddf, c('race', 'driver'), 'startingGrid')

	if (FALSE) {
	  # why do you need predsec before model 30? i can't see why you do
	if (isPreDelta) {
		lbl = f1laptimelm:::MakePredSec(lbl, 4, adjustForCarProblem = TRUE) %>%
					rename(predSec = mod4PredSec)
	}
	}

	if (!isPreDelta) {
		lbl = f1laptimelm:::MakePredSec(lbl, 30, adjustForCarProblem = TRUE) %>%
					rename(predSec = mod30PredSec)
	}

	if (!isPreDelta) {
		lbl = f1gaptrafficpitstop::AdjustLapTimeForPitStop(lbl, includeIntermediateColumn = TRUE)
	}

	arrangedlbl = lbl %>%
					group_by(race, driver) %>%
					arrange(lap)
	if (isPreDelta) {
		lbl = arrangedlbl %>%
				mutate(endTimeElapsed = cumsum(sec),
						startTimeElapsed = lag(endTimeElapsed)) %>%
				mutate_cond(lap == 1, startTimeElapsed = (startingGrid - 1) * 0.3) %>%
				ungroup()
	}



	# but if we've got the inlap and outlap deltas then we need to correct around the pit stops
	if (!isPreDelta) {
		lbl = arrangedlbl %>%
				mutate(tempEndTimeElapsed = cumsum(sec),
						endTimeElapsed = tempEndTimeElapsed + endTimeElapsedAdjustment,
						startTimeElapsed = lag(tempEndTimeElapsed) + startTimeElapsedAdjustment) %>%
				mutate_cond(lap == 1, startTimeElapsed = (startingGrid - 1) * 0.3) %>%
				ungroup()
	}

	if (isPreDelta) secToUse = 'sec'
	if (!isPreDelta) secToUse = 'impsec'

	lbl = lbl %>%
			group_by(race, lap) %>%
			mutate(startRank = as.integer(rank(startTimeElapsed)),
					endRank = as.integer(rank(endTimeElapsed))) %>%
			ungroup()

	lbl = lbl %>%
			group_by(race, lap) %>%
			arrange(startTimeElapsed) %>%
			mutate(SOLDA = lag(driver),
					SOLDASec = lag(get(secToUse)),
					SOLGapToSOLDA = startTimeElapsed - lag(startTimeElapsed),
					EOLGapToSOLDA = SOLGapToSOLDA + (get(secToUse) - SOLDASec)) %>%
			ungroup()

	lbl = lbl %>%
			group_by(race, lap) %>%
			arrange(endTimeElapsed) %>%
			mutate(EOLDA = lag(driver),
					EOLDASec = lag(get(secToUse)),
					SOLGapToEOLDA = startTimeElapsed - lag(startTimeElapsed),
					EOLGapToEOLDA = SOLGapToEOLDA + (get(secToUse) - EOLDASec)) %>%
			ungroup()

	# but these are massively misleading if you overtake somebody, they become negative, so the pmins coming up are completely nonsensical
	lbl = lbl %>%
	        mutate_cond(!is.na(SOLGapToEOLDA) & SOLGapToEOLDA < 0,
	                    SOLGapToEOLDA = 999)
	lbl = lbl %>%
      	  mutate_cond(!is.na(EOLGapToSOLDA) & EOLGapToSOLDA < 0,
      	              EOLGapToSOLDA = 999)
	
	lbl = lbl %>%
			mutate(SOLGap = pmin(SOLGapToSOLDA, SOLGapToEOLDA, na.rm = TRUE),
    					EOLGap = pmin(EOLGapToSOLDA, EOLGapToEOLDA, na.rm = TRUE))

	# spent a lot of time deciding on secLimit.
	# important to remember why you want to use it. i think we only need it for non-overtaking laps, so only define it for that
	# see project/overtaking/more-investigation for explanation
	# for most situations, secLimit is just EOLDASec - SOLGapToEOLDA. Which is equal to SOLDASec - SOLGapToSOLDA when the driver ahead doesn't have any problems or overtake. It's not the case whent he driver ahead does overtaking, but that's why the first equation is better
	# exceptions (1): the driver ahead gets overtaken by us but does an outlying lap. in that case, we ought to ignore that driver then recalculate based on other drivers. but that's complicated, and gives us virtually no benefit, because all we gain is a few extra (possibly dodgy) data points in our massive database of possible overtakings/blocked laps. might help in a situation where we're trying to estimate circuit OT parameters, but that's too much of an edge case to worry about I think
	# exceptions (2): the driver ahead pits. actually, this isn't a problem, impsec and endTimeElapsed should cover it. this is fine

	if (!isPreDelta) {
  	lbl = lbl %>%
					mutate(secLimit = ifelse(didOt == 0 & gotOt == 0, EOLDASec - SOLGapToEOLDA, NA),
									overlap = predSec - secLimit)
	}

	lbl = lbl %>%
			select(-startingGrid)

	if (!isPreDelta) {
		lbl = lbl %>%
				select(-c(inlapDelta, outlapDelta, replaceTyre, pitStopTime, penalty, prevLapPitStopTime,
							tempEndTimeElapsed, startTimeElapsedAdjustment, endTimeElapsedAdjustment,
							predSec))
	}
	if (!includeIntermediateColumn) {
		lbl = remove_column(lbl, intermediateColumnMade)
	}

	return(lbl)
}

GetLeadGap = function(lbl) {
	lbl = lbl %>%
			group_by(race, lap) %>%
			arrange(startRank) %>%
			mutate(leadStartTimeElapsed = lead(startTimeElapsed)) %>%
			mutate_cond(startRank == 1,
						leadGap = leadStartTimeElapsed - startTimeElapsed) %>%
			select(-leadStartTimeElapsed) %>%
			ungroup()
	return(lbl)
}

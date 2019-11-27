
GetGapAhead = function(lbl, isPreDelta) {

	# this takes a while to run, so check you don't already have the columns that it makes
	columnMade = c('impsec', 'startTimeElapsed', 'endTimeElapsed', 'aheadDriv', 'aheadSec',
					'startTimeGap', 'updateTimeGap', 'startRank', 'endRank')
	if (all(columnMade %in% names(lbl))) {
		return(lbl)
	}
					
	# use impsec when you have it, otherwise sec, even though it's a bit rubbish
	lbl = lazy_left_join(lbl, rddf, c('race', 'driver'), 'startingGrid')
	
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
						endTimeElapsed = tempEndTimeElapsed - inlapDelta * inlap,
						startTimeElapsed = lag(tempEndTimeElapsed) + outlapDelta * outlap) %>%
				mutate_cond(lap == 1, startTimeElapsed = (startingGrid - 1) * 0.3) %>%
				ungroup()
	}

	if (isPreDelta) secToUse = 'sec'
	if (!isPreDelta) secToUse = 'impsec'
	
	lbl = lbl %>%
			group_by(race, lap) %>%
			arrange(startTimeElapsed) %>%
			mutate(aheadDriv = lag(driver),
					aheadSec = lag(get(secToUse)),
					startTimeGap = startTimeElapsed - lag(startTimeElapsed),
					updateTimeGap = startTimeGap + (get(secToUse) - aheadSec),
					startRank = as.integer(rank(startTimeElapsed)),
					endRank = as.integer(rank(endTimeElapsed))) %>%
			ungroup()
			
	lbl = lbl %>%
			select(-startingGrid)
			
	if (!isPreDelta) {
		lbl = lbl %>%
				select(-c(inlapDelta, outlapDelta, replaceTyre, pitStopTime, penalty, prevLapPitStopTime,
							tempEndTimeElapsed))
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

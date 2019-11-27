
AlignPitStopData = function(lbl) {
	lbl = lazy_left_join(lbl,
						pitStopDF %>%
						dplyr::rename(lap = endOfLap),
						c('race', 'driver', 'lap'),
						c('pitStopTime', 'penalty', 'replaceTyre'))
	lbl = lbl %>%
			group_by(race, driver) %>%
			arrange(lap) %>%
			mutate(prevLapPitStopTime = lag(pitStopTime)) %>%
			ungroup()

	return(lbl)
}


.BolsterPitStopDF = function(lbl, stintDF) {
  bolsteredPitStopDF = stintDF %>%
                filter(!isSafetyCar & !isRed & !stintRetired & isPitStop & !penalty & !isWet) %>%
                select(race, driver, endLap, pitStopTime) %>%
                lazy_left_join(lbl %>%
                                mutate(rawInlapSec = sec,
                                      inlapPredSec = mod4PredSec),
                                by = c('race', 'driver', 'endLap' = 'lap'),
                                c('rawInlapSec', 'inlapPredSec', 'isSafetyCar')) %>%
                lazy_left_join(lbl %>%
                              mutate(nextLap = lap - 1,
                                      rawOutlapSec = sec,
                                      outlapPredSec = mod4PredSec),
                              by = c('race', 'driver', 'endLap' = 'nextLap'),
                              c('rawOutlapSec', 'outlapPredSec'))

  # let's take out the ones that potentially/probably include the pitstop
  pitStopSummaryByRace = bolsteredPitStopDF %>%
                      filter(!is.na(rawInlapSec + rawOutlapSec)) %>%
											group_by(race) %>%
                      summarise(numStandardInlap = sum(!is.na(inlapPredSec)),
																numStandardOutlap = sum(!is.na(outlapPredSec)),
																medianInlapSec = median(rawInlapSec, na.rm = TRUE),
                                medianOutlapSec = median(rawOutlapSec, na.rm = TRUE)) %>%
  										mutate(ioToAdjust = ifelse(medianInlapSec > medianOutlapSec, 'inlap', 'outlap'))
  # there is the odd occasion where we have no valid pitstops in a valid race, just have to do these manually, way to destructive to make it cope more generally
  manualAddition = tibble(race = '2016monaco', numStandardInlap = 0, numStandardOutlap = 0,
                            medianInlapSec = 100, medianOutlapSec = 84, ioToAdjust = 'inlap')
  pitStopSummaryByRace = bind_rows(pitStopSummaryByRace,
                                   manualAddition)
  stillMissingRace = setdiff(with(raceDF, race[isValidRace4]), pitStopSummaryByRace$race)
  if (length(stillMissingRace) > 0) {
    stop('You need to add manual information in .BolsterPitStopDF because there are missing race(s) from it')
  }

	bolsteredPitStopDF = lazy_left_join(bolsteredPitStopDF, pitStopSummaryByRace, 'race', 'ioToAdjust')
	bolsteredPitStopDF = bolsteredPitStopDF %>%
												mutate(inlapSec = case_when(
																	ioToAdjust == 'inlap' ~ rawInlapSec - pitStopTime,
																	ioToAdjust == 'outlap' ~ rawInlapSec),
																outlapSec = case_when(
																	ioToAdjust == 'inlap' ~ rawOutlapSec,
																	ioToAdjust == 'outlap' ~ rawOutlapSec - pitStopTime)
																)

  return(list(bolsteredPitStopDF = bolsteredPitStopDF,
							pitStopSummaryByRace = pitStopSummaryByRace))
}

.InlapOutlapDeltaDisplay = function(myPitStopDF, myRace, overwrittenUpperYLim = NULL, addedRaceDF, inlapOutlapDelta) {
	myXLim = with(myPitStopDF, range(c(inlapPredSec, outlapPredSec), na.rm = TRUE))
	myRaceLowerYLim = with(myPitStopDF, min(c(inlapPredSec, outlapPredSec, inlapSec, outlapSec), na.rm = TRUE))
  if (is.null(overwrittenUpperYLim)) {
    myRaceUpperYLim = with(myPitStopDF, max(c(inlapPredSec, outlapPredSec, inlapSec, outlapSec), na.rm = TRUE))
  }
  if (!is.null(overwrittenUpperYLim)) {
    myRaceUpperYLim = overwrittenUpperYLim
  }

	if (nrow(addedRaceDF) == 0) {
	  addedRaceLowerYLim = NULL
	  addedRaceUpperYLim = NULL
	}
  if (nrow(addedRaceDF) > 0) {
    addedRaceLowerYLim = min(myXLim[1] + c(addedRaceDF$inlapDelta, addedRaceDF$outlapDelta))
    addedRaceUpperYLim = max(tail(myXLim, 1) + c(addedRaceDF$inlapDelta, addedRaceDF$outlapDelta))
  }
  
	lowerYLim = min(c(myRaceLowerYLim, addedRaceLowerYLim))
	upperYLim = max(c(myRaceUpperYLim, addedRaceUpperYLim))
  myYLim = c(lowerYLim, upperYLim)
  
  with(myPitStopDF, plot(inlapPredSec, inlapPredSec, col = 'red', pch = 16,
													xlim = myXLim, ylim = myYLim,
													main = myRace))
  with(myPitStopDF, points(inlapPredSec, inlapSec, col = 'red'))
  # that's including the pit stop time potentially
  with(myPitStopDF, points(outlapPredSec, outlapPredSec, col = 'green', pch = 16))
  with(myPitStopDF, points(outlapPredSec, outlapSec, col = 'green'))

  # now display the delta
  rawInlapDelta = with(myPitStopDF, median(inlapSec - inlapPredSec, na.rm = TRUE))
  inlapPredSecRange = with(myPitStopDF, range(inlapPredSec, na.rm = TRUE))
  with(myPitStopDF, lines(inlapPredSecRange, inlapPredSecRange + rawInlapDelta, col = 'red'))

  rawOutlapDelta = with(myPitStopDF, median(outlapSec - outlapPredSec, na.rm = TRUE))
  outlapPredSecRange = with(myPitStopDF, range(outlapPredSec, na.rm = TRUE))
  with(myPitStopDF, lines(outlapPredSecRange, outlapPredSecRange + rawOutlapDelta, col = 'green'))

  if (nrow(addedRaceDF) > 0) {
    for (yi in 1:nrow(addedRaceDF)) {
      with(myPitStopDF, lines(inlapPredSecRange, inlapPredSecRange + addedRaceDF$inlapDelta[yi],
                                col = 'red', lty = 3))
      with(myPitStopDF, lines(outlapPredSecRange, outlapPredSecRange + addedRaceDF$outlapDelta[yi],
                                col = 'green', lty = 3))
      # but need to label them along with their  number of standard stops
      text(inlapPredSecRange[1], (inlapPredSecRange + addedRaceDF$inlapDelta[yi])[1],
                with(addedRaceDF[yi,], paste(year, numStandardInlap, sep = ' / ')),
                cex = 0.7, adj = c(0, 0))
      text(outlapPredSecRange[1], (outlapPredSecRange + addedRaceDF$outlapDelta[yi])[1],
                with(addedRaceDF[yi,], paste(year, numStandardOutlap, sep = ' / ')),
                cex = 0.7, adj = c(0, 0))
    }
  }

  if (!is.null(inlapOutlapDelta)) {
    lines(inlapPredSecRange, inlapPredSecRange + inlapOutlapDelta[1], col = 'red', lty = 1, lwd = 2)
    lines(inlapPredSecRange, inlapPredSecRange + inlapOutlapDelta[2], col = 'green', lty = 1, lwd = 2)
  }
  
  legend(myXLim[1], upperYLim, c('inlap', 'outlap'), lty = 1, col = c('red', 'green'),
          xjust = 0, yjust = 1)
  legend(myXLim[1] + 1, upperYLim, c('observed', 'actual'), pch = c(1, 16),
         xjust = 0, yjust = 1)
  
  return(list(rawInlapDelta = rawInlapDelta, rawOutlapDelta = rawOutlapDelta))
}


.InlapOutlapDeltaRetrieveRelevantOtherRace = function(addedRaceDF, myRace, raceDF) {
  myCircuit = with(raceDF, circuit[race == myRace])
  myDaynum = with(raceDF, daynum[race == myRace])
  relevantRace = raceDF %>%
                    filter(circuit == myCircuit & race != myRace &
																	isValidRace4 & doneInlapOutlapDelta &
																	 (numStandardInlap > 0 & numStandardOutlap > 0)) %>%
                    select(race, daynum)
  relevantRace$daynumDelta = abs(relevantRace$daynum - myDaynum)
  # but they're not relevant if we've already got them
  relevantRace = anti_join(relevantRace, addedRaceDF, 'race')
  # so we'll add in the previous race to the list
  if (length(relevantRace) == 0) {
    message('No more races to add!')
  }
  if (length(relevantRace) > 0) {
    raceToAdd = relevantRace %>%
                arrange(daynumDelta) %>%
                slice(1) %>%
                pull(race)
    addedRaceDF = bind_rows(addedRaceDF,
                          raceDF %>%
                            filter(race == raceToAdd) %>%
                            select(race, year, inlapDelta, outlapDelta,
																		numStandardInlap, numStandardOutlap))
  }
  return(addedRaceDF)
}

.MakeSingleRaceInlapOutlapDelta = function(myRace, bolsteredPitStopDF, raceDF) {
  # so we try to do it automatically (allowing user to check it) but if it's a really messy race we might have to use data from other season to suggest the deltas
  myPitStopDF = bolsteredPitStopDF %>% filter(race == myRace)

  # there may be no actual pitstops, if that happens, just take the previous year's inlap/outlap delta
  # possible it rains in the first year at a race...worry about that when it happens
  if (nrow(myPitStopDF) == 0) {
    myCircuit = with(raceDF, circuit[race == myRace])
    myDaynum = with(raceDF, daynum[race == myRace])
    relevantRace = raceDF %>%
      filter(circuit == myCircuit & race != myRace &
               isValidRace4 & doneInlapOutlapDelta &
               daynum < myDaynum &
               (numStandardInlap > 0 & numStandardOutlap > 0)) %>%
      select(race, daynum, inlapDelta, outlapDelta)
    inlapDelta = tail(relevantRace$inlapDelta, 1)
    outlapDelta = tail(relevantRace$outlapDelta, 1)
  }
  
  if (nrow(myPitStopDF) > 0) {
  	print(myPitStopDF %>%
  				select(race, driver, endLap, pitStopTime, inlapPredSec, inlapSec, outlapPredSec, outlapSec) %>%
  				arrange(endLap))
  
    satis = FALSE
    overwrittenUpperYLim = NULL
    addedRaceDF = raceDF %>%
  								select(race, year, inlapDelta, outlapDelta, numStandardInlap, numStandardOutlap) %>%
  								slice(0)
    inlapDelta = NULL
    outlapDelta = NULL
    while(!satis) {
      suggestedDelta = f1gaptrafficpitstop:::.InlapOutlapDeltaDisplay(myPitStopDF,
                                        myRace = myRace,
                                        overwrittenUpperYLim = overwrittenUpperYLim,
                                        addedRaceDF = addedRaceDF,
                                        c(inlapDelta, outlapDelta))
  
      message('If you are happy with the suggestion, press \'h\'\nIf you want to adjust the upper y limit, press \'y\'\nIf you want to add a suggestion from a previous year, press \'a\'\nIf you want to click on the graph to force the inlapDelta and outlapDelta, press \'io\'')
      userInput = askcond(FALSE, FALSE)
      if (userInput == 'y') {
        message('What do you wnat the new upper y limit to be?')
        overwrittenUpperYLim = scan(quiet = TRUE, nmax = 1)
      }
      if (userInput == 'h') {
        inlapDelta = suggestedDelta$rawInlapDelta
        outlapDelta = suggestedDelta$rawOutlapDelta
        satis = TRUE
      }
      if (userInput == 'a') {
        addedRaceDF = f1gaptrafficpitstop:::.InlapOutlapDeltaRetrieveRelevantOtherRace(addedRaceDF, myRace, raceDF)
      }
      if (userInput == 'io') {
        message('Click on a place that you think represents a sensible location for the inlap (RED) line')
        clickLocation = locator(n = 1)
        inlapDelta = with(clickLocation, y - x)
        message('Click on a place that you think represents a sensible location for the outlap (GREEN) line')
        clickLocation = locator(n = 1)
        outlapDelta = with(clickLocation, y - x)
      }
    }
  }

  return(list(inlapDelta = inlapDelta, outlapDelta = outlapDelta))
}

ProcessInlapOutlapDelta = function() {

	LoadAllData()
	lbl = f1laptimelm::MakePredSec(lbl, 4)
	stintDF = f1messystint:::DetectStintIssue(stintDF, lbl, isPreDelta = TRUE)
	dum = f1gaptrafficpitstop:::.BolsterPitStopDF(lbl, stintDF)
	bolsteredPitStopDF = dum$bolsteredPitStopDF
	pitStopSummaryByRace = dum$pitStopSummaryByRace
	raceDF = lazy_left_join(raceDF,
													pitStopSummaryByRace,
													'race',
													c('numStandardInlap', 'numStandardOutlap'))

	rrToModel = with(raceDF, rr[!doneInlapOutlapDelta])

	if (length(rrToModel) > 0) {
		for (ri in rrToModel) {
			if (raceDF$isValidRace4[ri]) {
				myInfo = f1gaptrafficpitstop:::.MakeSingleRaceInlapOutlapDelta(raceDF$race[ri],
																																				bolsteredPitStopDF,
																																				raceDF)
				raceDF[ri, c('inlapDelta', 'outlapDelta')] =
					with(myInfo, c(inlapDelta, outlapDelta))
			}
			raceDF$doneInlapOutlapDelta[ri] = TRUE
			sqlLazyUpdate(raceDF[ri,], 'race', 'race', 'doneInlapOutlapDelta')
			if (raceDF$isValidRace4[ri]) {
				sqlLazyUpdate(raceDF[ri,], 'race', 'race',	c('inlapDelta', 'outlapDelta'))
			}
			message('Have processed inlap/outlap deltas for', raceDF$race[ri])
		}
	}
}

CalculateMedianPitStopTime = function(raceDF) {
	lbl = f1validity::MakeIsRogue(lbl, inlapOutlapAreRogue = FALSE)
	lbl = lazy_left_join(lbl, rddf, c('race', 'driver'), 'mod4PredNValid')
	lbl$isGoodInlap = with(lbl, !isRogue & !isOutlier0 & mod4PredNValid > 0)
	# what is wrong with penalty pit stop times?
	lbl = f1gaptrafficpitstop::AlignPitStopData(lbl)
	inlapOutlapDelta = lbl %>%
						group_by(race) %>%
						filter(isGoodInlap) %>%
						summarise(medianPitStopTime = median( pitStopTime[inlap & !penalty]))

	raceDF = lazy_left_join(raceDF, inlapOutlapDelta, 'race', 'medianPitStopTime')

	return(raceDF)
}

CheckForSuspiciousInlapOutlap = function(lbl) {

	lbl = f1laptimelm::MakePredSec(lbl, 4)
	# but we only want the newly susicous ones, so check that these haven't already been cleared
	checkedSuspiciouslyFastPitLap = ReadF1Data('data/suspiciously-fast-pit-lap.csv', 'suspiciouslyFastPitLap')
	lbl = lazy_left_join(lbl, checkedSuspiciouslyFastPitLap, c('race', 'driver', 'lap'), 'isOK')

	suspiciouslyFastIndex = with(lbl, which(impsec < mod4PredSec - 3 &
																					!is.na(mod4PredSec) &
																					!is.na(impsec) &
																					(inlap | outlap) &
																					!isOK))

	if (length(suspiciouslyFastIndex) > 0) {
		suspiciouslyFastDF = lbl[suspiciouslyFastIndex, c('race','driver','lap','inlap','outlap', 'impsec', 'mod4PredSec')]
		print(suspiciouslyFastDF %>%
			arrange(race))
	}
}

CorrectPitLinePositionProblem = function(lbl) {

	# let's first check to see if there are surprising new additions to the list of drivers who've had suspicious fast laps

	pitStopTimeingLineCorrectionDF = ReadF1Data('data/pit-stop-timing-line-correction.csv',
												'PitStopTimingLineCorrection')
	lbl = indicate_overlapping_combination(
				lbl,
				pitStopTimeingLineCorrectionDF,
				c('race', 'driver'),
				'raceDriverNeedsCorrecting')

	lbl = subset_join(lbl,
						pitStopTimeingLineCorrectionDF,
						c('race', 'driver'),
						raceDriverNeedsCorrecting)

	lbl = lbl %>%
			mutate_cond(raceDriverNeedsCorrecting & inlap,
							sec = sec + inlapAddition) %>%
			mutate_cond(raceDriverNeedsCorrecting & outlap,
							sec = sec - inlapAddition)

	lbl = lbl %>%
			select(-c(raceDriverNeedsCorrecting, inlapAddition))

	return(lbl)
}

AdjustLapTimeForPitStop = function(lbl, includeIntermediateColumn = FALSE) {

	# before doing anything, there was that problem for the pit lane line in valencia. we should correct for that plus add in something that is on alert for other times it might happen

	lbl = f1laptimelm::MakePredSec(lbl, 4)
	stintDF = f1messystint:::DetectStintIssue(stintDF, lbl, isPreDelta = TRUE)
	dum = f1gaptrafficpitstop:::.BolsterPitStopDF(lbl, stintDF)
	pitStopSummaryByRace = dum$pitStopSummaryByRace
	lbl = lazy_left_join(lbl, pitStopSummaryByRace, 'race', 'ioToAdjust')
	lbl = lazy_left_join(lbl, raceDF, 'race', c('inlapDelta', 'outlapDelta', 'isValidRace4'))
	lbl = f1gaptrafficpitstop::AlignPitStopData(lbl)

	lbl = lbl %>%
		mutate(inlapOutlapAdjustment = 0,
		        startTimeElapsedAdjustment = 0,
		        endTimeElapsedAdjustment = 0) %>%
		mutate_cond(inlap,
								inlapOutlapAdjustment = inlapOutlapAdjustment - inlapDelta,
								endTimeElapsedAdjustment = endTimeElapsedAdjustment - inlapDelta) %>%
		mutate_cond(inlap & isValidRace4 & ioToAdjust == 'inlap',
								inlapOutlapAdjustment = inlapOutlapAdjustment - pitStopTime,
								endTimeElapsedAdjustment = endTimeElapsedAdjustment - pitStopTime) %>%
		mutate_cond(outlap,
								inlapOutlapAdjustment = inlapOutlapAdjustment - outlapDelta,
								startTimeElapsedAdjustment = startTimeElapsedAdjustment + outlapDelta) %>%
		mutate_cond(outlap & isValidRace4 & ioToAdjust == 'outlap',
								inlapOutlapAdjustment = inlapOutlapAdjustment - prevLapPitStopTime,
								startTimeElapsedAdjustment = startTimeElapsedAdjustment + prevLapPitStopTime) %>%
		mutate(impsec = sec + inlapOutlapAdjustment)
	# sign of adjustment is different for impsec and startTime which is confusing.
	# That's because we want to take the time away from impsec because it's artificial to include in the lap time
	# But we want to shift the car backwards when doing an outlap, so we want to include that time

	# remove unwanted intermediate columns
	lbl = lbl %>% select(-c(mod4PredSec, ioToAdjust))

	if (!includeIntermediateColumn) {
		lbl = lbl %>%
				select(-c(inlapOutlapAdjustment, startTimeElapsedAdjustment, endTimeElapsedAdjustment,
				          inlapDelta, outlapDelta, pitStopTime, prevLapPitStopTime, penalty))
	}

	f1gaptrafficpitstop::CheckForSuspiciousInlapOutlap(lbl)

	return(lbl)
}

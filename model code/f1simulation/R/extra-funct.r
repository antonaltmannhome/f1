### these are functions that are nice for getting insights from the simulated data

RetrieveFinPosProb = function(lbl, myFinPosList) {
	pastedFinPos = paste(myFinPosList, collapse = ',')
	sqlComm = paste0('select * from finishingpositionprobability where finPos in (', pastedFinPos, ')')
	verticalDF = sqlQuery2(sqlComm)
	horizDF = spread(verticalDF, key = finPos, value = probability, sep = '')
	lbl = join_on_overlap(lbl, horizDF, c('race', 'driver', 'lap'))
	return(lbl)
}

.DriverRaceHeadToHeadExtendDF = function(myDriverLapFinPos) {
	with(myDriverLapFinPos, tibble(driver = rep(driver, n), finPos = rep(finPos, n)))
}

.DriverRaceHeadToHeadByLap = function(myVerticalDF, myDriv1, myDriv2) {
	extendedVerticalDF = myVerticalDF %>%
												rowwise() %>%
												do(.DriverRaceHeadToHeadExtendDF(.))
	extendedHorizDF = with(extendedVerticalDF,
														cbind(resample(finPos[driver == myDriv1]),
																	resample(finPos[driver == myDriv2])))

	myProbDF = tibble(driver1 = myDriv1,
										driver2 = myDriv2,
										probD1BeatD2 = mean(extendedHorizDF[,1] < extendedHorizDF[,2]))
	return(myProbDF)
}

DriverRaceHeadToHead = function(myRace, myDriv1, myDriv2, numberOfSimulations = 1000) {
	# is this quite how you'd want to run it? maybe you'd want to send in a data frame of race/lap/driver1/driver2 instead
  sqlComm = paste0('select * from finishingpositionprobability where race = "', myRace, '" and driver in ("', myDriv1, '", "', myDriv2,'")')
  verticalDF = sqlQuery2(sqlComm)
  verticalDF = verticalDF %>%
								mutate(n = 1000 * probability)

	headToHeadProbByLap = verticalDF %>%
												group_by(lap) %>%
												do(.DriverRaceHeadToHeadByLap(., myDriv1 = myDriv1, myDriv2 = myDriv2))

	return(headToHeadProbByLap)
}

DriverRaceCompareSituation = function(myRace, myDriv1, myDriv2, toFile = FALSE) {
  # there might be a better name for this function, easily confused with DriverRaceHeadToHead
  # that one comapres two drivers' chance of beating each other.
  # this one just dispalys where two drivers were on track to finish and their probability of finshing there
  # useful to see if two team mates both had/didn't have an incentive at various points of race
  vertDF = lbl %>%
            filter(race == myRace & driver %in% c(myDriv1, myDriv2))
  
  # fairly sure these next two lines are completely pointless:
  horizDF = vertDF %>%
            select(lap, driver, meanFinPos, modalFinPosProb) %>%
            spread_multiple(keyCol = driver, meanFinPos, modalFinPosProb)
  names(horizDF) = gsub('driver', '', names(horizDF))
  
  positionPlot = ggplot(vertDF) +
                  geom_line(aes(x = lap, y = meanFinPos, col = driver)) +
                  ylab('expected finishing position')
  probabilityPlot = ggplot(vertDF) +
                  geom_line(aes(x = lap, y = modalFinPosProb, col = driver)) +
                  ylab('probability of finishing in\nmost likely finishing position')
  myPlot = gridExtra::grid.arrange(positionPlot, probabilityPlot, nrow = 2)
  
  if (toFile) {
    fileOut = MakeRaceFile(myRace, paste0('driver-head-to-head-simulation-', myDriv1, '-', myDriv2, '.png'))
    aafunct::NiceGgSave(myPlot = myPlot, myFile = fileOut)
  }
  
  print(myPlot)
}

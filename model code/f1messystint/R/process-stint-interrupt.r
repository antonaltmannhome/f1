
CheckStintForInterrupt = function(myRace, mydriv, stintDF) {
	print(f1data:::ViewDriverComment(myRace, mydriv))
	## now offer user the chance to declare a dodgy end of stint
	finished = FALSE
	driverRaceStintIndex = with(stintDF, which(race == myRace & driver == mydriv))
	while(!finished) {
		cat('\n')
		coltoprint = c('driver', 'stint', 'startLap', 'endLap', 'tyre', 'newTyre', 'stintRetired',
		'isSafetyCar', 'isRed',
		'slowStop', 'pitStopTime', 'medianPitStopTime',
		'slowInlap', 'inlapDelta', 'shortStint', 'lateStop', 'toQuery', 'interrupt')
		print(stintDF[driverRaceStintIndex, coltoprint])
		cat('\nWhich stop denotes a unplanned end to stint (press 0 when finished)?\n')
		stopnum = askcond(TRUE, FALSE)
		if (stopnum == 0) {
			finished = TRUE
			next
		}
		if (!between(stopnum, 1, length(driverRaceStintIndex))) {
			print('Invalid entry, please enter again')
			next
		}
		stintDF$interrupt[driverRaceStintIndex[stopnum]] = TRUE
	}
	### the ones you've not denoted as interrupted can therefore be declared fine
	undeclaredDriverRaceStintIndex = driverRaceStintIndex[which(is.na(stintDF$interrupt[driverRaceStintIndex]))]
	stintDF$interrupt[undeclaredDriverRaceStintIndex] = FALSE

	toReturn = stintDF[driverRaceStintIndex, c('race', 'driver', 'stint', 'interrupt')]

	return(toReturn)
}

ProcessInterruptedStint = function() {
	LoadAllData()
	lbl = f1gaptrafficpitstop:::AdjustLapTimeForPitStop(lbl)
	lbl = f1laptimelm:::MakePredSec(lbl, 30, adjustForCarProblem = TRUE)
	stintDF = f1messystint:::DetectStintIssue(stintDF, lbl, isPreDelta = FALSE)
	stintDF = f1messystint:::BolsterStintWithModelInfo(stintDF)

	### the lap1/retired/safety car ones need no checking, they're interrupted
	stintDF$interrupt[with(stintDF, which( lap1Stop | isSafetyCar | stintRetired | dryToWet))] = TRUE

	# might seem weird to have an asterisk rather than a TRUE/FALSE but it's easier to spot when manually checking
	stintDF$toQuery = ''
	stintDF$toQuery[with(stintDF, !(!is.na(interrupt)  & interrupt) &
									(slowStop | shortStint | lateStop | (!is.na(slowInlap) & slowInlap)))] = '*'

	rrToModel = with(raceDF, rr[!doneStintInterrupt])

	for (ri in rrToModel) {

		cat('About to process',raceDF$race[ri],'\n')

		if (raceDF$isValidRace30[ri]) {

			checkedInterruptedStintFile = MakeRaceFile(raceDF$race[ri], 'checked-interrupted-stint.csv')
			alreadyDone = file.exists(checkedInterruptedStintFile)
			if (!alreadyDone) {

				f1plot:::StintSummary(raceDF$race[ri])

				### but the other issues, we need to query to see if they are actually interrupted

				# generally easiest to look at driver's entire race in one go, so cycle through all drivers who have any interruption
				toQueryDriver = with(stintDF, unique(driver[which(rr == ri & toQuery == '*')]))
				if (length(toQueryDriver) > 0) {
					for (di in 1:length(toQueryDriver)) {

						dum = f1messystint:::CheckStintForInterrupt(raceDF$race[ri], toQueryDriver[di], stintDF)
						stintDF = join_on_overlap(stintDF, dum, c('driver', 'race', 'stint'))
					}
				}

				### then write the checked interruption data to disk
				checkedStintIndex = with(stintDF, which(rr == ri & toQuery == '*'))
				write_csv(path = checkedInterruptedStintFile, x = stintDF[checkedStintIndex, c('driver', 'stint', 'interrupt')])
			}

			if (alreadyDone) {
				myCheckedInterruptedStint = ReadF1Data(checkedInterruptedStintFile, 'checkedInterruptedStint') %>%
									mutate(race = raceDF$race[ri])
				stintDF = join_on_overlap(stintDF, myCheckedInterruptedStint, c('race', 'driver', 'stint'))
			}

			checkedStintIndex = with(stintDF, which(rr == ri & !is.na(stintDF$interrupt)))
			if (length(checkedStintIndex) > 0) {
				sqlLazyUpdate(stintDF[checkedStintIndex,],
								'stint',
								c('race', 'driver', 'stint'),
								'interrupt')
			}
		}

		raceDF$doneStintInterrupt[ri] = TRUE
		sqlLazyUpdate(raceDF[ri,],'race','race','doneStintInterrupt')
		cat('Have processed all interrupted stints for',raceDF$race[ri],'\n')
	}
}

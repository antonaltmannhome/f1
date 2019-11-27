### we've got a problem, that we didn't record when a stint WAS interrupted in old world but DIDN@T have a ny further pitstops
# so, we can't distinguish between:
### pitstops that are newly defined as interrupted therefore need to be checked
##			 and
### pitstops that had been interrupted before but had no further pit stops

### so, we'll loop through all examples of interrupted stints in new world that don't have an alternative strategy and update to say that we've checked them

# in old world:
if (FALSE) {
	write.csv(file='c:/temp/temp.csv',stintdb[,c('rr', 'driver','stint','interrupt')],row.names=F)

	# in new world, after resetting all doneGuessedPitStops to FALSE:

	LoadAllData()
	b = read.csv('c:/temp/temp.csv') %>%
			mutate(doneGuessedPitStop = (!is.na(interrupt) & interrupt==1))
	stintDF = lazy_left_join(stintDF, b, c('rr', 'driver', 'stint'), 'doneGuessedPitStop')
	# it's the ones that are TRUE interrupt and !doneGuessedPitStop that we need to recheck
	sqlLazyUpdate(stintDF, 'stint', c('race', 'driver', 'stint'), 'doneGuessedPitStop')
	
	## no, it's more fucked than that, there are some occasions where interrupt is NA in old world but they still have a guessed strategy, need to pick those up too
	
myList = NULL
for (ri in 1:nrace) {
	if (raceDF$isValidRace30[ri]) {
		guessedPitStopFile=MakeRaceFile(raceDF$race[ri], 'guessed-pitstop.csv')
		if (file.exists(guessedPitStopFile)) {
			myList[[ri]] = read.csv(file = guessedPitStopFile, as.is = TRUE)
			myList[[ri]]$race = raceDF$race[ri]
		}
	}
}
guessedPitStopDF = bind_rows(myList)

stintDF = indicate_overlapping_combination(stintDF,
									guessedPitStopDF %>%
									rename(stint = vantageStint),
									c('race', 'driver', 'stint'),
									'guessStratOnFile')

stintDF$doneGuessedPitStop[stintDF$guessStratOnFile] = TRUE
mustUpdateIndex = which(stintDF$guessStratOnFile)
	# it's the ones that are TRUE interrupt and !doneGuessedPitStop that we need to recheck
	sqlLazyUpdate(stintDF, 'stint', c('race', 'driver', 'stint'), 'doneGuessedPitStop')

}

LoadAllData()

# did stint end with a redflag/safetycar/racefinishing under safety car?
rddf$isRetirement = f1data:::DetectRetirement(rddf)
stintDF = lazy_left_join(stintDF, rddf, c('race', 'driver'), c('team', 'isRetirement', 'maxLap'))
stintDF$stintRetired = with(stintDF, endLap == maxLap & isRetirement)
stintDF$stintFinishRace = with(stintDF, endLap == maxLap & !isRetirement)
stintDF = within(stintDF, rm(maxLap))

stintDF = indicate_overlapping_combination(stintDF,
											lbl %>% filter(isCarProblem),
											c('race', 'driver', 'stint'),
											'isCarProblem')

stintDF = lazy_left_join(stintDF,
							lbl %>% rename(endLap = lap),
							c('race', 'driver', 'endLap'),
							c('isSafetyCar', 'isRed'))
stintDF$isSCFinish = with(stintDF, !isRetirement & stintFinishRace & (isSafetyCar | isRed))

stintDF$isMessy = with(stintDF, !isSCFinish & ((!is.na(interrupt) & interrupt) | (isCarProblem & !stintFinishRace)))

stintDF$newTyre = with(stintDF, tyre[match(paste(race, driver, stint + 1), paste(race, driver, stint))])

myList = NULL
for (ri in 1:nrace) {
	if (raceDF$isValidRace30[ri]) {
		guessedPitStopFile=MakeRaceFile(raceDF$race[ri], 'guessed-pitstop.csv')
		if (file.exists(guessedPitStopFile)) {
			myList[[ri]] = read.csv(file = guessedPitStopFile, as.is = TRUE)
			myList[[ri]]$race = raceDF$race[ri]
		}
	}
}
guessedPitStopDF = bind_rows(myList)

stintDF = indicate_overlapping_combination(stintDF,
									guessedPitStopDF %>%
									rename(stint = vantageStint),
									c('race', 'driver', 'stint'),
									'hasDoneGuessedStop')
stintDF$needToCheck = with(stintDF, isMessy & !hasDoneGuessedStop)

guessedStopList = list()
for (si in which(stintDF$needToCheck)) {
	### display overall strategies
	HackStintSummary(stintDF$race[si], stintDF$driver[si], stintDF$stint[si])
	
	print(f1data:::ViewDriverComment(stintDF$race[si], stintDF$driver[si]))
	guessedStopList[[si]] = GuessPitStop(stintDF$race[si], stintDF$driver[si], stintDF$stint[si], miniTyreDF)
	stintDF$doneGuessedPitStop[si] = TRUE
}

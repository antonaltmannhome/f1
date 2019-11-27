
### now create all the alternative reality projections
### we want to have ALL drivers, from all vantage points, catered for here - so we have a file saying for any vantage point what the strategy for all drivers was going to be
### so i think best approach is to loop through drivers, as long as all is ok, vantage point keeps going up - if there's a badend of any sort, then we need to halt the vantage point and start putting in alternative futures

### this maybe should be put into a function, copied from process_guessed_pitstop

### bloody complicated, let's put it all into a function so that we can simulate scenarios
GenerateAlternativePitStop = function(currentDriverStintDF, currentDriverGuessedStopDF = NULL) {
	universeCount = 0 # how many universes are created within race by this driver
	newUniverse = TRUE
	pitStopList = NULL
	for (si in 1:nrow(currentDriverStintDF)) {
		## this restarts whenever driver changes, or whenever a bad stint happens
		if (newUniverse) {
			universeCount = universeCount + 1
			vantageStartLap = currentDriverStintDF$startLap[si]
		}
		### was there a clean end to the stint?
		if (!currentDriverStintDF$hasGuessedPitStop[si]) {
			### is it the final stint? if not, insert the accepted tyre strategy info in to mypitstoparr
			if (with(currentDriverStintDF[si,], isPitStop & !interrupt)) {
				rowtoadd = with(currentDriverStintDF[si,],tibble(vantageStartLap = vantageStartLap, vantageEndLap = NA, inlap = endLap, newTyre = newTyre))
				if (newUniverse) {
					pitStopList[[universeCount]] = rowtoadd
				}
				if (!newUniverse) {
					pitStopList[[universeCount]] = rbind(pitStopList[[universeCount]], rowtoadd)
				}
			}
			### if it is the final stint, then let us know that the vantage point extends to end of driver's race
			if (with(currentDriverStintDF[si,], !isPitStop | interrupt)) {
				### if no pitstops in the universe, still need to initialise it
				if (newUniverse) {
					pitStopList[[universeCount]] = with(currentDriverStintDF[si,], tibble(vantageStartLap = vantageStartLap, vantageEndLap = NA, inlap = NA, newTyre = NA))
				}
				pitStopList[[universeCount]]$vantageEndLap = currentDriverStintDF$endLap[si]
			}
			newUniverse = FALSE
		}
		if (currentDriverStintDF$hasGuessedPitStop[si]) {
			### at this point we should look to see if we've filled info for this race already
			guessedIndex=which(currentDriverGuessedStopDF$vantageStint == currentDriverStintDF$stint[si])
			for (j in 1:length(guessedIndex)) {
				### need to initialise a new row within this universe
				rowtoadd = with(currentDriverGuessedStopDF[guessedIndex[j],],tibble(vantageStartLap = vantageStartLap, vantageEndLap = NA,inlap = lap,newTyre = newTyre))
				if (newUniverse) {
					pitStopList[[universeCount]] = rowtoadd
				}
				if (!newUniverse) {
					pitStopList[[universeCount]] = rbind(pitStopList[[universeCount]], rowtoadd)
				}
				newUniverse=F
			}
			### this stint was interrupted so info only valid to end of this stint
			pitStopList[[universeCount]]$vantageEndLap = currentDriverStintDF$endLap[si]
			### we've completed the view of what would have happened if this bad ending had not taken place - now switch back to reality, ie add another universe
			newUniverse = TRUE
		}
	}
	myPitStopDF = do.call(rbind,pitStopList)
	myPitStopDF$driver = currentDriverStintDF$driver[1]
	myPitStopDF = myPitStopDF %>% select(driver, everything())
	return(myPitStopDF)
}

ProcessAlternativeStrategy = function() {
	LoadAllData()

	lbl = f1gaptrafficpitstop:::AdjustLapTimeForPitStop(lbl)
	lbl = f1laptimelm:::MakePredSec(lbl, 30, adjustForCarProblem = TRUE)
	stintDF = f1messystint:::DetectStintIssue(stintDF, lbl, isPreDelta = FALSE)
	stintDF = f1messystint:::BolsterStintWithModelInfo(stintDF)

	rrToModel = with(raceDF, rr[!doneAlternativeStrategy])

	for (ri in rrToModel) {
		if (raceDF$isValidRace30[ri]) {
			miniStintDF = stintDF %>% filter(rr == ri)

			### scan in guessed pitstops
			guessedPitStopFile = MakeRaceFile(raceDF$race[ri], 'guessed-pitstop.csv')
			if (!file.exists(guessedPitStopFile)) {
				message('Warning: have not found any guessed pitstops for ', raceDF$race[ri],', does this sound right (y/n)?')
				satis = FALSE
				while(!satis) {
					dum = askcond(F, F)
					if (dum == 'y') satis = TRUE
					if (dum == 'n') stop('Ok, have a check\n')
					if (!dum %in% c('y', 'n')) print('Invalid entry, please try again')
				}
			}
			haveGuessedPitStop=file.exists(guessedPitStopFile)
			if (haveGuessedPitStop) {
				guessedStopDF = ReadF1Data(guessedPitStopFile, 'guessedPitStop')

				### indicate which stints had messy ends that we needed to make guesses for
				### note that this doesn't include all drivers who had messy ends, just the ones who we think would have pitted again if it wasn't for the messy ending
				miniStintDF = indicate_overlapping_combination(miniStintDF,
															guessedStopDF %>% rename(stint = vantageStint),
															c('driver', 'stint'),
															'hasGuessedPitStop')
			}
			if (!haveGuessedPitStop) {
				miniStintDF$hasGuessedPitStop = FALSE
				guessedStopDF = tibble(driver = character(), vantageStint = integer(), lap = integer(), newTyre = character())
			}

			currentRaceDriver = unique(miniStintDF$driver)
			myPitStopList=NULL
			for (di in 1:length(currentRaceDriver)) {
				currentDriverStintDF = miniStintDF %>%
										filter(driver == currentRaceDriver[di]) %>%
										select(driver, stint, startLap, endLap, newTyre, isPitStop, interrupt, hasGuessedPitStop) %>%
										### total pain in the arse not to do this
										mutate(interrupt = ifelse(!is.na(interrupt), interrupt, FALSE))
				currentDriverGuessedStopDF = guessedStopDF %>% filter(driver == currentRaceDriver[di])
				myPitStopList[[di]] = f1messystint:::GenerateAlternativePitStop(currentDriverStintDF, currentDriverGuessedStopDF)
				cat('Have processed all strategies for', currentRaceDriver[di],'for', raceDF$race[ri], '\n')
			}
			combinedPitStopDF = bind_rows(myPitStopList)
			alternativeStrategyFile = MakeRaceFile(raceDF$race[ri], 'alternative-strategy.csv')
			write_csv(combinedPitStopDF, path = alternativeStrategyFile)
		}

		raceDF$doneAlternativeStrategy[ri] = TRUE
		sqlLazyUpdate(raceDF[ri,],'race','race','doneAlternativeStrategy')
	}
}

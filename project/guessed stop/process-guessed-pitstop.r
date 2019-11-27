
GuessPitStop=function(myRace, myDriver, myStint, miniTyreDF) {
	onStintNumber = 0
	finished = FALSE

	stintInQuestion = stintDF %>% filter(race == myRace & driver == myDriver & stint == myStint)
	whatActuallyHappenedDF=stintDF %>% filter(race==myRace & driver== myDriver & stint >= myStint) %>% select(driver,stint,endLap,tyre,newTyre)
	myGuessedStopDF = tibble(driver = character(0), vantageStint = integer(0), tyre = character(0), lap = integer(0), newTyre = character(0))

	while(!finished) {
		### want to display what has happened so far
		print(whatActuallyHappenedDF)
		if (onStintNumber==0) cat('What lap do you expect',toupper(myDriver),'would have stopped on without stop on lap',stintInQuestion$endLap,'? (press 0 if finished)\n')
		if (onStintNumber>0) {
			### display what has been declared so far
			cat('**** what you\'ve chosen so far **** \n')
			print(myGuessedStopDF)
			cat('And then?\n')
		}
		satis = FALSE
		while(!satis) {
			guessLap = askcond(T,F)
			if (guessLap == 0 | (guessLap>=stintInQuestion$startLap & guessLap<raceDF$nlap[ri])) satis=T
			if (!satis) print('Incorrect entry, try again')
		}
		if (guessLap == 0) {
			finished = TRUE
		}
		if (guessLap != 0) {
			onStintNumber = onStintNumber + 1
			if (onStintNumber == 1) {
				### initialise tyre stops data frame
				myGuessedStopDF = tibble(driver = myDriver, vantageStint = myStint, tyre = stintInQuestion$tyre, lap = guessLap, newTyre = NA)
			}
			if (onStintNumber>1) {
				newStopDF = tibble(driver = myDriver, vantageStint = myStint, tyre = tail(myGuessedStopDF$tyre, 1), lap = guessLap, newTyre = NA)
				myGuessedStopDF  = rbind(myGuessedStopDF, newStopDF)
			}
			cat('And what tyres do you think he would have taken on?\n')
			print(miniTyreDF)
			satis = FALSE
			while(!satis) {
				guessTyre=askcond(F,F)
				if (guessTyre %in% miniTyreDF$abbreviation) satis = TRUE
				if (!satis) print('Incorrect entry, try again')
			}
			myGuessedStopDF$newTyre[onStintNumber] = miniTyreDF$tyre[which(miniTyreDF$abbreviation == guessTyre)]
		}
	}
	
	# 'tyre' was useful for displaying but is not necessary any more, get rid
	myGuessedStopDF = myGuessedStopDF %>% select(-tyre)
	return(myGuessedStopDF)
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

rrToModel = with(raceDF, rr[!doneGuessedPitStop])

appending = FALSE # use appending if we want to e.g. add a new rule for interruptions and ad additional data for races that had been considered 'done'

for (ri in rrToModel) {
	guessedPitStopFile = MakeRaceFile(raceDF$race[ri], 'guessed-pitstop.csv')
	if (raceDF$isValidRace30[ri]) {
		if (!appending) messyIndex=with(stintDF,which(rr == ri & isMessy))
		if (appending) {
			messyIndex=with(stintDF,which(rr == ri & !doneGuessedPitStop & isMessy))
			# if we're appending, helpful to show what decisions were made for other drivers (esp in case of safety car eg)
			if (length(messyIndex) > 0) {
				alreadyDoneGuessedPitStopDF = ReadF1Data(guessedPitStopFile, 'guessedPitStop')
				dum = alreadyDoneGuessedPitStopDF
				dum$race = raceDF$race[ri]
				dum = dum %>% select(race, everything())
				View(dum)
				}
		}
		if (length(messyIndex)>0) {
			
			miniTyreDF = lazy_left_join(raceTyreDF %>%
											filter(race == raceDF$race[ri]) %>%
											select(tyre),
										tyreDF,
										'tyre',
										c('abbreviation', 'colour'))
			
			guessedStopList = list()
			for (si in 1:length(messyIndex)) {
				### display overall strategies
				
				f1code:::StintSummary(raceDF$race[ri],
								focusDriverStint = list(driver = stintDF$driver[messyIndex[si]],
														stint = stintDF$stint[messyIndex[si]]))
				
				print(f1data:::ViewDriverComment(raceDF$race[ri], stintDF$driver[messyIndex[si]]))
				### display driver's actual race, stintwise:
				probsax=with(stintDF,which(rr==ri & driver==stintDF$driver[messyIndex[si]]))
				print(stintDF[probsax,c('race', 'driver', 'team', 'stint', 'startLap', 'endLap', 'tyre',
										'interrupt', 'isCarProblem', 'isSafetyCar','isRed','stintRetired')])
				
				guessedStopList[[si]] = GuessPitStop(raceDF$race[ri], stintDF$driver[messyIndex[si]], stintDF$stint[messyIndex[si]], miniTyreDF)
				### declare that this driver has been done
				stintDF$doneGuessedPitStop[messyIndex[si]] = TRUE
				message('Have done ', si, ' out of ', length(messyIndex))
			}
			
			### write that to disk
			myGuessedStopDF = bind_rows(guessedStopList)
						
			# but if we're appending, should add to what has already been done - check we're not overwriting anything already done
			if (appending) {
				doubledUp = length(intersect(with(myGuessedStopDF, paste(driver, vantageStint)),
											with(alreadyDoneGuessedPitStopDF, paste(driver, vantageStint)))) > 0
				if (doubledUp) {
					stop('Clash between what guessed pit stops have already been done and what you want to add, please investigate\n')
				}
				myGuessedStopDF = bind_rows(myGuessedStopDF, alreadyDoneGuessedPitStopDF)
			}

			if (nrow(myGuessedStopDF) > 0) {
				write_csv(myGuessedStopDF, path = guessedPitStopFile)
			}
		}
	}
	raceDF$doneGuessedPitStop[ri] = TRUE
	sqlLazyUpdate(raceDF[ri,],'race','race','doneGuessedPitStop')
	currentRaceMessyIndex = with(stintDF, which(rr == ri & isMessy))
	if (length(currentRaceMessyIndex) > 0) {
		sqlLazyUpdate(stintDF[currentRaceMessyIndex,], 'stint', c('race', 'driver', 'stint'), 'doneGuessedPitStop')
	}
	cat('Have processed all guessed pit stops for',raceDF$race[ri],'\n')
}


CheckStintForInterrupt = function(myRace, mydriv) {
	#print(commview(myRace,mydriv))
	## now offer user the chance to declare a dodgy end of stint
	finished = FALSE
	driverRaceStintIndex = with(stintDF, which(race == myRace & driver == mydriv))
	while(!finished) {
		cat('\n')
		coltoprint = c('driver', 'stint', 'startLap', 'endLap', 'tyre', 'newTyre', 'retired',
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

lbl = f1laptimelm:::MakePredSec(lbl, 30)
lbl = f1gaptrafficpitstop:::AdjustLapTimeForPitStop(lbl)
raceDF = f1gaptrafficpitstop::CalculatePitStopDelta(raceDF, lbl)
rddf$isRetirement = f1data:::DetectRetirement(rddf)

lbl = indicate_overlapping_combination(lbl,
								rddf %>% rename(lap = maxLap),
								c('race', 'driver', 'lap'),
								'isLastLapForDriver')
lbl$lapsDown = with(lbl, leadLap - lap)
rddf = lazy_left_join(rddf,
						lbl %>%
						filter(isLastLapForDriver),
						c('race', 'driver'),
						'lapsDown')
rddf$scheduledLastLap = raceDF$nlap[rddf$rr] - rddf$lapsDown

rddf = lazy_left_join(rddf, raceDF, 'race', 'circuit')

### and we want to know the difference between your lap time and what you should have done given pit stop time
lbl$inlapDelta=round(lbl$impsec-lbl$mod30PredSec, 2)
### that returns nonsensical stuff for non-inlaps, so NA then
lbl$inlapDelta[!lbl$inlap]=NA

stintDF = lazy_left_join(stintDF,
							lbl %>% rename(endLap = lap),
							c('race', 'driver', 'endLap'),
							c('impsec', 'mod30PredSec', 'isSafetyCar', 'isRed', 'inlapDelta'))
stintDF = lazy_left_join(stintDF,
							pitStopDF %>%
							rename(endLap = endOfLap),
							c('race', 'driver', 'endLap'),
							c('replaceTyre', 'pitStopTime', 'penalty'))
stintDF = lazy_left_join(stintDF, rddf, c('race', 'driver'), c('isRetirement', 'maxLap', 'scheduledLastLap'))
stintDF = lazy_left_join(stintDF, raceDF, 'race', c('year', 'medianPitStopTime'))

stintDF = indicate_overlapping_combination(stintDF,
											pitStopDF %>% rename(endLap = endOfLap),
											c('race', 'driver', 'endLap'),
											'isPitStop')
stintDF$newTyre = with(stintDF, tyre[match(paste(race, driver, stint + 1), paste(race, driver, stint))])
# but hold on isRetirment means they retired from the race. so it'll be true for all stints by a driver who retired from the race. but we want to know if they retired at the end of a specific stint
stintDF$retired = with(stintDF, (endLap == maxLap) & isRetirement)
stintDF = within(stintDF, rm(isRetirement, maxLap))
											
#### don't want to check stints at end of race, or where they were switching from wet to dry tyres
stintDF$isValidStop=with(stintDF, isPitStop &
									tyre %in% with(tyreDF, tyre[isDry]) &
									newTyre %in% with(tyreDF, tyre[isDry]))
									
### now construct the indicator variables to indicate potential issues

### this is fiddly, you can serve penalties at same time as pitstops from 2014 onwards, so want to take away the 5 seconds from pitstop times after 2014
stintDF = lazy_left_join(stintDF, yearGuideDF, 'year', 'couldChangeTyresAndServePenalty')

stintDF = stintDF %>%
			mutate(slowStop = case_when(!couldChangeTyresAndServePenalty ~
											isValidStop & !isSafetyCar & pitStopTime > medianPitStopTime + 3,
										couldChangeTyresAndServePenalty ~
											isValidStop & !isSafetyCar &
												pitStopTime - 5 * penalty > medianPitStopTime + 3))

### other ones that are worth querying are pitstops that occur after a suspiciously short amount of laps
stintDF$shortStint = with(stintDF, !isSafetyCar & endLap-startLap < 10)

### or with a suspiciously low number of laps left til the end of the race
stintDF$lateStop = with(stintDF, scheduledLastLap - endLap < 8)

### or with a suspiciously slow inlap
stintDF$slowInlap = with(stintDF, inlapDelta > 3)

stintDF$lap1Stop = with(stintDF, endLap <= 2)
stintDF$dryToWet = with(stintDF, !is.na(newTyre) &
									tyre %in% with(tyreDF, tyre[isDry]) &
									!newTyre %in% with(tyreDF, tyre[isDry]))
# if race finished under a safety car, then we don't want to count that 
									
### the lap1/retired/safety car ones need no checking, they're interrupted
stintDF$interrupt[with(stintDF, which(isValidStop & (lap1Stop | isSafetyCar | retired | dryToWet)))] = TRUE

# might seem weird to have an asterisk rather than a TRUE/FALSE but it's easier to spot when manually checking
stintDF$toQuery = ''
stintDF$toQuery[with(stintDF, !(!is.na(interrupt)  & interrupt) &
								(slowStop | shortStint | lateStop | (!is.na(slowInlap) & slowInlap)))] = '*'

rrToModel = with(raceDF, rr[!doneStintInterrupt])

for (ri in rrToModel) {
	
	cat('About to process',raceDF$race[ri],'\n')
	#sax = with(stintdb, which(racename == racedb$racename[ri]))
	#stintdb[sax, 'interrupt'] = 0
	
	if (raceDF$isValidRace30[ri]) {

		stintInterruptFile = MakeRaceFile(raceDF$race[ri], 'pitstop-interrupt.csv')
		alreadyDone = file.exists(stintInterruptFile)
		if (!alreadyDone) {

			f1plot:::StintSummary(raceDF$race[ri])

			### but the other issues, we need to query to see if they are actually interrupted
			
			# generally easiest to look at driver's entire race in one go, so cycle through all drivers who have any interruption
			toQueryDriver = with(stintDF, unique(driver[which(rr == ri & toQuery == '*')]))
			if (length(toQueryDriver) > 0) {
				for (di in 1:length(toQueryDriver)) {
					dum = CheckStintForInterrupt(raceDF$race[ri], toQueryDriver[di])
					stintDF = join_on_overlap(stintDF, dum, c('driver', 'race', 'stint'))
				}
			}
			
			### then write the full stintdb for this race to disk
			checkedStintIndex = with(stintDF, which(rr == ri & !is.na(stintDF$interrupt)))
			dum = stintDF[checkedStintIndex, c('driver', 'stint', 'interrupt')]
			write_csv(file = stintInterruptFile, dum)
		}
		
		if (alreadyDone) {
			myStintInterrupt = ReadF1Data(stintInterruptFile, 'stintinterrupt') %>%
								mutate(race = raceDF$race[ri])
			stintDF = join_on_overlap(stintDF, myStintInterrupt, c('race', 'driver', 'stint'))
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

### right, we're done here, hooray. But it's not in a package, and we've not really checked it on new data, just old

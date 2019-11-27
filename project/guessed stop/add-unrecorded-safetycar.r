## another problem in old world, it seems that in some cases, the safety car interruptions were not added to the interrupe file. let's see when this has happened and correct for it

source('model code/f1messystint/R/messy-stint-funct.R')
LoadAllData()
stintDF = BolsterStint(stintDF)

### the lap1/retired/safety car ones need no checking, they're interrupted
stintDF$interrupt[with(stintDF, which(isDryToDryStop & (lap1Stop | isSafetyCar | stintRetired | dryToWet)))] = TRUE

stintDF$loggedInterrupt = FALSE
for (ri in 1:nrace) {
	if (raceDF$isValidRace30[ri]) {
		stintInterruptFile = MakeRaceFile(raceDF$race[ri], 'pitstop-interrupt.csv')
		myStintInterrupt = ReadF1Data(stintInterruptFile, 'stintinterrupt') %>%
								mutate(race = raceDF$race[ri]) %>%
								rename(loggedInterrupt = interrupt)
		stintDF = join_on_overlap(stintDF, myStintInterrupt, c('race', 'driver', 'stint'))
		toAdd = with(stintDF, which(rr == ri & interrupt & !loggedInterrupt))
		if (length(toAdd) > 0) {
			myAugmentedStintInterrupt = bind_rows(ReadF1Data(stintInterruptFile, 'stintinterrupt'),
										stintDF[toAdd, c('driver', 'stint', 'interrupt')])
}}

#### no no no, totally wrong. we DON'T want to add safety cars to that
### pitstop-interrupt is a terrible name. it should be called 'checked-interrupted-stint'.
### we should remove trivial ones, such as safety cars and retirements that are in there at the moment, they're confusing
### stintDF$interrupted already records them, only need to add the ambiguous ones to the file

## so we're going to do two things here:
### 1 loop through and rename all the files
### 2 remove the trivial ones

stintDF$obviousInterrupt = with(stintDF, lap1Stop | isSafetyCar | stintRetired | dryToWet)

for (ri in 1:nrace) {
	if (raceDF$isValidRace30[ri]) {
		f1plot:::StintSummary(raceDF$race[ri])
		#stintInterruptFile = MakeRaceFile(raceDF$race[ri], 'pitstop-interrupt.csv')
		#checkedInterruptedStintFile = MakeRaceFile(raceDF$race[ri], 'checked-interrupted-stint.csv')

		#file.rename(from = stintInterruptFile, to = checkedInterruptedStintFile)
		
		stintInterruptFile = MakeRaceFile(raceDF$race[ri], 'pitstop-interrupt.csv')

		# then scan it in and remove trivial ones
		myCheckedInterruptedStint = ReadF1Data(stintInterruptFile, 'checkedInterruptedStint')
		
		removalCandidate = anti_join(myCheckedInterruptedStint, stintDF %>% filter(race == raceDF$race[ri] & toQuery == '*'), c('driver', 'stint'))
		
		## join in the information that we thinks makes them trival to remove
		removalCandidate = lazy_left_join(removalCandidate, stintDF %>% filter(race == raceDF$race[ri]), c('driver', 'stint'), c('lap1Stop', 'isSafetyCar', 'stintRetired', 'dryToWet', 'obviousInterrupt'))
		
		### these we can surely remove:
		removalSurely = removalCandidate %>% 
						filter(obviousInterrupt & interrupt)
		
		if (nrow(removalSurely) > 0) {
			message('These look obvious. Are you happy to have these all removed?')
			print(removalSurely)
			dum = askcond(F,T)
			
			if (dum == 'y') {
				myCheckedInterruptedStint = anti_join(myCheckedInterruptedStint, removalSurely, c('driver', 'stint'))
			}
			if (dum == 'n') {
				stop('ok, better look into that\n')
			}
		}
		
		removalQuery = removalCandidate %>%
						filter(obviousInterrupt & !interrupt)
		
		
		if (nrow(removalQuery) > 0) {
			message('These are less clear. Are you happy to have these all removed?')
			print(removalQuery)
			dum = askcond(F,T)
			
			if (dum == 'y') {
				myCheckedInterruptedStint = anti_join(myCheckedInterruptedStint, removalQuery, c('driver', 'stint'))
			}
			if (dum == 'n') {
				stop('ok, better look into that\n')
			}
		}
		
		checkedInterruptedStintFile = MakeRaceFile(raceDF$race[ri], 'checked-interrupted-stint.csv')
		write_csv(myCheckedInterruptedStint, path = checkedInterruptedStintFile)
	}
}

### yawn, obvious what we should be doing, let's just do it


for (ri in 1:nrace) {
	if (raceDF$isValidRace30[ri]) {
		stintInterruptFile = MakeRaceFile(raceDF$race[ri], 'pitstop-interrupt.csv')
		checkedInterruptedStintFile = MakeRaceFile(raceDF$race[ri], 'checked-interrupted-stint.csv')

		file.rename(from = stintInterruptFile, to = checkedInterruptedStintFile)
		
		# then scan it in and remove trivial ones
		myCheckedInterruptedStint = ReadF1Data(checkedInterruptedStintFile, 'checkedInterruptedStint')
		
		myCheckedInterruptedStint = lazy_left_join(myCheckedInterruptedStint, stintDF %>% filter(race == raceDF$race[ri]), c('driver', 'stint'), 'obviousInterrupt')

		myCheckedInterruptedStint = myCheckedInterruptedStint %>%
									filter(!obviousInterrupt) %>%
									select(-obviousInterrupt)
		
		write_csv(myCheckedInterruptedStint, path = checkedInterruptedStintFile)
	}
}

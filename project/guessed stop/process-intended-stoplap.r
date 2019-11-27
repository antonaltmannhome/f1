### want to know for each stint, what lap did the driver INTEND to stop

rrToModel = which(!raceDF$doneIntendedStopLap)

### this is a fiddly little one, so probably could be more efficient, but let's make sure it does what it's supposed to do
for (ri in rrToModel) {
	cat('About to process intended pit stop laps for', raceDF$race[ri],'\n')
	if (raceDF$isValidRace30[ri]) {
		alternativeStrategyFile = MakeRaceFile(raceDF$race[ri], 'alternative-strategy.csv')
		alternativeStrategyDF = ReadF1Data(alternativeStrategyFile, 'alternativeStrategy')
		currentRaceStintIndex = which(stintDF$rr == ri)
		for (si in currentRaceStintIndex) {
			myAlternativeStrategyDF = alternativeStrategyDF %>%
										filter(driver == stintDF$driver[si] &
												vantageStartLap <= stintDF$startLap[si] &
												vantageEndLap >= stintDF$startLap[si])
			sax = with(myAlternativeStrategyDF, which(inlap > stintDF$startLap[si]))
			if (length(sax)>0) {
				stintDF$intendedStopLap[si] = min(myAlternativeStrategyDF$inlap[sax])
			}
		}
		currentRaceNonNAStintIndex = with(stintDF, which(rr == ri & !is.na(intendedStopLap)))
		sqlLazyUpdate(stintDF[currentRaceNonNAStintIndex,], 'stint', c('race', 'driver', 'stint'), 'intendedStopLap')
	}
	raceDF$doneIntendedStopLap[ri] = TRUE
	sqlLazyUpdate(raceDF[ri,], 'race', 'race', 'doneIntendedStopLap')
}


SingleLapPossibleOvertaking = function(myRaceLapDF) {
	# this should ensure that the first listed driver wil be the one ranked higher at start of lap (which we need)
	myRaceLapDF = myRaceLapDF %>% arrange(startRank)
	combIndex = combn(nrow(myRaceLapDF), 2)

	goodColumn = c('driver', 'startTimeElapsed', 'mod30PredSec', 'endRank')
	driver1DF = myRaceLapDF[combIndex[1,], goodColumn] %>% mutate(inPair = 1:n())
	driver2DF = myRaceLapDF[combIndex[2,], goodColumn] %>% mutate(inPair = 1:n())
	combDF = left_join(driver1DF, driver2DF, 'inPair') %>% select(-inPair)
	combDF = combDF %>%
				mutate(timeElapsedDelta = startTimeElapsed.y - startTimeElapsed.x,
						predSecDelta = mod30PredSec.y - mod30PredSec.x,
						gotOvertaken = endRank.x > endRank.y) %>%
				filter(timeElapsedDelta + predSecDelta < 3) # don't need to retain combos that have no chance of yielding an overtake

	# clean up DF a little then return
	combDF = combDF %>%
				rename(driver1 = driver.x,
						driver2 = driver.y) %>%
				mutate(race = myRaceLapDF$race[1],
						lap = myRaceLapDF$lap[1]) %>%
				select(race, lap, driver1, driver2, timeElapsedDelta, predSecDelta, gotOvertaken)

	return(combDF)
}

ProcessPossibleOvertaking = function() {
	LoadAllData()
	lbl = f1validity:::MakeOvertakingIsGood(lbl)
	lbl = f1laptimelm:::MakePredSec(lbl, 30, adjustForCarProblem = TRUE)

	rrToModel = which(!raceDF$donePossibleOvertaking)

	for (ri in rrToModel) {

		if (raceDF$isValidRace30[ri]) {

			goodOTLap = lbl %>%
					filter(rr == ri & isGoodOvertaking) %>%
					group_by(lap) %>%
					summarise(sumGoodOT = n()) %>%
					filter(sumGoodOT >= 2)

			myLbl = semi_join(lbl %>%
								filter(rr == ri & isGoodOvertaking),
								goodOTLap,
								'lap')

			myPossibleOvertaking = myLbl %>%
							group_by(lap) %>%
							do(f1overtakingmodel:::SingleLapPossibleOvertaking(.))

			sqlInsert_usedf('possibleovertaking', myPossibleOvertaking)
		}

		raceDF$donePossibleOvertaking[ri] = TRUE
		sqlLazyUpdate(raceDF[ri,], 'race', 'race', 'donePossibleOvertaking')
		message('Have processed possible overtaking for ', raceDF$race[ri])
	}
}

myRaceDF = MakeTidyRaceDF('2018spain')

checklap = function(myLap) {

currentLeaderStartTelapse = myRaceDF %>%
					filter(lap == myLap) %>%
					select(driver, lap, starttelapse) %>%
					filter(starttelapse == min(starttelapse)) %>%
					pull(starttelapse)
					
# then we want all drivers who crossed the line around that time
subLapTimeDF = myRaceDF %>%
				filter(between(starttelapse,
								currentLeaderStartTelapse - 200,
								currentLeaderStartTelapse + 200)) %>%
				group_by(driver) %>%
				arrange(starttelapse) %>%
				filter(lap == lap[1]) %>%
				select(driver, starttelapse, endtelapse, lap, inlap, outlap)
					
### NB out of date comment, i think: NB this goes wrong e.g for laps 26 and 27 of spanish GP. stroll's 26th lap is never processed. but have spent too long looking at it, will come back to it later
### NB danger of double counting things, but htat's fine, we'll weed out those at the end	
				
	allPair = as_tibble(t(combn(subLapTimeDF$driver, 2)))
	allPair$pairNumber = 1:nrow(allPair)
vertAllPair = gather(allPair, inPair, driver, -pairNumber)  %>% arrange(pairNumber)
vertAllPair = left_join(vertAllPair,
						subLapTimeDF,
						'driver')
horizAllPair = vertAllPair %>%
				spread_multiple(key = inPair, driver, starttelapse, endtelapse, lap, inlap, outlap, sep = '')
pairColumnName = grep('inPair', names(horizAllPair))
for (j in pairColumnName) {
	names(horizAllPair)[j] = gsub('(inPairV)([0-9])(\\_)(.+$)', '\\4\\2', names(horizAllPair)[j])
}

### that's pretty much there
horizAllPair = horizAllPair %>%
				mutate(overtake1 = starttelapse1 > starttelapse2 &
									endtelapse1 < endtelapse2 &
									!inlap2 & !outlap2,
						overtake2 = starttelapse2 > starttelapse1 &
									endtelapse2 < endtelapse1 &
									!inlap1 & !outlap1,
						didlap1 = overtake1 & (lap1 > lap2),
						didlap2 = overtake2 & (lap2 > lap1))
#						unlap1 = overtake1 & (lap2 > lap1),
#						unlap2 = overtake2 & (lap1 > lap2))
# NB think we need the implied lap times to do unlapping

otDF = horizAllPair %>% filter(overtake1 | overtake2)

return(otDF)
}

### no, this is just a bad routine, doens't pick up e.g perez getting lapped in spain 2018

ProcessOvertaking = function(myRaceDF) {
	myOvertakingDF = myRaceDF %>%
						group_by(leadlap) %>%
						do(ProcessOvertaking.SingleLap(.$driver,
														.$starttelapse,
														.$endtelapse,
														.$sec))
}

## first draft at this bad routine:
ProcessOvertaking.SingleLap = function(driver, startTelapse, endTelapse, sec) {
	subLapTimeDF = tibble(driver = driver,
							startTelapse = startTelapse,
							endTelapse = endTelapse,
							sec = sec) %>%
					arrange(startTelapse)
	allPair = as_tibble(t(combn(subLapTimeDF$driver, 2)))
	allPair[,c('startTelapse1', 'endTelapse1')] =
		subLapTimeDF[match(allPair$V1, subLapTimeDF$driver), c('startTelapse', 'endTelapse')]
	allPair[,c('startTelapse2', 'endTelapse2')] =
		subLapTimeDF[match(allPair$V2, subLapTimeDF$driver), c('startTelapse', 'endTelapse')]
	allPair = allPair %>%
				mutate(startDelta = startTelapse1 - startTelapse2,
						endDelta = endTelapse1 - endTelapse2)
	subOvertakingDF = allPair %>%
					filter(sign(startDelta) != sign(endDelta))
	## then got a bit of a faff working out who overtook whom, what lap they were on, was there a pit stop involved etc
	return(subOvertakingDF)
}
		
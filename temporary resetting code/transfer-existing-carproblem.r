### really fiddly to check the new carproblem stuff is working. let's have an easy way of setting things up in a way they might be half way through a season

## here is the sql code to reset the carproblem table:

source('c:/research/f1/f1-startup.r')
LoadAllData()

sto.rddf = read.csv('c:/research/f1/temporary transfer data/rddf_carproblem.csv',as.is=T)
sto.carprobdf = read.csv('c:/research/f1/temporary transfer data/carprobdf.csv',as.is=T)

sto.rddf = sto.rddf %>%
			dplyr::rename(race = racename) %>%
			mutate(anyCarProblem = anycarproblem == 1,
					confidentCarProblem = confidentcarproblem == 1) %>%
			select(-c(anycarproblem, confidentcarproblem))
sto.carprobdf = sto.carprobdf %>%
					dplyr::rename(race = racename,
											startLap = startlap,
											endLap = endlap)

# two ways you can run this, you could only update part of them and leave the others to be tested by process-carproblem:
runMode = 'testCarProblem'
# runMode = 'fillAll'

if (runMode == 'testCarProblem') {
	numracetoreset = 3
	rrToSayAreDone = which(raceDF$rr < nrace - (numracetoreset - 1))
	rrToReset = which(raceDF$rr>=nrace - (numracetoreset - 1))
	rrToUndoConfidence = which(between(raceDF$rr, nrace - (2 * numracetoreset - 2), nrace - (numracetoreset - 1)))

	sto.carprobdf = sto.carprobdf %>% filter(race %in% raceDF$race &
												!race %in% raceDF$race[rrToReset])
	sto.carprobdf$startLap = pmax(sto.carprobdf$startLap, 1L)
												
	sto.rddf[which(sto.rddf$race %in% raceDF$race[rrToReset]),c('anyCarProblem','confidentCarProblem')]= FALSE

	# let's make some of the recent carproblems not confident too
	rddf$confidentCarProblem[which(rddf$rr %in% rrToUndoConfidence)] = FALSE
}

if (runMode == 'fillAll') {
		rrToSayAreDone = which(raceDF$race %in% unique(sto.rddf$race))
}

for (ri in rrToSayAreDone) {
	mycarprobdf = sto.carprobdf %>%
					filter(race==raceDF$race[ri])
	fileout = MakeRaceFile(myRace = raceDF$race[ri], myFile = 'carproblem.csv')
	write_csv(mycarprobdf, path = fileout)
}
		
raceDF$doneCarProblem[rrToSayAreDone] = TRUE
rddf[,c('anyCarProblem','confidentCarProblem')] = FALSE
sax = which(with(rddf, paste(race, driver)) %in% with(sto.rddf, paste(race, driver)))
mdum = match(with(rddf[sax,], paste(race, driver)), with(sto.rddf, paste(race, driver)))
rddf[sax,c('anyCarProblem','confidentCarProblem')] = sto.rddf[mdum,c('anyCarProblem','confidentCarProblem')]

sqlUpdate_multikey('racedriver',
					c('anyCarProblem','confidentCarProblem'),
					c('race','driver'),
					rddf[sax,c('anyCarProblem','confidentCarProblem')],
					rddf[sax,c('race','driver')])

sqlUpdate_multikey('race',
					'doneCarProblem',
					'race',
					raceDF[rrToSayAreDone,'doneCarProblem'],
					raceDF[rrToSayAreDone,'race'])


lblRrToModelIndex = with(lbl, which(rr %in% rrToSayAreDone))
lbl$isCarProblem[lblRrToModelIndex] = FALSE # 
for (ci in 1:nrow(sto.carprobdf)) {
	if (sto.carprobdf$race[ci] %in% raceDF$race[rrToSayAreDone]) {
		lbl = lbl %>%
				mutate_cond(driver == sto.carprobdf$driver[ci] &
							race == sto.carprobdf$race[ci] &
							between(lap, 
									sto.carprobdf$startLap[ci],
									sto.carprobdf$endLap[ci]),
							isCarProblem = TRUE)
	}
}
for (ri in rrToSayAreDone) {
	lblCurrentRrIndex = with(lbl, which(rr == ri))
	sqlUpdate_multikey('racedriverlap',
					'isCarProblem',
					c('race', 'driver', 'lap'),
					lbl[lblCurrentRrIndex, 'isCarProblem'],
					lbl[lblCurrentRrIndex, c('race', 'driver', 'lap')])
	message('Have overwritten isCarProblem for ', raceDF$race[ri])
}


################################ that is all you need i think #############################################

					
if (FALSE) {
sqlInsert_usedf('carproblem',
				sto.carprobdf)
}		
### NB latest thinking: get rid of carproblem table, just have lots of little files instead, only have raceDF$donecarproblem and lbl$carproblem on data base

for (ri in rrToSayAreDone) {
	mycarprobdf = sto.carprobdf %>%
					filter(racename==raceDF$racename[ri])
	fileout = MakeRaceFile(myracename = raceDF$racename[ri], myfilename = 'carproblem.csv')
	write_csv(mycarprobdf, path = fileout)
}

### final suggestion, if we're happy carproblem is now working, just transfer all data over AND fill in rddf, lbl and raceDF

### firstly write car problem from old world:
write.csv(file = 'c:/research/f1/temporary transfer data/rddf_carproblem.csv',
			rddf[,c('racename', 'driver', 'anycarproblem', 'confidentcarproblem')],
			row.names = FALSE)
write.csv(file = 'c:/research/f1/temporary transfer data/carprobdf.csv',
			carprobdf[,c('racename', 'driver', 'startlap', 'endlap', 'explanation')],
			row.names = FALSE)


source('c:/research/f1/f1-startup.r')
LoadAllData()

sto.rddf = read.csv('c:/research/f1/temporary transfer data/rddf_carproblem.csv',as.is=T)
sto.carprobdf = read.csv('c:/research/f1/temporary transfer data/carprobdf.csv',as.is=T)

sto.rddf$anycarproblem = sto.rddf$anycarproblem == 1
sto.rddf$confidentcarproblem = sto.rddf$confidentcarproblem == 1

rrToReset = 1:nrace

for (ri in rrToReset) {
	mycarprobdf = sto.carprobdf %>%
					filter(racename==raceDF$race[ri]) %>%
					dplyr::rename(race = racename,
									startLap = startlap,
									endLap = endlap)
	fileout = MakeRaceFile(myRace = raceDF$race[ri], myFile = 'carproblem.csv')
	write_csv(mycarprobdf, path = fileout)
}

sqlUpdate_multikey('racedriver',
					c('anyCarProblem', 'confidentCarProblem'),
					c('race', 'driver'),
					rddf[,c('anyCarProblem', 'confidentCarProblem')],
					rddf[,c('race', 'driver')])

lblRrToModelIndex = with(lbl, which(rr %in% rrToReset))
lbl$isCarProblem[lblRrToModelIndex] = FALSE # 
for (ci in 1:nrow(sto.carprobdf)) {
	lbl = lbl %>%
			mutate_cond(driver == sto.carprobdf$driver[ci] &
						race == sto.carprobdf$racename[ci] &
						between(lap, 
								sto.carprobdf$startlap[ci],
								sto.carprobdf$endlap[ci]),
						isCarProblem = TRUE)
}
for (ri in rrToReset) {
	lblCurrentRrIndex = with(lbl, which(rr == ri))
	sqlUpdate_multikey('racedriverlap',
					'isCarProblem',
					c('race', 'driver', 'lap'),
					lbl[lblCurrentRrIndex, 'isCarProblem'],
					lbl[lblCurrentRrIndex, c('race', 'driver', 'lap')])
	message('Have overwritten isCarProblem for ', raceDF$race[ri])
}


source('c:/research/f1/f1-startup.r')
LoadAllData()

## in old world:
if (FALSE) {
	write.csv(file = 'c:/research/f1/temporary transfer data/qualifying_outlier.csv', qdf[,c('racename','qs','driver','isoutlier')], row.names = FALSE)
}

sto.qualifyingoutlierdf = read.csv('c:/research/f1/temporary transfer data/qualifying_outlier.csv',as.is=T)

sto.qualifyingoutlierdf = sto.qualifyingoutlierdf %>%
						dplyr::rename(race = racename,
										session = qs) %>%
						mutate(isOutlier = isoutlier == 1)

raceDF$doneQualifyingOutlier = raceDF$race %in% sto.qualifyingoutlierdf$race
qdf = lazy_left_join(qdf,
					sto.qualifyingoutlierdf,
					c('race', 'driver', 'session'),
					'isOutlier')

if (FALSE) {
sqlUpdate_multikey('race',
					'doneQualifyingOutlier',
					'race',
					raceDF[,'doneQualifyingOutlier'],
					raceDF[,'race'])

sqlUpdate_multikey('qualifying',
					'isOutlier',
					c('race','session','driver'),
					qdf[,'isOutlier'],
					qdf[,c('race','session','driver')])
}
sqlLazyUpdate(raceDF, 'race', 'race', 'doneQualifyingOutlier')
haveDoneRaceIndex = which(qdf$race %in% raceDF$race[raceDF$doneQualifyingOutlier])
sqlLazyUpdate(qdf[haveDoneRaceIndex,], 'qualifying', c('race','session','driver'), 'isOutlier')

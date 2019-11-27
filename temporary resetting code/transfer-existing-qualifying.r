
source('c:/research/f1/f1-startup.r')
LoadAllData()

## in old world:
if (FALSE) {
	write.csv(file = 'c:/research/f1/temporary transfer data/qdf-qualifying.csv', qdf[,c('racename','qs','driver','predqt','qualprob','qualwgt')], row.names = FALSE)
	write.csv(file = 'c:/research/f1/temporary transfer data/race-qualifying.csv', racedb[,c('racename','modqualqualintercept')], row.names = FALSE)
	write.csv(file = 'c:/research/f1/temporary transfer data/rddf-qualifying.csv', rddf[,c('racename','driver','qualrawdcoef','qualprednvalid')], row.names = FALSE)
}

sto.qdfqualifyingdf = read.csv('c:/research/f1/temporary transfer data/qdf-qualifying.csv',as.is=T)
sto.racequalifyingdf = read.csv('c:/research/f1/temporary transfer data/race-qualifying.csv',as.is=T)
sto.rddfqualifyingdf = read.csv('c:/research/f1/temporary transfer data/rddf-qualifying.csv',as.is=T)

sto.qdfqualifyingdf = sto.qdfqualifyingdf %>%
						dplyr::rename(race = racename,
										session = qs,
										predSec = predqt,
										qualProb = qualprob,
										qualWgt = qualwgt)

sto.racequalifyingdf = sto.racequalifyingdf %>%
						dplyr::rename(race = racename,
										modQualQualIntercept = modqualqualintercept)
sto.rddfqualifyingdf = sto.rddfqualifyingdf %>%
						dplyr::rename(race = racename,
										modQualRawDCoef = qualrawdcoef,
										modQualPredNValid = qualprednvalid)

raceDF$doneQualifyingModel = raceDF$race %in% sto.qdfqualifyingdf$race
qdf = lazy_left_join(qdf,
					sto.qdfqualifyingdf,
					c('race', 'driver', 'session'),
					c('predSec', 'qualProb', 'qualWgt'))
raceDF = lazy_left_join(raceDF, sto.racequalifyingdf, 'race', 'modQualQualIntercept')
rddf = lazy_left_join(rddf, sto.rddfqualifyingdf, c('race', 'driver'), c('modQualPredNValid', 'modQualRawDCoef'))

sqlLazyUpdate(raceDF, 'race', 'race', 'doneQualifyingModel')
haveDoneRaceIndex = which(qdf$race %in% raceDF$race[raceDF$doneQualifyingModel])
sqlLazyUpdate(qdf[haveDoneRaceIndex,], 'qualifying', c('race','session','driver'), c('predSec', 'qualProb', 'qualWgt'))
qualyHappenedIndex = with(raceDF, which(!is.na(modQualQualIntercept)))
sqlLazyUpdate(raceDF[qualyHappenedIndex,], 'race', 'race', 'modQualQualIntercept')
driverDidQualyIndex = with(rddf, which(!is.na(modQualRawDCoef)))
sqlLazyUpdate(rddf[driverDidQualyIndex,], 'racedriver', c('race','driver'), 'modQualRawDCoef')
sqlLazyUpdate(rddf, 'racedriver', c('race','driver'), 'modQualPredNValid')

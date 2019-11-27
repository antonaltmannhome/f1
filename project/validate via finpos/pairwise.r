# we want to construct all pairs within a race

LoadAllData()

dum = f1smoothing:::GetSmooth(method = 'downweight',
							qrtofit = 'qr',
							qrtopredict = 'r',
							fwbw = 'fwbw',
							modelchoice = 30,
							usecurrentrace = FALSE,
							usestretch = TRUE,
							myRace = raceDF$race[nrace])

rddf[,c('smval', 'numobval')] = cbind(dum$smval, dum$numobval)

myrddf =  rddf %>%
			filter(race == '2018brazil' &
					!is.na(officialFinishingPosition))
finpospair = combn(myrddf$officialFinishingPosition,2)
smvalpair = combn(myrddf$smval,2)
myPairDF = tibble(isWinner = finpospair[1,] < finpospair[2,],
					paceDelta = smvalpair[1,] - smvalpair[2,])

MakePairDF = function(myOFP, mySmVal) {
	finpospair = combn(myOFP, 2)
	smvalpair = combn(mySmVal, 2)
	myPairDF = tibble(isWinner = finpospair[1,] < finpospair[2,],
					paceDelta = smvalpair[1,] - smvalpair[2,])
	return(myPairDF)
}

pairDF = rddf %>%
			filter(!is.na(smval) & !is.na(officialFinishingPosition)) %>%
			group_by(race) %>%
			do(MakePairDF(.$officialFinishingPosition, .$smval))

# all looks pretty smooth, let's fine logit of best fit
# by inspection firstly:
binPoint = calibplot(pairDF$paceDelta, pairDF$isWinner, retval = TRUE)
# invlogit(-2*x) looks close, let's verify properly
LikFunct = function(theta) {
	pairDF$probWin = invlogit(-exp(theta)*pairDF$paceDelta)
	pairDF$logLik = with(pairDF, log(dbinom(isWinner, 1, probWin)))
	meanLogLik = mean(pairDF$logLik)
	return(-meanLogLik)
}

maxInfo = optimise(LikFunct, interval = c(log(0.5), log(5)))
# 2.27 is the number that comes out
paceNormCoef = 2.27

# right, now we can get numbers for every race i think
# try on one race, then generalise
ProcessNumWin = function(myRace) {
	myrddf =  rddf %>%
				filter(race == myRace &
				!is.na(officialFinishingPosition) &
						!is.na(smval))
	allcombodf = expand.grid(driv1 = myrddf$driver,
							driv2 = myrddf$driver,
							stringsAsFactors = FALSE) %>%
					filter(driv1 != driv2) %>%
					lazy_left_join(myrddf %>%
									rename(driv1 = driver,
											smval1 = smval),
									'driv1',
									'smval1') %>%
					lazy_left_join(myrddf %>%
									rename(driv2 = driver,
											smval2 = smval),
									'driv2',
									'smval2')

	allcombodf$winProb = with(allcombodf, invlogit(-paceNormCoef * (smval1 - smval2)))

	## right, now who wins how much?
	numWin = allcombodf %>%
				group_by(driv1) %>%
				summarise(numWin = sum(winProb)) %>%
				arrange(-numWin) %>%
				mutate(expectedFinPos = n() - numWin) %>%
				mutate(race = myRace) %>%
				rename(driver = driv1)

	return(numWin)
}

dum = purrr::map_df(raceDF$race, ProcessNumWin)
dum = lazy_left_join(dum, rddf, c('race', 'driver'), 'officialFinishingPosition')
dum$sqDiff = with(dum, (officialFinishingPosition - expectedFinPos)^2)

calibplot(dum$expectedFinPos, dum$officialFinishingPosition)

### YES

# let's get some mean squared differences for a few obvious things:

# r/r/30/stretch: 8.425
# qr/r/30/stretch: 8.337 (but different # of valid obs)
### put this into a function, it's a nightmare to do this way

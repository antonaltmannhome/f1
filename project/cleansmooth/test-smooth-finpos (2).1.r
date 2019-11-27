
LoadAllData()
suppressWarnings(library(data.table))
source('project/cleansmooth/smoothing-plus-finpos.r')
source('project/cleansmooth/smoothing-admin-funct.r')
source('project/validate via finpos/messy-race-funct.r')

# NB there is some dodgy logic in here, e.g. using the smooth to predict messy qualifying, even though it's the smooth we're doing it to help
rddf = MakeCleanRace(rddf, 4)
dum = GetSmooth(qrToFit = 'r', qrToPredict = 'rfinpos', useStretch = TRUE,
				fwbw = 'fwbw', modelChoice = 4, customSmoothInfo = NULL)
rddf  = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'expectedFinPos') %>%
		rename(expectedFinPos4 = expectedFinPos)

rddf = MakeCleanRace(rddf, 30)
dum = GetSmooth(qrToFit = 'r', qrToPredict = 'rfinpos', useStretch = TRUE,
				fwbw = 'fwbw', modelChoice = 30, customSmoothInfo = NULL)
rddf  = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'expectedFinPos') %>%
		rename(expectedFinPos30 = expectedFinPos)

# now compare them
rddf = rddf %>%
		mutate(deltaWgt = 1 - messiness,
				sqDiff4 = deltaWgt * (officialFinishingPosition - expectedFinPos4)^2,
				sqDiff30 = deltaWgt * (officialFinishingPosition - expectedFinPos30)^2)

aaConfInt(with(rddf, (sqDiff4 - sqDiff30)[!is.na(sqDiff4+sqDiff30)]))
### model 4 significantly better almost, ouch
		
RunModel('2018abudhabi')

# shame that model 4 does better than model 30 and that useStretch actually makes things worse

### that is the only good bit of this code, ignore everything else and jump straight to the code proper
### next steps:
## finish off the code - maybe alleviate some of the horrible names e.g pairedDriverDT and pairedDriverDF DONE
## we've glossed over which races are valid e.g need to eliminate messy races DONE
### remove dodgy vicious circle logic within MAkeCleanRace
## want to merge numobwgt into the optimisation
## need to do tedious admin work of adding qrtopredict = 'rfinpos' into Combo generation. but in the longer term, will ditch 'r' and maybe even 'q' in favour of qfinpos DONE
## still got to do team mate thing POSSIBLY NOT ESSENTIAL BEFORE START OF SEASON
## supplying custom dcoefs DONE

	modelinfo=list(qrtofit=qrtofit,
					qrtopredict=qrtopredict,
					modelchoice=modelchoice,
					usestretch=usestretch,
					fwbw=fwbw)
	modelparamname=GetModelParamName()

	cat('About to process the following combination:\n')
	DebugDisplay(modelinfo)
	message('myRace = "', myRace, '"')
	
	### going to want to know this constantly, so define these variables just for brevity
	modelinfo$useQual=modelinfo$qrtofit %in% c('q','qr')
	modelinfo$useRace=modelinfo$qrtofit %in% c('r','qr')

	CheckValidModelInfo(modelinfo)
	
	# this creates racePredNValid, raceDCoef, qualDCoef, stretch
	rddf = MakeGeneralColumnName(rddf, modelinfo)
	
	# this calculates when a driver/team/season has done any other races, whether they have a valid estimate today etc
	rddf = DetectDriverHasOtherData(rddf, modelinfo)
	
	### next, we'll use the values derived from timeseries analysis to determine how much weight to put on points
	rddf = MakeFitWeight(rddf, modelinfo)
	
	### now build up our lists saying how we're going to weight qualy/race when optimising
	
	rddf = MakeNumobForPredict(rddf, modelinfo$qrtopredict)
	
	### now we need to define what we're going to be smoothing - it depends on whther we're doing stretching, or mixing qs with rs
	### if we're fitting qr, then ought to adjline correct the coefs of whatever we're not predicting
	
	rddf = MakeQualRaceRescaleColumn(rddf, modelinfo)
	rddf = PreSmoothRescaleDCoef(rddf, modelinfo)
	
	OOSRddf = MakeOOSRddf(modelinfo, rddf, myRace)
	
	dum = MakeDTFromTibble(rddf, OOSRddf, modelinfo)
	raceDriverDT = dum$raceDriverDT
	OOSRaceDriverDT = dum$OOSRaceDriverDT
	QRRaceDriverDT = dum$QRRaceDriverDT

	if (modelinfo$qrtopredict %in% c('q', 'r')) {
		pairedDriverDT = NULL
		numFinisherByRace = NULL
	}
	if (modelinfo$qrtopredict == 'rfinpos') {
		pairedDriverDT = MakePairedDriverDT(rddf)
		numFinisherByRace = raceDriverDT[
							!is.na(officialFinishingPosition) & hasOtherData,
									.(numFinisher = .N),
									'race']
	}
	
	# stick em all in a list
	DTList = list(raceDriverDT = raceDriverDT,
					OOSRaceDriverDT = OOSRaceDriverDT,
					QRRaceDriverDT = QRRaceDriverDT,
					pairedDriverDT = pairedDriverDT,
					numFinisherByRace = numFinisherByRace)
	
#	theta = InitialiseAndProcessCoef(theta = NULL, modelinfo)$theta
#maxtheta = RetrieveMaxTheta(myRace, modelinfo)

	maxtheta = OptimiseParam(modelinfo, DTList, myRace)


	dum = InitialiseAndProcessCoef(theta = NULL, modelinfo)
	mycoef = dum$mycoef
	
	raceDriverDT = CalculateSmoothRaceDriverPlusEFinPos(mycoef, modelinfo, raceDriverDT, OOSRaceDriverDT, QRRaceDriverDT, pairedDriverDT, numFinisherByRace)

	# now we can start
	# we need to filter out messy race/driver combos, will do that later, let's just sort out the finpos estimating for now
	

MakePairDF = function(myOFP, mySmoothDCoef) {
	finposPair = combn(myOFP, 2)
	smoothDCoefPair = combn(mySmoothDCoef, 2)
	myPairDF = tibble(isWinner = finposPair[1,] < finposPair[2,],
					paceDelta = smoothDCoefPair[1,] - smoothDCoefPair[2,])
	return(myPairDF)
}

raceDriverDF = as_data_frame(raceDriverDT)
pairDF = raceDriverDF %>%
			filter(!is.na(smoothDCoef) & !is.na(officialFinishingPosition)) %>%
			group_by(race) %>%
			do(MakePairDF(.$officialFinishingPosition, .$smoothDCoef))

# invlogit(-2*x) looks close, let's verify properly
LikFunct = function(theta) {
	pairDF$probWin = invlogit(-exp(theta)*pairDF$paceDelta)
	pairDF$logLik = with(pairDF, log(dbinom(isWinner, 1, probWin)))
	meanLogLik = mean(pairDF$logLik)
	return(-meanLogLik)
}

maxInfo = optimise(LikFunct, interval = c(log(0.5), log(5)))

binPoint = calibplot(pairDF$paceDelta, pairDF$isWinner, retval = TRUE, addabline = FALSE)
curve(invlogit(-exp(maxInfo$min) * x), add = TRUE, col = 'red')
paceNormCoef = exp(maxInfo$min)


# right, now we can get numbers for every race i think
# try on one race, then generalise
ProcessNumWin = function(myRace) {
	myrddf =  raceDriverDF %>%
				filter(race == myRace &
				!is.na(officialFinishingPosition) &
						!is.na(smoothDCoef))
	allcombodf = expand.grid(driv1 = myrddf$driver,
							driv2 = myrddf$driver,
							stringsAsFactors = FALSE) %>%
					filter(driv1 != driv2) %>%
					lazy_left_join(myrddf %>%
									rename(driv1 = driver,
											smoothDCoef1 = smoothDCoef),
									'driv1',
									'smoothDCoef1') %>%
					lazy_left_join(myrddf %>%
									rename(driv2 = driver,
											smoothDCoef2 = smoothDCoef),
									'driv2',
									'smoothDCoef2')

	allcombodf$winProb = with(allcombodf, invlogit(-paceNormCoef * (smoothDCoef1 - smoothDCoef2)))

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

rddfPlusEFP = purrr::map_df(raceDF$race, ProcessNumWin)
rddfPlusEFP = lazy_left_join(rddfPlusEFP, rddf, c('race', 'driver'), 'officialFinishingPosition')
rddfPlusEFP$sqDiff = with(rddfPlusEFP, (officialFinishingPosition - expectedFinPos)^2)

calibplot(rddfPlusEFP$expectedFinPos, rddfPlusEFP$officialFinishingPosition)

### ok so that's all correct but very slow, let's DT its ass

# should be able to do this outside of the optimisation loop:
transposeCombn = function(xx, nn) {
	myDF = as.data.frame(t(combn(xx, nn)))
	names(myDF) = c('driver1', 'driver2')
	return(myDF)
}
combnDF = rddf %>%
			filter(!is.na(officialFinishingPosition) & hasOtherData) %>%
			group_by(race) %>%
			do(transposeCombn(.$driver, 2))
			
combnDT = data.table(combnDF)

combnDT = setnames(raceDriverDT[,c('race', 'driver', 'officialFinishingPosition')], 'driver', 'driver1')[
				combnDT, on = c('race', 'driver1')]
setnames(combnDT, 'officialFinishingPosition', 'finPos1')

combnDT = setnames(raceDriverDT[,c('race', 'driver', 'officialFinishingPosition')], 'driver', 'driver2')[
				combnDT, on = c('race', 'driver2')]
setnames(combnDT, 'officialFinishingPosition', 'finPos2')

combnDT$isWinner1 = with(combnDT, finpos1 < finpos2)
numFinisherByRace = raceDriverDT[!is.na(officialFinishingPosition) & hasOtherData,
									.(numFinisher = .N),
									'race']
### then after you've smoothed

CalculateExpectedFinPos = function(raceDriverDT) {
	combnPlusSmoothDT = setnames(raceDriverDT[,c('race', 'driver', 'smoothDCoef')], 'driver', 'driver1')[
					combnDT, on = c('race', 'driver1')]
	setnames(combnPlusSmoothDT, 'smoothDCoef', 'smoothDCoef1')

	combnPlusSmoothDT = setnames(raceDriverDT[,c('race', 'driver', 'smoothDCoef')], 'driver', 'driver2')[
					combnPlusSmoothDT, on = c('race', 'driver2')]
	setnames(combnPlusSmoothDT, 'smoothDCoef', 'smoothDCoef2')

	combnPlusSmoothDT$smoothDCoefDelta = with(combnPlusSmoothDT, smoothDCoef1 - smoothDCoef2)

	combnPlusSmoothDT$driver1WinProb = with(combnPlusSmoothDT, invlogit(-paceNormCoef * smoothDCoefDelta))

	sumByRaceDriver1 = combnPlusSmoothDT[,.(sumDriverWin1 = sum(driver1WinProb)), c('race', 'driver1')]
	setnames(sumByRaceDriver1, 'driver1', 'driver')
	sumByRaceDriver2 = combnPlusSmoothDT[,.(sumDriverWin2 = sum(1 - driver1WinProb)), c('race', 'driver2')]
	setnames(sumByRaceDriver2, 'driver2', 'driver')

	sumByRaceDriver = data.table:::merge.data.table(sumByRaceDriver1,
													sumByRaceDriver2,
													on = c('race', 'driver'),
													all = TRUE)
	sumByRaceDriver[is.na(sumDriverWin1),sumDriverWin1 := 0]
	sumByRaceDriver[is.na(sumDriverWin2),sumDriverWin2 := 0]
	sumByRaceDriver = numFinisherByRace[sumByRaceDriver, on = 'race']

	sumByRaceDriver$expectedFinPos = with(sumByRaceDriver, numFinisher - (sumDriverWin1 + sumDriverWin2))

	return(sumByRaceDriver)
}
	
rddfPlusEFP = lazy_left_join(rddfPlusEFP,
							sumByRaceDriver %>% rename(DTEFP = expectedFinPos),
							c('race', 'driver'),
							'DTEFP')

with(rddfPlusEFP, plot(expectedFinPos, DTEFP))

### woohoo it all agrees

# quick time comparison:

date()
for (j in 1:10) {
rddfPlusEFP = purrr::map_df(raceDF$race, ProcessNumWin)
}
date()
# 1.4 seconds for 1 calculation

date()
for (j in 1:500) {
	dum = CalculateExpectedFinPos(raceDriverDT)
}
date()
# 0.04 seconds. 35  times faster

# got to integrate it in somehowthough, that requires a little more thought


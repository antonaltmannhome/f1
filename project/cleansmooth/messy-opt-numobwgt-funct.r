### let's try to find out what the best weight curve to use is for smoothing. Let's divide up the numbers into bins to firm up what shape we want


MakeOOSRddf = function(smoothParam, rddf, myRace) {
	myrr = with(raceDF, rr[race == myRace])
	### now we do the smoothing/timeseries for all races
	### let's have theta in  a nice form:
		
	myList = NULL
		
	for (ri in 1:myrr) {
		
		rddf = FilterRddfByTime(ri, rddf, smoothParam)
		CheckSmoothValidity(rddf, smoothParam)
		if (smoothParam$qrToFit == 'q') anyDataToSmooth = any(rddf$isQualFitValid)
		if (smoothParam$qrToFit == 'r') anyDataToSmooth = any(rddf$isRaceFitValid)
		if (smoothParam$qrToFit == 'qr') {
			anyDataToSmooth = with(rddf, any(isQualFitValid | isRaceFitValid))
		}
		
		if (anyDataToSmooth) {
			# get this race's version of rddf
			raceDF$daynumDelta = abs(raceDF$daynum[ri] - raceDF$daynum)
			if (smoothParam$useQual) {
				# this might look like it would be suitable for dplyr, but don't be tempted, it's too slow
				isQualFitValidIndex = with(rddf, isQualFitValid)
				qualRddf = rddf[isQualFitValidIndex,]
				qualRddf$daynumDelta = raceDF$daynumDelta[match(qualRddf$rr, raceDF$rr)]
			}
			if (smoothParam$useRace) {
				isRaceFitValidIndex = with(rddf, isRaceFitValid)
				raceRddf = rddf[isRaceFitValidIndex,]
				raceRddf$daynumDelta = raceDF$daynumDelta[match(raceRddf$rr, raceDF$rr)]
			}
			# we want to smooth quali, or race, or both together, which is why this is all a bit ugly
			if (smoothParam$useQual) {
				qualOOSRddf = qualRddf %>%
								select(rr, driver, team, daynumDelta, modQualPredNValid) %>%
								mutate(targetRr = ri,
										predNValid = modQualPredNValid,
										qualRace = 'qual')
			}
			if (smoothParam$useRace) {
				raceOOSRddf = raceRddf %>%
								select(rr, driver, team, daynumDelta, racePredNValid) %>%
								mutate(targetRr = ri,
										predNValid = racePredNValid,
										qualRace = 'race')
			}
			if (smoothParam$qrToFit == 'q') {
				myOOSRddf = qualOOSRddf
			}
			if (smoothParam$qrToFit == 'r') {
				myOOSRddf = raceOOSRddf
			}
			if (smoothParam$qrToFit == 'qr') {
				myOOSRddf = rbind(qualOOSRddf,
									raceOOSRddf)
			}
		myList[[ri]] = myOOSRddf
		}
	}
	
	OOSRddf = do.call(rbind, myList)
	OOSRddf = as.data.frame(OOSRddf)
	
	return(OOSRddf)
}

AugmentOOSRaceDriverDTWithModel = function(mycoef, smoothParam, OOSRaceDriverDT, QRRaceDriverDT) {

	OOSRaceDriverDT = QRRaceDriverDT[OOSRaceDriverDT, on = c('OOSRr', 'driver', 'qualRace')]
	if (FALSE) {
	OOSRaceDriverDT = data.table:::merge.data.table(OOSRaceDriverDT,
													QRRaceDriverDT,
													by = c('OOSRr', 'driver', 'qualRace'),
													all.x = TRUE)
	}
	OOSRaceDriverDT$tdwCoef = 0
	if (smoothParam$useQual) {
		OOSRaceDriverDT[qualRace == 'qual',tdwCoef := mycoef$qualTdwCoef]
	}
	if (smoothParam$useRace) {
		OOSRaceDriverDT[qualRace == 'race',tdwCoef := mycoef$raceTdwCoef]
	}
	
	OOSRaceDriverDT$timeWgt = with(OOSRaceDriverDT, exp(-tdwCoef*daynumDelta))
	OOSRaceDriverDT$numobTimeWgt = with(OOSRaceDriverDT, timeWgt * numobwgtvec)

	if (smoothParam$qrToFit %in% c('q', 'r')) {
		OOSRaceDriverDT$wgt = OOSRaceDriverDT$numobTimeWgt
	}
	if (smoothParam$qrToFit == 'qr') {
		OOSRaceDriverDT$qualProportion = -99.0
		OOSRaceDriverDT[qualRace == 'qual', qualProportion := mycoef$qualProportion]
		OOSRaceDriverDT[qualRace == 'race', qualProportion := (1 - mycoef$qualProportion)]
		OOSRaceDriverDT$wgt = with(OOSRaceDriverDT, qualProportion * numobTimeWgt)
	}

	return(OOSRaceDriverDT)
}

# old version of function
MakeFitWeight = function(rddf, smoothParam) {
	
	rddf$qualNumobWgtVec=rddf$raceNumobWgtVec=rep(NA,dim(rddf)[1])
	
	if (smoothParam$useQual) {
		rddf$qualNumobWgtVec[which(rddf$modQualPredNValid<=1)]=0.8
		rddf$qualNumobWgtVec[which(rddf$modQualPredNValid>1)]=1
	}
	if (smoothParam$useRace) {
		rddf$raceNumobWgtVec[which(rddf$racePredNValid<=30)]=0.5 + 0.5*rddf$racePredNValid[which(rddf$racePredNValid<=30)]/30
		rddf$raceNumobWgtVec[which(rddf$racePredNValid>30)]=1
	}

	return(rddf)
}

# this is what we used while developping it:
InitialiseAndProcessCoef=function(theta = NULL, smoothParam, display=F) {
	# runmode can be 'lik' or 'display'
	mycoef=NULL
	initialiseTheta = is.null(theta)
	count=1
	if (smoothParam$qrToFit %in% c('q','qr')) {
		if (initialiseTheta) {
			theta = c(theta, log(0.01))
		}
		mycoef$qualTdwCoef=exp(theta[count])
		count=count+1
	}
	if (smoothParam$qrToFit %in% c('r','qr')) {
		if (initialiseTheta) {
			theta = c(theta, log(0.01))
		}
		mycoef$raceTdwCoef=exp(theta[count])
		count=count+1
	}
	if (smoothParam$qrToFit=='qr') {
		if (initialiseTheta) {
			theta = c(theta, logit(0.5))
		}
		mycoef$qualProportion=invlogit(theta[count])
		count=count+1
	}
	if (smoothParam$qrToPredict == 'rfinpos') {
		if (initialiseTheta) {
			theta = c(theta, log(2))
		}
		mycoef$paceToPairedWinCoef = exp(theta[count])
		count = count + 1
	}
	# then we need to bolster with the bin numob coefs
	if (smoothParam$useQual) {
		if (initialiseTheta) {
			qualBinTheta = rep(0, length(qualBin) - 2)
			theta = c(theta, qualBinTheta)
		}
		mycoef$qualBinnedNumobWgtCoef = c(exp(theta[count:(count + length(qualBin) - 3)]), 1)
		count = count + length(qualBin) - 2
	}
	if (smoothParam$useRace) {
		if (initialiseTheta) {
			raceBinTheta = rep(0, length(raceBin) - 2)
			theta = c(theta, raceBinTheta)
		}
		mycoef$raceBinnedNumobWgtCoef = c(exp(theta[count:(count + length(raceBin) - 3)]), 1)
		count = count + length(raceBin) - 2
	}

	if (display) {
		cat(paste(names(mycoef),mycoef,sep=': '),sep='\n')
	}
	return(list(theta = theta, mycoef = mycoef))
}

.F1Startup()
LoadAllData()
 source('project/cleansmooth/smoothing-plus-finpos-opt-numobwgt.r')
 source('project/cleansmooth/smoothing-admin-funct.r')
 source('project/validate via finpos/messy-race-funct.r')

# NB there is some dodgy logic in here, e.g. using the smooth to predict messy qualifying, even though it's the smooth we're doing it to help

rddf = lazy_left_join(rddf, raceDF, 'race', 'circuit')

rddf = MakeCleanRace(rddf, 30)


qualBin = c(-1, 1, 2, 3)
raceBin = c(-1, 5, 10, 20, 30, 40, 100)

qrToFit = 'q'
qrToPredict = 'q'
fwbw = 'fwbw'
useStretch = TRUE
modelChoice = 30
customSmoothInfo = NULL
resetMaxTheta = FALSE
myRace = raceDF$race[nrace]
smoothDCoefName = NULL
smoothWgtName = NULL
expectedFinPosName = NULL

### paste it all in until optimise param, and then...

#mycoef = InitialiseAndProcessCoef(theta = NULL, smoothParam)$mycoef

# when predicting 'r', it optimises to this:
$qualBinnedNumobWgtCoef
[1] 0.4355762 0.6649476 1.0000000
$raceBinnedNumobWgtCoef
[1] 0.3150688 0.8917980 1.0980021 1.4263533 1.8222786 1.0000000
# very strange, that final coef. why would it want less weight on the final box, the one with the most numbesr in it? Something to do with drivers with lots of laps being slwoer than expected due to being not under pressure?
# mean finishing position in this category is 6.61, while average with <=40 laps is 10.42. mean win prob is 0.17, mean with <=40 laps is 0.0244. could well be something in that
# think we'll ignore that when setting the curve though, it's something we'd hopefully fix properly via the simulations

## meanwhile if you try to optimise rfinpos:

$qualBinnedNumobWgtCoef
[1] 0.3579227 0.6405442 1.0000000

$raceBinnedNumobWgtCoef
[1] 0.5766808 0.5762940 1.0958157 0.9439724 1.1157924 1.0000000

### don't really have the problem with high prednvalid,although the non-smooth curve is a bit disappointing

# so it looks like a good curve is one that is similar to the one already in smoothing, along the lines of:

raceNumobTheta = c(-1, log(0.1))
numobIntercept = invlogit(raceNumobTheta[1])
curve(numobIntercept + (1 - numobIntercept) * (1 - exp(-exp(raceNumobTheta[2]) * x)), from = 0, to = 50, ylim = c(0,1))

### ok so it's looking sensible now. let's try running the bastard properly


.F1Startup()
LoadAllData()
 source('project/cleansmooth/smoothing-plus-finpos-opt-numobwgt.r')
 source('project/cleansmooth/smoothing-admin-funct.r')
 source('project/validate via finpos/messy-race-funct.r')

# NB there is some dodgy logic in here, e.g. using the smooth to predict messy qualifying, even though it's the smooth we're doing it to help

rddf = lazy_left_join(rddf, raceDF, 'race', 'circuit')

rddf = MakeCleanRace(rddf, 30)

allCombo = GenerateCombo(filterList = list(modelChoice = c('qual', 30)))

for (mi in 1:nrow(allCombo)) {
	dum = GetSmooth(qrToFit = allCombo$qrToFit[mi],
					qrToPredict = allCombo$qrToPredict[mi],
					modelChoice = allCombo$modelChoice[mi],
					useStretch = allCombo$useStretch[mi],
					fwbw = allCombo$fwbw[mi])
}

RunModel('2018abudhabi', filterList = list(modelChoice = c('qual', 30)))

# visibility could be better. would be nice to know what weight has been placed on each race at each time point, so would need OOSRaceDriverDT to be returned for that. which we can do

qrToFit = 'qr'
qrToPredict = 'rfinpos'
fwbw = 'fwbw'
useStretch = FALSE
modelChoice = 30
customSmoothInfo = NULL
resetMaxTheta = FALSE
myRace = raceDF$race[nrace]
smoothDCoefName = NULL
smoothWgtName = NULL
expectedFinPosName = NULL
	
dum = GetSmooth(qrToFit = qrToFit, qrToPredict = qrToPredict, fwbw = fwbw, useStretch = useStretch,
			modelChoice = modelChoice)
			
OOSRaceDriverDT = dum$OOSRaceDriverDT

OOSRaceDriverDT[driver=='eocon' & aggregatedRace == '2018abudhabi',,] %>%
	select(driver, OOSRr, qualRace, predNValidBin, numobWgt, wgt) %>%
	spread_multiple(keyCol = qualRace, predNValidBin, numobWgt, wgt) %>%
	rename(qualNumobWgt = qualRacequal_numobWgt,
			qualBin = qualRacequal_predNValidBin,
			raceNumobWgt = qualRacerace_numobWgt,
			raceBin = qualRacerace_predNValidBin,
			qualWgt = qualRacequal_wgt,
			raceWgt = qualRacerace_wgt) %>%
	lazy_left_join(rddf %>% rename(OOSRr = rr),
					c('OOSRr', 'driver'),
					c('race', 'modQualPredNValid', 'mod30PredNValid')) %>%
	select(-c(OOSRr, driver)) %>%
	select(race, everything())

# it's ok. the qualWgt thing has become completely unintuitive now, because the numobWgt obscures it. might be worth producing a function that graphically displays what all the parameters are doing, e.g how rapid the time downweights are effectively, how sharp the paceToPairedWinCoef is, and overlay qualNumobWgt and raceNumobWgt to see what effect really is

	smoothParam=list(qrToFit=qrToFit,
					qrToPredict=qrToPredict,
					useStretch=useStretch,
					fwbw=fwbw)
	smoothParamName=GetSmoothParamName()

	DisplayChoice(smoothParam, modelChoice, customSmoothInfo, myRace)
	
	whatToSmooth = customSmoothInfo
	whatToSmooth$modelChoice = modelChoice
	
	### going to want to know this constantly, so define these variables just for brevity
	smoothParam$useQual=smoothParam$qrToFit %in% c('q','qr')
	smoothParam$useRace=smoothParam$qrToFit %in% c('r','qr')
	
	CheckValidSmoothInfo(smoothParam, whatToSmooth)
	
	# this creates racePredNValid, raceDCoef, qualDCoef, stretch
	rddf = MakeGeneralColumnName(rddf, smoothParam, whatToSmooth)
	
	# this calculates when a driver/team/season has done any other races, whether they have a valid estimate today etc
	rddf = DetectDriverHasOtherData(rddf, smoothParam)
	
	### now build up our lists saying how we're going to weight qualy/race when optimising
	
	rddf = MakeNumobForPredict(rddf, smoothParam, whatToSmooth)
	
	### now we need to define what we're going to be smoothing - it depends on whther we're doing stretching, or mixing qs with rs
	### if we're fitting qr, then ought to adjline correct the coefs of whatever we're not predicting
	
	rddf = MakeQualRaceRescaleColumn(rddf, smoothParam)
	rddf = PreSmoothRescaleDCoef(rddf, smoothParam)
	
	OOSRddf = MakeOOSRddf(smoothParam, rddf, myRace)
	
	dum = MakeDTFromTibble(rddf, OOSRddf, smoothParam)
	raceDriverDT = dum$raceDriverDT
	OOSRaceDriverDT = dum$OOSRaceDriverDT
	QRRaceDriverDT = dum$QRRaceDriverDT

	raceDriverDT = MakeIsPredValid(smoothParam, raceDriverDT, myRace)

	if (smoothParam$qrToPredict %in% c('q', 'r')) {
		pairedDriverDT = NULL
		numFinisherByRace = NULL
	}
	if (smoothParam$qrToPredict == 'rfinpos') {
		pairedDriverDT = MakePairedDriverDT(raceDriverDT)
		numFinisherByRace = raceDriverDT[
								!is.na(officialFinishingPosition) & hasOtherData,
									.(numFinisher = .N),
									'race']
	}
								
	DTList = list(raceDriverDT = raceDriverDT,
					OOSRaceDriverDT = OOSRaceDriverDT,
					QRRaceDriverDT = QRRaceDriverDT,
					pairedDriverDT = pairedDriverDT,
					numFinisherByRace = numFinisherByRace)

	### forget about time downweight for now, just do the smooth of the raw coefs
	OOSRaceDriverDT2 = QRRaceDriverDT[,c('OOSRr', 'driver', 'predNValid[OOSRaceDriverDT,, on = c('OOSRr', 'driver')]
						
	
	sumByRDT = OOSRaceDriverDT[,.(sumRescaledDCoef = sum(rescaledDCoef * wgt),
											smoothWgt = sum(wgt)), .(aggregatedRr, driver, team)]
	sumByRDT$smoothRescaledDCoef = with(sumByRDT, sumRescaledDCoef / smoothWgt)

	raceDriverDT = sumByRDT[DTList$raceDriverDT, on = c('aggregatedRr', 'driver', 'team')]

	raceDriverDT[(!hasOtherData), smoothWgt := 0]

# can we combine FilterOutOneOff

date();
for (xx in 1:1000) {
	raceDF$daynumDelta = abs(raceDF$daynum[ri] - raceDF$daynum)
		qualRddf = rddf %>%
					filter(isQualFitValid) %>%
					rename(dCoef = qualDCoefToSmooth) %>%
					lazy_left_join(raceDF, 'race', 'daynumDelta') %>%
					mutate(qualTimeWgt = exp(-mycoef$qualtdwcoef*daynumDelta),
							qualNumobTimeWgt = qualTimeWgt * qualnumobwgtvec)
}
date()					
date();
for (xx in 1:1000) {
	raceDF$daynumDelta = abs(raceDF$daynum[ri] - raceDF$daynum)
		isQualFitValidIndex = with(rddf, isQualFitValid)
		qualRddf = rddf[isQualFitValidIndex,]
		qualRddf$dCoef = qualRddf$qualDCoefToSmooth
		qualRddf$daynumDelta = raceDF$daynumDelta[match(qualRddf$rr, raceDF$rr)]
		qualRddf$qualTimeWgt = with(qualRddf, exp(-mycoef$qualtdwcoef*daynumDelta))
		qualRddf$qualNumobTimeWgt = with(qualRddf, qualTimeWgt * qualnumobwgtvec)
	}
date()
					rename(dCoef = qualDCoefToSmooth) %>%
					lazy_left_join(raceDF, 'race', 'daynumDelta') %>%
					mutate(qualTimeWgt = exp(-mycoef$qualtdwcoef*daynumDelta),
							qualNumobTimeWgt = qualTimeWgt * qualnumobwgtvec)
}
date()					



					lazy_left_join(raceDF, 'race', 'daynumDelta') %>%
					mutate(qualTimeWgt = exp(-mycoef$qualtdwcoef*daynumDelta),
							qualNumobTimeWgt = qualTimeWgt * qualnumobwgtvec)
		if (modelinfo$qrtofit == 'q') {
			qualRddf$wgt = qualRddf$qualNumobTimeWgt
		}
		if (modelinfo$qrtofit == 'qr') {
			qualRddf$wgt = with(qualRddf, mycoef$qrmix * qualNumobTimeWgt)
		}
	}
	if (modelinfo$userace) {
		raceRddf = rddf %>%
					filter(isRaceFitValid) %>%
					rename(dCoef = raceDCoefToSmooth) %>%
					lazy_left_join(raceDF, 'race', 'daynumDelta') %>%
					mutate(raceTimeWgt = exp(-mycoef$racetdwcoef*daynumDelta),
							raceNumobTimeWgt = raceTimeWgt * racenumobwgtvec)
		if (modelinfo$qrtofit == 'r') {
			raceRddf$wgt = raceRddf$raceNumobTimeWgt
		}
		if (modelinfo$qrtofit == 'qr') {
			raceRddf$wgt = with(raceRddf, (1 - mycoef$qrmix) * raceNumobTimeWgt)
		}
	}
	# we want to smooth quali, or race, or both together, which is why this is all a bit ugly
	mySmoothDF = tibble(dCoef = numeric(),
						driver= character(),
						team = character(),
						weight = numeric())
	if (modelinfo$usequal) {
		mySmoothDF = bind_rows(mySmoothDF,
								qualRddf %>%
									select(dCoef, driver, team, wgt))
	}
	if (modelinfo$userace) {
		mySmoothDF = bind_rows(mySmoothDF,
								raceRddf %>%
									select(dCoef, driver, team, wgt))
	}
}
date()

date();
for (xx in 1:1000) {

	smoothByDriverTeam = mySmoothDF %>%
							group_by(driver, team) %>%
							summarise(numobval = sum(wgt),
										smval = sum(dCoef * wgt) / numobval)
	}
date();


SmoothForSingleRace = function(mycoef, ri, modelinfo, rddf) {

	raceDF$daynumDelta = abs(raceDF$daynum[ri] - raceDF$daynum)
	if (modelinfo$usequal) {
		# this might look like it would be suitable for dplyr, but don't be tempted, it's too slow
		isQualFitValidIndex = with(rddf, isQualFitValid)
		qualRddf = rddf[isQualFitValidIndex,]
		qualRddf$dCoef = qualRddf$qualDCoefToSmooth
		qualRddf$daynumDelta = raceDF$daynumDelta[match(qualRddf$rr, raceDF$rr)]
		qualRddf$qualTimeWgt = with(qualRddf, exp(-mycoef$qualtdwcoef*daynumDelta))
		qualRddf$qualNumobTimeWgt = with(qualRddf, qualTimeWgt * qualnumobwgtvec)
		if (modelinfo$qrtofit == 'q') {
			qualRddf$wgt = qualRddf$qualNumobTimeWgt
		}
		if (modelinfo$qrtofit == 'qr') {
			qualRddf$wgt = with(qualRddf, mycoef$qrmix * qualNumobTimeWgt)
		}
	}
	if (modelinfo$userace) {
		isRaceFitValidIndex = with(rddf, isRaceFitValid)
		raceRddf = rddf[isRaceFitValidIndex,]
		raceRddf$dCoef = raceRddf$raceDCoefToSmooth
		raceRddf$daynumDelta = raceDF$daynumDelta[match(raceRddf$rr, raceDF$rr)]
		raceRddf$raceTimeWgt = with(raceRddf, exp(-mycoef$racetdwcoef*daynumDelta))
		raceRddf$raceNumobTimeWgt = with(raceRddf, raceTimeWgt * racenumobwgtvec)
		if (modelinfo$qrtofit == 'r') {
			raceRddf$wgt = raceRddf$raceNumobTimeWgt
		}
		if (modelinfo$qrtofit == 'qr') {
			raceRddf$wgt = with(raceRddf, (1 - mycoef$qrmix) * raceNumobTimeWgt)
		}
	}
	# we want to smooth quali, or race, or both together, which is why this is all a bit ugly
	if (modelinfo$qrtofit == 'q') {
		mySmoothDF = qualRddf %>%
						select(dCoef, driver, team, wgt)
	}
	if (modelinfo$qrtofit == 'r') {
		mySmoothDF = raceRddf %>%
						select(dCoef, driver, team, wgt)
	}
	if (modelinfo$qrtofit == 'qr') {
		mySmoothDF = rbind(qualRddf %>%
								select(dCoef, driver, team, wgt),
							raceRddf %>%
								select(dCoef, driver, team, wgt))
	}

	sumDCoef = with(mySmoothDF, tapply(dCoef * wgt, paste(driver, team), sum))
	sumWgt = with(mySmoothDF, tapply(wgt, paste(driver, team), sum))
	wgtMeanDCoef = sumDCoef / sumWgt

	smoothByDriverTeam = data.frame(driverTeam = names(sumDCoef),
									numobval = sumWgt,
									smval = wgtMeanDCoef)
	return(smoothByDriverTeam)
}
	
	myrr = with(raceDF, rr[race == myRace])
	### now we do the smoothing/timeseries for all races
	### let's have theta in  a nice form:
	mycoef=ProcessCoef(theta, modelinfo)
	
	date()
	for (xx in 1:5) {
	rddf$smval = NA
	rddf$numobval = 0
	for (ri in 1:myrr) {
		
		rddf = FilterRddfByTime(ri, rddf, modelinfo)
		CheckSmoothValidity(rddf, modelinfo)
		if (modelinfo$qrtofit == 'q') anyDataToSmooth = any(rddf$isQualFitValid)
		if (modelinfo$qrtofit == 'r') anyDataToSmooth = any(rddf$isRaceFitValid)
		if (modelinfo$qrtofit == 'qr') {
			anyDataToSmooth = with(rddf, any(isQualFitValid | isRaceFitValid))
		}
		
		if (anyDataToSmooth) {
			### build up a environment for smtsfunct
			currentRaceSmooth = SmoothForSingleRace(mycoef, ri, modelinfo, rddf)
			rddf$toFill = with(rddf, rr == ri & hasOtherData)
			currentRaceToFillIndex = with(rddf, which(toFill))
			currentRaceSmoothToToFillMap = match(with(rddf[currentRaceToFillIndex,],
														paste(driver, team)),
													currentRaceSmooth$driverTeam)
			rddf[currentRaceToFillIndex,c('smval', 'numobval')] =
					currentRaceSmooth[currentRaceSmoothToToFillMap,c('smval', 'numobval')]
		}
	}
	}
date()


date()

	for (xx in 1:5) {
	
	smval=rep(NA,nrow(rddf))
	numobval=rep(0,nrow(rddf))
	for (ri in 1:myrr) {
		qualfitsax=NULL
		racefitsax=NULL
		if (modelinfo$usequal) qualfitsax=with(rddf,which(year==raceDF$year[ri] & modQualPredNValid>0))
		if (modelinfo$userace) racefitsax=with(rddf,which(year==raceDF$year[ri] & racePredNValid>0))
		
		### however, in the (common) case we don't want to use current race, need to flush out those points
		if (modelinfo$usecurrentrace==0) {
			qualfitsax=setdiff(qualfitsax,which(rddf$rr==ri))
			racefitsax=setdiff(racefitsax,which(rddf$rr==ri))
		}
		### and if we're only looking backwards, then get rid of everything that happened after ri
		if (modelinfo$fwbw=='bw') {
			qualfitsax=setdiff(qualfitsax,which(rddf$daynum>raceDF$daynum[ri]))
			racefitsax=setdiff(racefitsax,which(rddf$daynum>raceDF$daynum[ri]))
		}
		if (length(qualfitsax)>0 | length(racefitsax)>0) {
			### build up a environment for smtsfunct
			mydaynum=raceDF$daynum[ri]
			#### here's the big call
			ovsm=f1smoothing::smtsfunct(mycoef, mydaynum, modelinfo, rddf, qualfitsax, racefitsax, dmatlist)
			rsax=which(rddf$rr==ri)
			sax=which(names(ovsm$sm) %in% rddf$driverTeamYear[rsax])
			smval[rsax[match(names(ovsm$sm)[sax],rddf$driverTeamYear[rsax])]]=ovsm$sm[sax]
			### now, are we stretching or not? if we are, need to multiply stretch coef back in here
			if (modelinfo$usestretch==1) {
				smval[which(rddf$rr==ri)]=smval[which(rddf$rr==ri)] * rddf$stretch[which(rddf$rr==ri)]
			}
			numobval[rsax[match(names(ovsm$sm)[sax],rddf$driverTeamYear[rsax])]]=ovsm$numob[sax]
		}
	}
}
date()

# 3 seconds v 5 seconds now ... not sure what is costing the time though

### ok, we're in a good spot now, let's try out data tables smoothing
### here's the setup, to save time:

	method= "downweight";
qrtofit= "q";
qrtopredict= "q";
usecurrentrace= FALSE;
modelchoice= "qual";
usestretch= TRUE;
fwbw= "fwbw";
myRace = "2018abudhabi"

	## qrtofit and qrtopredict can be 'q','r' or 'qr'
	## usecurrentrace can be 1 or 0
	## fwbw is 'fwbw' or 'bw'
	## optparam will optimise the parameters if you haven't saved them yet
	## myrr is last race you want to include when optimising parameters
	
	modelinfo=list(method = 'downweight',
					qrtofit=qrtofit,
					qrtopredict=qrtopredict,
					usecurrentrace=usecurrentrace,
					modelchoice=modelchoice,
					usestretch=usestretch,
					fwbw=fwbw)
	modelparamname=GetModelParamName()

	cat('About to process the following combination:\n')
	DebugDisplay(modelinfo)
	message('myRace = "', myRace, '"')
	
	### going to want to know this constantly, so define these variables just for brevity
	modelinfo$usequal=modelinfo$qrtofit %in% c('q','qr')
	modelinfo$userace=modelinfo$qrtofit %in% c('r','qr')

	CheckValidModelInfo(modelinfo)
	
	# this creates racePredNValid, raceDCoef, qualDCoef, stretch
	rddf = MakeGeneralColumnName(rddf, modelinfo)
	
	# this calculates when a driver/team/season has done any other races, whether they have a valid estimate today etc
	rddf = MakeValidPred(rddf, modelinfo)
	
	### next, we'll use the values derived from timeseries analysis to determine how much weight to put on points
	rddf = MakeFitWeight(rddf, modelinfo)
	
	### now build up our lists saying how we're going to weight qualy/race when optimising
	
	rddf = MakeNumobForPredict(rddf, modelinfo$qrtopredict)
	
	### now we need to define what we're going to be smoothing - it depends on whther we're doing stretching, or mixing qs with rs
	### if we're fitting qr, then ought to adjline correct the coefs of whatever we're not predicting
	
	rddf = AdjustForStretchAndQualAdjline(rddf, modelinfo)
	
	OOSRddf = MakeOOSRddf(modelinfo, rddf, myRace)
	
	maxtheta = RetrieveMaxTheta(myRace, modelinfo)

	theta = maxtheta
	
	OOSRddf = AugmentOOSRddfWithModel(theta, modelinfo, rddf, OOSRddf)

	date()
	for (j in 1:50) {
	smoothByDriverTeam = OOSRddf %>%
							group_by(targetRr, driver, team) %>%
							summarise(numobval = sum(wgt),
										smval = sum(dCoef * wgt) / numobval) %>%
							ungroup() %>%
							rename(rr = targetRr)
							
	rddf = lazy_left_join(rddf,
				smoothByDriverTeam,
				c('rr', 'driver', 'team'),
				c('smval', 'numobval'))
}
date()
# 7 secs with dplyr

library(data.table)
OOSRddf2 = OOSRddf %>%
			rename(OOSRr = rr) %>%
			rename(rr = targetRr)
OOSRdDT = data.table(OOSRddf2)
setkey(OOSRdDT, 'rr', 'driver', 'team')
rdDT = data.table(rddf)
setkey(rdDT, 'rr', 'driver', 'team')

	date()
	for (j in 1:500) {

dCoefSum = OOSRdDT[,sum(dCoef * wgt), .(rr, driver, team)]
names(dCoefSum)[4] = 'dCoefSum'
wgtSum = OOSRdDT[,sum(wgt), .(rr, driver, team)]
names(wgtSum)[4] = 'wgtSum'
dSmooth = dCoefSum[wgtSum]
dSmooth$dSmooth = with(dSmooth, dCoefSum / wgtSum)

rdDTPlusDSmooth = rdDT[dSmooth]
}
date()

# what if we don't set the key?
OOSRddf2 = OOSRddf %>%
			rename(OOSRr = rr) %>%
			rename(rr = targetRr)
OOSRdDT = data.table(OOSRddf2)
#setkey(OOSRdDT, 'rr', 'driver', 'team')
rdDT = data.table(rddf)
#setkey(rdDT, 'rr', 'driver', 'team')

	date()
	for (j in 1:500) {

dCoefSum = OOSRdDT[,sum(dCoef * wgt), .(rr, driver, team)]
names(dCoefSum)[4] = 'dCoefSum'
wgtSum = OOSRdDT[,sum(wgt), .(rr, driver, team)]
names(wgtSum)[4] = 'wgtSum'
dSmooth = dCoefSum[wgtSum, on = c('rr', 'driver','team')]
dSmooth$dSmooth = with(dSmooth, dCoefSum / wgtSum)

rdDTPlusDSmooth = rdDT[dSmooth, on = c('rr','driver','team')]
	}
	date()
# the same
	
## we need new versions of OptimiseParam, LikFunct, CalculateSmoothRddf

MakeDTFromTibble = function(rddf, OOSRddf, modelinfo) {
	# this looks weird, but we actually need to join to rddf in two different ways:
	# we want to match rddfRR to out of sample OOS rr when we augment
	# but having done the aggregation, we then want to match the aggregated thing by race back to rddf
	# so we need to have two different names for rr depending on desired merge
	OOSRddf2 = OOSRddf %>%
				rename(OOSRr = rr) %>%
				rename(aggregatedRr = targetRr)
	rddf2 = rddf %>%
				mutate(OOSRr = rr) %>%
				rename(aggregatedRr = rr)
				
	OOSRaceDriverDT = data.table(OOSRddf2)
	setkey(OOSRaceDriverDT, 'OOSRr', 'driver', 'team')
	raceDriverDT = data.table(rddf2)
	
	if (modelinfo$usequal) {
		qualRaceDriverDT = raceDriverDT[,c('OOSRr', 'driver', 'qualnumobwgtvec', 'qualDCoefToSmooth')]
		oldName = c('qualnumobwgtvec', 'qualDCoefToSmooth')
		newName = c('numobwgtvec', 'dCoefToSmooth')
		for (j in 1:length(oldName)) {
			names(qualRaceDriverDT)[names(qualRaceDriverDT) == oldName[j]] = newName[j]
		}
		qualRaceDriverDT$qualRace = 'qual'
	}
	
	if (modelinfo$usequal) {
		raceRaceDriverDT = raceDriverDT[,c('OOSRr', 'driver', 'racenumobwgtvec', 'raceDCoefToSmooth')]
		oldName = c('racenumobwgtvec', 'raceDCoefToSmooth')
		newName = c('numobwgtvec', 'dCoefToSmooth')
		for (j in 1:length(oldName)) {
			names(raceRaceDriverDT)[names(raceRaceDriverDT) == oldName[j]] = newName[j]
		}
		raceRaceDriverDT$qualRace = 'race'
	}
	
	if (modelinfo$qrtofit == 'q') {
		QRRaceDriverDT = qualRaceDriverDT
	}
	if (modelinfo$qrtofit == 'r') {
		QRRaceDriverDT = raceRaceDriverDT
	}
	if (modelinfo$qrtofit == 'qr') {
		QRRaceDriverDT = rbind(qualRaceDriverDT, raceRaceDriverDT)
	}
	
	return(list(OOSRaceDriverDT = OOSRaceDriverDT,
				raceDriverDT = raceDriverDT,
				QRRaceDriverDT = QRRaceDriverDT))
}


AugmentOOSRaceDriverDTWithModel = function(theta, modelinfo, QRRaceDriverDT, OOSRaceDriverDT) {
	mycoef=ProcessCoef(theta, modelinfo)

	OOSRaceDriverDT = QRRaceDriverDT[OOSRaceDriverDT, on = c('OOSRr', 'driver', 'qualRace')]
	
	OOSRaceDriverDT$tdwCoef = 0
	if (modelinfo$usequal) {
		OOSRaceDriverDT[qualRace == 'qual',tdwCoef := mycoef$qualtdwcoef]
	}
	if (modelinfo$userace) {
		OOSRaceDriverDT[qualRace == 'race',tdwCoef := mycoef$racetdwcoef]
	}
	
	OOSRaceDriverDT$timeWgt = with(OOSRaceDriverDT, exp(-tdwCoef*daynumDelta))
	OOSRaceDriverDT$numobTimeWgt = with(OOSRaceDriverDT, timeWgt * numobwgtvec)

	if (modelinfo$qrtofit %in% c('q', 'r')) {
		OOSRaceDriverDT$wgt = OOSRaceDriverDT$numobTimeWgt
	}
	if (modelinfo$qrtofit == 'qr') {
		OOSRaceDriverDT$qrmix = -99.0
		OOSRaceDriverDT[qualRace == 'qual', qrmix := mycoef$qrmix]
		OOSRaceDriverDT[qualRace == 'race', qrmix := (1 - mycoef$qrmix)]
		OOSRaceDriverDT$wgt = with(OOSRaceDriverDT, qrmix * numobTimeWgt)
	}

	return(OOSRaceDriverDT)
}

dum = MakeDTFromTibble(rddf, OOSRddf, modelinfo)
raceDriverDT = dum$raceDriverDT
OOSRaceDriverDT = dum$OOSRaceDriverDT
QRRaceDriverDT = dum$QRRaceDriverDT

OOSRaceDriverDT = AugmentOOSRaceDriverDTWithModel(maxtheta, modelinfo, QRRaceDriverDT, OOSRaceDriverDT)

CalculateSmoothRaceDriverDT = function(theta, modelinfo, raceDriverDT, OOSRaceDriverDT) {
	
	OOSRddf = AugmentOOSRddfWithModel(theta, modelinfo, rddf, OOSRddf)
	smoothByDriverTeam = OOSRddf %>%
							group_by(targetRr, driver, team) %>%
							summarise(numobval = sum(wgt),
										smval = sum(dCoef * wgt) / numobval) %>%
							ungroup() %>%
							rename(rr = targetRr)
							
	rddf = lazy_left_join(rddf,
				smoothByDriverTeam,
				c('rr', 'driver', 'team'),
				c('smval', 'numobval'))
	rddf$numobval[which(!rddf$hasOtherData)] = 0
	# this should be in a separate function, but we now multiply back out the smval by stretch
	if (modelinfo$usestretch) {
		rddf$smval = rddf$smval * rddf$stretch
	}
	rddf$sqDiff = with(rddf, numobForPredict * (smval - dCoefForPredict)^2)

	return(rddf)
}

##################################################################################################
##################################################################################################
##################################################################################################

### what is the bottleneck in the data table method overall? was expecting it to be >2 times faster than loopy method


	method= "downweight";
qrtofit= "q";
qrtopredict= "q";
usecurrentrace= FALSE;
modelchoice= "qual";
usestretch= TRUE;
fwbw= "fwbw";
myRace = "2018abudhabi"
	
	modelinfo=list(method = 'downweight',
					qrtofit=qrtofit,
					qrtopredict=qrtopredict,
					usecurrentrace=usecurrentrace,
					modelchoice=modelchoice,
					usestretch=usestretch,
					fwbw=fwbw)
	modelparamname=GetModelParamName()

	cat('About to process the following combination:\n')
	DebugDisplay(modelinfo)
	message('myRace = "', myRace, '"')
	
	### going to want to know this constantly, so define these variables just for brevity
	modelinfo$usequal=modelinfo$qrtofit %in% c('q','qr')
	modelinfo$userace=modelinfo$qrtofit %in% c('r','qr')

	CheckValidModelInfo(modelinfo)
	
	# this creates racePredNValid, raceDCoef, qualDCoef, stretch
	rddf = MakeGeneralColumnName(rddf, modelinfo)
	
	# this calculates when a driver/team/season has done any other races, whether they have a valid estimate today etc
	rddf = MakeValidPred(rddf, modelinfo)
	
	### next, we'll use the values derived from timeseries analysis to determine how much weight to put on points
	rddf = MakeFitWeight(rddf, modelinfo)
	
	### now build up our lists saying how we're going to weight qualy/race when optimising
	
	rddf = MakeNumobForPredict(rddf, modelinfo$qrtopredict)
	
	### now we need to define what we're going to be smoothing - it depends on whther we're doing stretching, or mixing qs with rs
	### if we're fitting qr, then ought to adjline correct the coefs of whatever we're not predicting
	
	rddf = AdjustForStretchAndQualAdjline(rddf, modelinfo)
	
	OOSRddf = MakeOOSRddf(modelinfo, rddf, myRace)
		dum = MakeDTFromTibble(rddf, OOSRddf, modelinfo)
		raceDriverDT = dum$raceDriverDT
		OOSRaceDriverDT = dum$OOSRaceDriverDT
		QRRaceDriverDT = dum$QRRaceDriverDT

		maxtheta = RetrieveMaxTheta(myRace, modelinfo)
		
date()
		for (j in 1:500) {
		dum=DTCalculateSmoothRddf(maxtheta, modelinfo, raceDriverDT, OOSRaceDriverDT, QRRaceDriverDT)
}
date()
# 19s

theta=maxtheta
date()
for (j in 1:500) {
	OOSRaceDriverDT2 = AugmentOOSRaceDriverDTWithModel(theta,
														modelinfo,
														OOSRaceDriverDT,
														QRRaceDriverDT)
}
date()
# 9s

OOSRaceDriverDT = AugmentOOSRaceDriverDTWithModel(theta,
														modelinfo,
														OOSRaceDriverDT,
														QRRaceDriverDT)
date()
for (j in 1:500) {

	dCoefSum = OOSRaceDriverDT[,.(dCoefSum = sum(dCoef * wgt),
									numobval = sum(wgt)), .(aggregatedRr, driver, team)]
	dCoefSum$smval = with(dCoefSum, dCoefSum / numobval)

	raceDriverDT2 = dCoefSum[raceDriverDT, on = c('aggregatedRr', 'driver', 'team')]
}
date()
# 6s

# who knows. they key thing didn't help, can't think of anything else to try really

### old world setup:

date()
		for (j in 1:100) {
	sminfo=f1smoothing::fitbyrace(maxtheta, myRace=myRace, modelinfo=modelinfo, rddf=rddf, runmode='fit')	
}
date()
# 45s

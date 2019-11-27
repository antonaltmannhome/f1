
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
	if (display) {
		cat(paste(names(mycoef),mycoef,sep=': '),sep='\n')
	}
	return(list(theta = theta, mycoef = mycoef))
}

MakeGeneralColumnName = function(rddf, smoothParam, whatToSmooth) {
	
	# bit of a dirty thing here, if you want stretching but it's not a standard model, then you need to provide stretching coefs. So how about you provide a modelChoice as well and we use the stretching coefs of that.
	
	isStandardModel = !'dCoefName' %in% names(whatToSmooth)
	
	if (isStandardModel) {
		if (whatToSmooth$modelChoice == 'qual') {
			
			rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, 'qual', 'qual')
		
			rddf$racePredNValid=NULL
			rddf$normRaceDCoef=NULL
			rddf$normQualDCoef=rddf$modQualQualDCoef
		}
	
		if (whatToSmooth$modelChoice != 'qual') {

			rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, whatToSmooth$modelChoice, 'qual')
			rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, whatToSmooth$modelChoice, 'race')

			rddf = rddf %>%
					mutate(racePredNValid = get(paste0('mod',ToUpperFirstLetter(whatToSmooth$modelChoice),'PredNValid')),
							normRaceDCoef = get(paste0('mod',ToUpperFirstLetter(whatToSmooth$modelChoice),'DCoef')),
							normQualDCoef = get(paste0('mod',ToUpperFirstLetter(whatToSmooth$modelChoice),'QualDCoef')))
		}
	}
	
	if (!isStandardModel) {
		if (whatToSmooth$qualRace == 'qual') {
			rddf = rddf %>%
					mutate(normQualDCoef = get(whatToSmooth$dCoefName),
							modQualPredNValid = get(whatToSmooth$predNValidName))
		}
		if (whatToSmooth$qualRace == 'race') {
			rddf = rddf %>%
					mutate(normRaceDCoef = get(whatToSmooth$dCoefName),
							racePredNValid = get(whatToSmooth$predNValidName))
		}
	}
	
	if (smoothParam$useStretch) {
			
		if (whatToSmooth$modelChoice == 'qual') {
			rddf$stretch=raceDF$stretchQual[rddf$rr]
		}
		if (whatToSmooth$modelChoice != 'qual') {
			stretchName = paste('stretch',ToUpperFirstLetter(whatToSmooth$modelChoice),sep='')
			rddf = lazy_left_join(rddf, raceDF, 'race', stretchName) %>%
						mutate(stretch = get(stretchName))
		}
	}
	
	return(rddf)
}

FilterRddfByTime = function(ri, rddf, smoothParam) {
	myYear = raceDF$year[ri]
	if (smoothParam$fwbw == 'bw') {
		rddf$isTimeValid = with(rddf, year == myYear & rr < ri)
	}
	if (smoothParam$fwbw == 'fwbw') {
		rddf$isTimeValid = with(rddf, year == myYear & rr != ri)
	}
	if (smoothParam$useQual) {
		rddf$isQualFitValid = with(rddf, isTimeValid & modQualPredNValid > 0)
	}
	if (smoothParam$useRace) {
		rddf$isRaceFitValid = with(rddf, isTimeValid & racePredNValid > 0)
	}
	return(rddf)
}

DetectDriverHasOtherData = function(rddf, smoothParam) {
	
	rddf[,c('hasOtherQualiData', 'hasOtherRaceData')] = NA
	for (yi in 1:length(unYear)) {
		for (ri in which(raceDF$year==unYear[yi])) {
			currentRaceIndex = with(rddf, which(race == raceDF$race[ri]))
			rddf = FilterRddfByTime(ri, rddf, smoothParam)
			if (smoothParam$useQual) {
				qualiAppearance = rddf %>%
									filter(isQualFitValid) %>%
									distinct(driver, team)
				rddf$hasOtherQualiData[currentRaceIndex] =
					with(rddf[currentRaceIndex,], paste(driver, team)) %in%
						with(qualiAppearance, paste(driver, team))
			}
			if (smoothParam$useRace) {
				raceAppearance = rddf %>%
									filter(isRaceFitValid) %>%
									distinct(driver, team)
				rddf$hasOtherRaceData[currentRaceIndex] =
					with(rddf[currentRaceIndex,], paste(driver, team)) %in%
						with(raceAppearance, paste(driver, team))
			}
		}
	}
	
	if (smoothParam$qrToFit == 'q') {
		rddf$hasOtherData = rddf$hasOtherQualiData
	}
	if (smoothParam$qrToFit == 'r') {
		rddf$hasOtherData = rddf$hasOtherRaceData
	}
	if (smoothParam$qrToFit == 'qr') {
		rddf$hasOtherData = with(rddf, hasOtherQualiData | hasOtherRaceData)
	}
	
	return(rddf)
}

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

MakeNumobForPredict = function(rddf, smoothParam, whatToSmooth) {
	if (smoothParam$qrToPredict == 'q') {
		rddf$numobForPredict = rddf$modQualPredNValid > 0
	}
	if (smoothParam$qrToPredict == 'r') {
		rddf = rddf %>%
				mutate(numobForPredict = case_when(
								racePredNValid < 1E-06 ~ 0,
								between(racePredNValid, 1E-06, 5 - 1E-06) ~ 0.25,
								racePredNValid >=5 ~ 0.5))
	}
	if (smoothParam$qrToPredict == 'rfinpos') {
		rddf = rddf %>%
				mutate(numobForPredict = cleanliness)
	}
	
	return(rddf)
}

MakeQualRaceRescaleColumn = function(rddf, smoothParam) {
	if (smoothParam$qrToFit %in% c('q', 'r')) {
		rddf$qToRRescale = 1
		rddf$rToQRescale = 1
	}
	if (smoothParam$qrToFit == 'qr') {
		rddf$isGoodQualRaceRescale = with(rddf, racePredNValid>5 & rddf$modQualPredNValid>0)
		if (smoothParam$qrToPredict == 'q') {
			mod = lm(normQualDCoef ~ normRaceDCoef - 1,
						data = rddf %>% filter(isGoodQualRaceRescale))
			rddf$rToQRescale = coef(mod)
			rddf$qToRRescale = 1 / coef(mod)
		}
		if (smoothParam$qrToPredict %in% c('r', 'rfinpos')) {
			mod = lm(normRaceDCoef ~ normQualDCoef - 1,
						data = rddf %>% filter(isGoodQualRaceRescale))
			rddf$qToRRescale = coef(mod)
			rddf$rToQRescale = 1 / coef(mod)
		}
	}
	
	return(rddf)
}

PreSmoothRescaleDCoef = function(rddf, smoothParam) {
	
	rddf$qualRescaledDCoef = rddf$raceRescaledDCoef=NA
	
	### firstly any adjline corrections
	if (smoothParam$qrToFit == 'q') {
		rddf$preStretchQualDCoef = rddf$normQualDCoef
	}
	if (smoothParam$qrToFit == 'r') {
		rddf$preStretchRaceDCoef = rddf$normRaceDCoef
	}
	if (smoothParam$qrToFit == 'qr') {
		if (smoothParam$qrToPredict == 'q') {
			rddf$preStretchQualDCoef = rddf$normQualDCoef
			rddf$preStretchRaceDCoef = with(rddf, rToQRescale * normRaceDCoef)
		}
		if (smoothParam$qrToPredict %in% c('r', 'rfinpos')) {
			rddf$preStretchQualDCoef = with(rddf, qToRRescale * normQualDCoef)
			rddf$preStretchRaceDCoef = rddf$normRaceDCoef
		}
	}
	
	if (!smoothParam$useStretch) {
		if (smoothParam$useQual) {
			rddf$qualRescaledDCoef = rddf$preStretchQualDCoef
		}
		if (smoothParam$useRace) {
			rddf$raceRescaledDCoef = rddf$preStretchRaceDCoef
		}
	}
	
	if (smoothParam$useStretch) {
		if (smoothParam$useQual) {
			rddf$qualRescaledDCoef = rddf$preStretchQualDCoef/rddf$stretch
		}
		if (smoothParam$useRace) {
			rddf$raceRescaledDCoef = rddf$preStretchRaceDCoef/rddf$stretch
		}
	}
	
	return(rddf)
}	

PostSmoothRescaleDCoef = function(raceDriverDT, smoothParam) {
	
	### as model stands, we don't need to reverse the qual/race adjline, because we never actualyl try to predict the thing that we rescaled
	### but for stretching we do
	
	if (!smoothParam$useStretch) {
		raceDriverDT$smoothDCoef = raceDriverDT$smoothRescaledDCoef
	}
	if (smoothParam$useStretch) {
		raceDriverDT$smoothDCoef = with(raceDriverDT, stretch * smoothRescaledDCoef)
	}

	return(raceDriverDT)
}

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
			if (smoothParam$qrToFit == 'q') {
				myOOSRddf = qualRddf %>%
								select(rr, driver, team, daynumDelta) %>%
								mutate(targetRr = ri,
										qualRace = 'qual')
			}
			if (smoothParam$qrToFit == 'r') {
				myOOSRddf = raceRddf %>%
								select(rr, driver, team, daynumDelta) %>%
								mutate(targetRr = ri,
										qualRace = 'race')
			}
			if (smoothParam$qrToFit == 'qr') {
				myOOSRddf = rbind(qualRddf %>%
										select(rr, driver, team, daynumDelta) %>%
								mutate(targetRr = ri,
										qualRace = 'qual'),
									raceRddf %>%
										select(rr, driver, team, daynumDelta) %>%
								mutate(targetRr = ri,
										qualRace = 'race'))
			}
		myList[[ri]] = myOOSRddf
		}
	}
	
	OOSRddf = do.call(rbind, myList)
	OOSRddf = as.data.frame(OOSRddf)
	
	return(OOSRddf)
}

MakeDTFromTibble = function(rddf, OOSRddf, smoothParam) {
	# this looks weird, but we actually need to join to rddf in two different ways:
	# we want to match rddfRR to out of sample OOS rr when we augment
	# but having done the aggregation, we then want to match the aggregated thing by race back to rddf
	# so we need to have two different names for rr depending on desired merge
	OOSRaceDriverDT = OOSRddf %>%
				rename(OOSRr = rr) %>%
				rename(aggregatedRr = targetRr) %>%
				data.table
	setkey(OOSRaceDriverDT, 'OOSRr', 'driver', 'team')
	raceDriverDT = rddf %>%
				mutate(OOSRr = rr) %>%
				rename(aggregatedRr = rr) %>%
				data.table
	
	if (smoothParam$useQual) {
		qualRaceDriverDT = raceDriverDT[,c('OOSRr', 'driver', 'qualNumobWgtVec', 'qualRescaledDCoef')]
		oldName = c('qualNumobWgtVec', 'qualRescaledDCoef')
		newName = c('numobwgtvec', 'rescaledDCoef')
		for (j in 1:length(oldName)) {
			names(qualRaceDriverDT)[names(qualRaceDriverDT) == oldName[j]] = newName[j]
		}
		qualRaceDriverDT$qualRace = 'qual'
	}
	
	if (smoothParam$useRace) {
		raceRaceDriverDT = raceDriverDT[,c('OOSRr', 'driver', 'raceNumobWgtVec', 'raceRescaledDCoef')]
		oldName = c('raceNumobWgtVec', 'raceRescaledDCoef')
		newName = c('numobwgtvec', 'rescaledDCoef')
		for (j in 1:length(oldName)) {
			names(raceRaceDriverDT)[names(raceRaceDriverDT) == oldName[j]] = newName[j]
		}
		raceRaceDriverDT$qualRace = 'race'
	}
	
	if (smoothParam$qrToFit == 'q') {
		QRRaceDriverDT = qualRaceDriverDT
	}
	if (smoothParam$qrToFit == 'r') {
		QRRaceDriverDT = raceRaceDriverDT
	}
	if (smoothParam$qrToFit == 'qr') {
		QRRaceDriverDT = rbind(qualRaceDriverDT, raceRaceDriverDT)
	}
	
	return(list(OOSRaceDriverDT = OOSRaceDriverDT,
				raceDriverDT = raceDriverDT,
				QRRaceDriverDT = QRRaceDriverDT))
}

.TransposeCombn = function(xx, nn) {
	myDF = as.data.frame(t(combn(xx, nn)))
	names(myDF) = c('driver1', 'driver2')
	return(myDF)
}
MakePairedDriverDT = function(raceDriverDT) {

	pairedDriverDT = raceDriverDT[!is.na(officialFinishingPosition) & hasOtherData,.TransposeCombn(driver, 2), race]

	pairedDriverDT = setnames(raceDriverDT[,c('race', 'driver', 'officialFinishingPosition')], 'driver', 'driver1')[
					pairedDriverDT, on = c('race', 'driver1')]
	setnames(pairedDriverDT, 'officialFinishingPosition', 'finPos1')

	pairedDriverDT = setnames(raceDriverDT[,c('race', 'driver', 'officialFinishingPosition')], 'driver', 'driver2')[
					pairedDriverDT, on = c('race', 'driver2')]
	setnames(pairedDriverDT, 'officialFinishingPosition', 'finPos2')

	pairedDriverDT$isWinner1 = with(pairedDriverDT, finPos1 < finPos2)
	
	return(pairedDriverDT)
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

CalculateSmoothRaceDriver = function(mycoef, smoothParam, raceDriverDT, OOSRaceDriverDT, QRRaceDriverDT, pairedDriverDT, numFinisherByRace) {
	
	OOSRaceDriverDT = AugmentOOSRaceDriverDTWithModel(mycoef,
														smoothParam,
														OOSRaceDriverDT,
														QRRaceDriverDT)

	sumByRDT = OOSRaceDriverDT[,.(sumRescaledDCoef = sum(rescaledDCoef * wgt),
											smoothWgt = sum(wgt)), .(aggregatedRr, driver, team)]
	sumByRDT$smoothRescaledDCoef = with(sumByRDT, sumRescaledDCoef / smoothWgt)

	raceDriverDT = sumByRDT[raceDriverDT, on = c('aggregatedRr', 'driver', 'team')]

	raceDriverDT[(!hasOtherData), smoothWgt := 0]
	
	# but if we've rescaled things to smooth them, need to undo that for predictions
	raceDriverDT = PostSmoothRescaleDCoef(raceDriverDT, smoothParam)
	
	if (smoothParam$qrToPredict == 'q') {
		raceDriverDT$sqDiff =
			with(raceDriverDT, numobForPredict * (smoothDCoef - normQualDCoef)^2)
	}
	if (smoothParam$qrToPredict == 'r') {
		raceDriverDT$sqDiff =
		with(raceDriverDT, numobForPredict * (smoothDCoef - normRaceDCoef)^2)
	}
	return(raceDriverDT)
}


CalculateExpectedFinPos = function(mycoef, raceDriverDT, pairedDriverDT, numFinisherByRace) {
	combnPlusSmoothDT = setnames(raceDriverDT[,c('race', 'driver', 'smoothDCoef')], 'driver', 'driver1')[
					pairedDriverDT, on = c('race', 'driver1')]
	setnames(combnPlusSmoothDT, 'smoothDCoef', 'smoothDCoef1')

	combnPlusSmoothDT = setnames(raceDriverDT[,c('race', 'driver', 'smoothDCoef')], 'driver', 'driver2')[
					combnPlusSmoothDT, on = c('race', 'driver2')]
	setnames(combnPlusSmoothDT, 'smoothDCoef', 'smoothDCoef2')

	combnPlusSmoothDT$smoothDCoefDelta = with(combnPlusSmoothDT, smoothDCoef1 - smoothDCoef2)

	combnPlusSmoothDT$driver1WinProb = with(combnPlusSmoothDT, invlogit(-mycoef$paceToPairedWinCoef * smoothDCoefDelta))

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

CalculateSmoothRaceDriverPlusEFinPos = function(mycoef, smoothParam, DTList) {
	
	OOSRaceDriverDT = AugmentOOSRaceDriverDTWithModel(mycoef,
														smoothParam,
														DTList$OOSRaceDriverDT,
														DTList$QRRaceDriverDT)

	sumByRDT = OOSRaceDriverDT[,.(sumRescaledDCoef = sum(rescaledDCoef * wgt),
											smoothWgt = sum(wgt)), .(aggregatedRr, driver, team)]
	sumByRDT$smoothRescaledDCoef = with(sumByRDT, sumRescaledDCoef / smoothWgt)

	raceDriverDT = sumByRDT[DTList$raceDriverDT, on = c('aggregatedRr', 'driver', 'team')]

	raceDriverDT[(!hasOtherData), smoothWgt := 0]
	
	# but if we've rescaled things to smooth them, need to undo that for predictions
	raceDriverDT = PostSmoothRescaleDCoef(raceDriverDT, smoothParam)

	if (smoothParam$qrToPredict == 'rfinpos') {
		expectedFinPosDT = CalculateExpectedFinPos(mycoef,
												raceDriverDT,
												DTList$pairedDriverDT,
												DTList$numFinisherByRace)
		raceDriverDT = expectedFinPosDT[,c('race', 'driver', 'expectedFinPos')][
							raceDriverDT,
							on = c('race', 'driver')]
		raceDriverDT$sqDiff = with(raceDriverDT, numobForPredict * (officialFinishingPosition - expectedFinPos)^2)
	}
	
	if (smoothParam$qrToPredict == 'q') {
		raceDriverDT$sqDiff =
			with(raceDriverDT, numobForPredict * (smoothDCoef - normQualDCoef)^2)
	}
	if (smoothParam$qrToPredict == 'r') {
		raceDriverDT$sqDiff =
		with(raceDriverDT, numobForPredict * (smoothDCoef - normRaceDCoef)^2)
	}
	return(raceDriverDT)
}

LikFunct = function(theta, smoothParam, DTList, myRace) {
	mycoef = InitialiseAndProcessCoef(theta, smoothParam)$mycoef
	raceDriverDT = CalculateSmoothRaceDriverPlusEFinPos(mycoef,
											smoothParam,
											DTList)
	meanSqDiff=with(raceDriverDT, mean(sqDiff[isPredValid]))
	return(meanSqDiff)
}

MakeIsPredValid = function(smoothParam, raceDriverDT, myRace) {
	
	myrr = with(raceDF, rr[race == myRace])
	if (smoothParam$qrToPredict == 'q') {
		raceDriverDT$isPredValid =
			with(raceDriverDT, hasOtherData & modQualPredNValid > 0 & aggregatedRr <= myrr)
	}
	if (smoothParam$qrToPredict == 'r') {
		raceDriverDT$isPredValid =
			with(raceDriverDT, hasOtherData & racePredNValid > 0 & aggregatedRr <= myrr)
	}
	if (smoothParam$qrToPredict == 'rfinpos') {
		raceDriverDT$isPredValid =
			with(raceDriverDT, hasOtherData & !is.na(officialFinishingPosition) & aggregatedRr <= myrr)
	}

	return(raceDriverDT)
}

OptimiseParam = function(smoothParam, DTList, myRace) {
	itercount=0
	assign('itercount',itercount,env=globalenv())
	
	thetaInit = InitialiseAndProcessCoef(theta = NULL, smoothParam)$theta
	
	cat('About to obtain smoothing parameters via optimisation\n')
	if (length(thetaInit) == 1) {
		maxInfo=optimise(LikFunct,
						interval=c(2*thetaInit,0.5 * thetaInit),
						smoothParam=smoothParam,
						DTList = DTList,
						myRace=myRace)
		maxTheta=maxInfo$min
	}
	if (length(thetaInit) > 1) {
		maxInfo=nlm2(LikFunct,
						p=thetaInit,
						smoothParam=smoothParam,
						DTList = DTList,
						myRace=myRace)
		maxTheta=maxInfo$est
	}
	message('Maximum has been obtained, coefficients are:')
	mycoef = InitialiseAndProcessCoef(maxTheta, smoothParam, display = TRUE)
	
	return(maxTheta)
}

CheckSmoothValidity = function(rddf, smoothParam) {
	stopWithError = FALSE
	if (smoothParam$useQual) {
		badRow = with(rddf, isQualFitValid &
									(near(modQualPredNValid, 0) | is.na(modQualPredNValid)))
		if (any(badRow)) stopWithError = TRUE
	}
	if (smoothParam$useRace) {
		badRow = with(rddf, isRaceFitValid &
									(near(racePredNValid, 0) | is.na(racePredNValid)))
		if (any(badRow)) stopWithError = TRUE
	}
	if (stopWithError) {
		stop('Some of the coefficients to smooth have weighting of zero or are NA, exiting...\n')
	}
}

GetSmooth=function(qrToFit, qrToPredict, fwbw, useStretch,
					modelChoice = NULL, customSmoothInfo = NULL,
					resetMaxTheta = FALSE, myRace = raceDF$race[nrace],
					smoothDCoefName = NULL, smoothWgtName = NULL, expectedFinPosName = NULL) {
	
	## qrToFit can be 'q','r' or 'qr'
	## qrToPredict can be 'q', 'r' or 'rfinpos'
	## fwbw is 'fwbw' or 'bw'
	## useStretch is T or F
	## resetMaxTheta will optimise the parameters if you haven't saved them yet
	## myRace is last race you want to include when optimising parameters
	## customSmoothInfo incoudes qualRace, dCoefName and predNValidName
	
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
	
	### next, we'll use the values derived from timeseries analysis to determine how much weight to put on points
	rddf = MakeFitWeight(rddf, smoothParam)
	
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

	maxTheta = RetrieveMaxTheta(myRace, smoothParam, whatToSmooth)
	if (is.null(maxTheta) | resetMaxTheta) {
		optParam = TRUE
	}
	if (!is.null(maxTheta) & !resetMaxTheta) {
		optParam = FALSE
		cat('Using previously saved parameter values for smoothing:\n')
	}

	if (optParam) {
		# maxTheta = OptimiseParam(rddf, smoothParam, myRace)
		maxTheta = OptimiseParam(smoothParam, DTList, myRace)
	}
	
	### we're about up to there, no need to do any more right now
	### then generate the smooths to go with these
	mycoef = InitialiseAndProcessCoef(theta = maxTheta, smoothParam)$mycoef
	raceDriverDT = CalculateSmoothRaceDriverPlusEFinPos(mycoef,	smoothParam, DTList)
	if (qrToPredict %in% c('q', 'r')) {
		smoothDF = as_data_frame(raceDriverDT[,c('race', 'driver', 'smoothDCoef', 'smoothWgt')])
	}		
	if (qrToPredict == 'rfinpos') {
		smoothDF = as_data_frame(raceDriverDT[,c('race', 'driver', 'smoothDCoef', 'smoothWgt', 'expectedFinPos')])
	}

	if (!is.null(smoothDCoefName)) {
		smoothDF = smoothDF %>%
					rename(!!smoothDCoefName := smoothDCoef)
	}
	if (!is.null(smoothWgtName)) {
		smoothDF = smoothDF %>%
					rename(!!smoothWgtName := smoothWgt)
	}
	if (!is.null(expectedFinPosName)) {
		smoothDF = smoothDF %>%
					rename(!!expectedFinPosName := expectedFinPos)
	}
	
	sqDiff = LikFunct(maxTheta, smoothParam, DTList, myRace)
	
	return(list(smoothDF = smoothDF, optcoef = maxTheta, sqDiff = sqDiff))
}


InitialiseAndProcessCoef = function(theta = NULL, smoothParam, display = FALSE) {
	# runmode can be 'lik' or 'display'
	mycoef = NULL
	initialiseTheta = is.null(theta)
	count = 1
	if (smoothParam$fwbw %in% c('bw', 'fwbw')) {
		if (smoothParam$qrToFit %in% c('q','qr')) {
			if (initialiseTheta) {
				theta = c(theta, log(0.01))
			}
			mycoef$qualTdwCoef = exp(theta[count])
			count = count + 1
		}
		if (smoothParam$qrToFit %in% c('r','qr')) {
			if (initialiseTheta) {
				theta = c(theta, log(0.01))
			}
			mycoef$raceTdwCoef = exp(theta[count])
			count = count + 1
		}
	}
	if (smoothParam$qrToFit == 'qr') {
		if (initialiseTheta) {
			theta = c(theta, 0)
		}
		mycoef$qualMultiplier = exp(theta[count])
		count = count + 1
	}
	if (smoothParam$qrToPredict == 'rfinpos') {
		if (initialiseTheta) {
			theta = c(theta, log(2))
		}
		mycoef$paceToPairedWinCoef = exp(theta[count])
		count = count + 1
	}
	# then we need to bolster with the bin numob coefs
	# a smooth curve just doesn't work. let's just downweight for lower number of obs
	if (smoothParam$qrToFit %in% c('q', 'qr')) {
		if (initialiseTheta) {
			theta = c(theta, rep(0, 2))
		}
		mycoef$qualNumobWgtCoef = c(exp(theta[count:(count + 1)]), 1)
		count = count + 2
	}
	if (smoothParam$qrToFit %in% c('qr', 'r')) {
		if (initialiseTheta) {
			theta = c(theta, rep(0, 2))
		}
		mycoef$raceNumobWgtCoef = c(exp(theta[count:(count + 1)]), 1)
		count = count + 2
	}

	if (display) {
		roundedPastedCoef = sapply(mycoef, function(x) paste(round(as.numeric(x), 4), collapse = ', '))
		cat(paste(names(mycoef),  roundedPastedCoef, sep = ': '), sep = '\n')
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
				mutate(racePredNValid = get(paste0('mod', ToCamel(whatToSmooth$modelChoice), 'PredNValid')),
						normRaceDCoef = get(paste0('mod', ToCamel(whatToSmooth$modelChoice), 'DCoef')),
						normQualDCoef = get(paste0('mod', ToCamel(whatToSmooth$modelChoice), 'QualDCoef')))
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

		# we have the irritating problem that we might want to have stretch for a race that we've not yet calcualted it for. if that's the case, just use 1, i don't think it actually matters that much for the situations we currently use it
		if (whatToSmooth$modelChoice == 'qual') {
			rddf$stretch = raceDF$stretchQual[rddf$rr]
			rddf$stretch[which(rddf$haveRunQualModel)] = 1
		}
		if (whatToSmooth$modelChoice != 'qual') {
			stretchName = paste('stretch', ToCamel(whatToSmooth$modelChoice), sep = '')
			rddf = lazy_left_join(rddf, raceDF, 'race', stretchName) %>%
						mutate(stretch = get(stretchName))
			rddf$stretch[which(!rddf$haveRunRaceModel)] = 1
		}

	}

	return(rddf)
}

FilterRddfByTime = function(ri, rddf, smoothParam) {
	myYear = raceDF$year[ri]
	if (smoothParam$fwbw == 'bw') {
		rddf$isTimeValid = with(rddf, year == myYear & rr < ri)
	}
	if (smoothParam$fwbw %in% c('fwbw', 'flat')) {
		rddf$isTimeValid = with(rddf, year == myYear & rr != ri)
	}
	if (smoothParam$useQual) {
		rddf$isQualFitValid = with(rddf, isTimeValid & haveRunQualModel & modQualPredNValid > 0)
	}
	if (smoothParam$useRace) {
		rddf$isRaceFitValid = with(rddf, isTimeValid & haveRunQualModel & racePredNValid > 0)
	}
	return(rddf)
}

DetectModelHasBeenRun = function(rddf, raceDF, whatToSmooth) {
	rddf = lazy_left_join(rddf, raceDF, 'race', 'doneQualifyingModel')
	rddf = lazy_left_join(rddf, raceDF, 'race', paste0('doneRawModel', whatToSmooth$modelChoice))
	rddf$haveRunQualModel = with(rddf, doneQualifyingModel)
	rddf$haveRunRaceModel = with(rddf, get(paste0('doneRawModel', whatToSmooth$modelChoice)))

	return(rddf)
}

DetectDriverHasOtherData = function(rddf, smoothParam) {

	rddf[,c('hasOtherQualiData', 'hasOtherRaceData')] = NA
	for (yi in 1:length(unYear)) {
		for (ri in which(raceDF$year==unYear[yi])) {
			currentRaceIndex = with(rddf, which(race == raceDF$race[ri]))
			rddf = f1smoothing:::FilterRddfByTime(ri, rddf, smoothParam)
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

MakePredNValidBin = function(rddf, smoothParam) {

	if (smoothParam$useQual) {
		rddf$qualPredNValidBin = cut(rddf$modQualPredNValid, br = c(-1, 1, 2, 10E6), labels = FALSE)
	}
	if (smoothParam$useRace) {
		rddf$racePredNValidBin = cut(rddf$racePredNValid, br = c(-1, 5, 15, 10E6), labels = FALSE)
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

	rddf$qualRescaledDCoef = rddf$raceRescaledDCoef = NA

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

MakeOOSRddf = function(smoothParam, rddf) {
	### now we do the smoothing/timeseries for all races
	### let's have theta in  a nice form:

	myList = NULL

	for (ri in 1:nrace) {

		rddf = f1smoothing:::FilterRddfByTime(ri, rddf, smoothParam)
		f1smoothing:::CheckSmoothValidity(rddf, smoothParam)
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
				if (smoothParam$fwbw %in% c('bw', 'fwbw')) {
					qualRddf$daynumDelta = raceDF$daynumDelta[match(qualRddf$rr, raceDF$rr)]
				}
				if (smoothParam$fwbw == 'flat') {
					qualRddf$daynumDelta = 0
				}
			}
			if (smoothParam$useRace) {
				isRaceFitValidIndex = with(rddf, isRaceFitValid)
				raceRddf = rddf[isRaceFitValidIndex,]
				if (smoothParam$fwbw %in% c('bw', 'fwbw')) {
					raceRddf$daynumDelta = raceDF$daynumDelta[match(raceRddf$rr, raceDF$rr)]
				}
				if (smoothParam$fwbw == 'flat') {
					raceRddf$daynumDelta = 0
				}
			}
			# we want to smooth quali, or race, or both together, which is why this is all a bit ugly
			if (smoothParam$useQual) {
				# this is just while we develpo the model, will ditch when possible
				qualOOSRddf = qualRddf %>%
								select(rr, driver, team, daynumDelta) %>%
								mutate(targetRr = ri,
										qualRace = 'qual')
			}
			if (smoothParam$useRace) {
				raceOOSRddf = raceRddf %>%
								select(rr, driver, team, daynumDelta) %>%
								mutate(targetRr = ri,
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
		qualRaceDriverDT = raceDriverDT[,c('OOSRr', 'driver', 'qualPredNValidBin', 'qualRescaledDCoef')]
		oldName = c('qualPredNValidBin', 'qualRescaledDCoef')
		newName = c('predNValidBin', 'rescaledDCoef')
		for (j in 1:length(oldName)) {
			names(qualRaceDriverDT)[names(qualRaceDriverDT) == oldName[j]] = newName[j]
		}
		qualRaceDriverDT$qualRace = 'qual'
	}

	if (smoothParam$useRace) {
		raceRaceDriverDT = raceDriverDT[,c('OOSRr', 'driver', 'racePredNValidBin', 'raceRescaledDCoef')]
		oldName = c('racePredNValidBin', 'raceRescaledDCoef')
		newName = c('predNValidBin', 'rescaledDCoef')
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

	pairedDriverDT = raceDriverDT[!is.na(officialFinishingPosition) & hasOtherData, f1smoothing:::.TransposeCombn(driver, 2), race]

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

	# first step, we allocate the weights to each race based on predNValid
	QRRaceDriverDT$numobWgt = 0
	if (smoothParam$useQual) {
		if (FALSE) {
			# can't get smooth function to work, adapt this if you think it's possible
			QRRaceDriverDT[qualRace == 'qual',
				numobWgt := mycoef$qualNumobWgtCoef[1] +
							(1 - mycoef$qualNumobWgtCoef[1]) * (1 - exp(-mycoef$qualNumobWgtCoef[2] *predNValid))
							]
		}
		QRRaceDriverDT[qualRace == 'qual',
							numobWgt := mycoef$qualNumobWgtCoef[predNValidBin]]
	}
	if (smoothParam$useRace) {
		if (FALSE) {
			QRRaceDriverDT[qualRace == 'race',
				numobWgt := mycoef$raceNumobWgtCoef[1] +
							(1 - mycoef$raceNumobWgtCoef[1]) * (1 - exp(-mycoef$raceNumobWgtCoef[2] *predNValid))
							]
		}
		QRRaceDriverDT[qualRace == 'race',
							numobWgt := mycoef$raceNumobWgtCoef[predNValidBin]]
	}

	OOSRaceDriverDT = QRRaceDriverDT[OOSRaceDriverDT, on = c('OOSRr', 'driver', 'qualRace')]

	OOSRaceDriverDT$tdwCoef = 0
	if (smoothParam$fwbw %in% c('bw', 'fwbw')) {
		if (smoothParam$useQual) {
			OOSRaceDriverDT[qualRace == 'qual', tdwCoef := mycoef$qualTdwCoef]
		}
		if (smoothParam$useRace) {
			OOSRaceDriverDT[qualRace == 'race', tdwCoef := mycoef$raceTdwCoef]
		}
		OOSRaceDriverDT$timeWgt = with(OOSRaceDriverDT, exp(-tdwCoef*daynumDelta))
	}
	if (smoothParam$fwbw == 'flat') {
		OOSRaceDriverDT$timeWgt = 1
	}

	OOSRaceDriverDT$numobTimeWgt = with(OOSRaceDriverDT, timeWgt * numobWgt)

	if (smoothParam$qrToFit %in% c('q', 'r')) {
		OOSRaceDriverDT$wgt = OOSRaceDriverDT$numobTimeWgt
	}
	if (smoothParam$qrToFit == 'qr') {
		OOSRaceDriverDT$qualMultiplier = -99
		OOSRaceDriverDT[qualRace == 'qual', qualMultiplier := mycoef$qualMultiplier]
		OOSRaceDriverDT[qualRace == 'race', qualMultiplier := 1]
		OOSRaceDriverDT$wgt = with(OOSRaceDriverDT, qualMultiplier * numobTimeWgt)
	}

	return(OOSRaceDriverDT)
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

	OOSRaceDriverDT = f1smoothing:::AugmentOOSRaceDriverDTWithModel(mycoef,
														smoothParam,
														DTList$OOSRaceDriverDT,
														DTList$QRRaceDriverDT)

	sumByRDT = OOSRaceDriverDT[,.(sumRescaledDCoef = sum(rescaledDCoef * wgt),
											smoothWgt = sum(wgt)), .(aggregatedRr, driver, team)]
	sumByRDT$smoothRescaledDCoef = with(sumByRDT, sumRescaledDCoef / smoothWgt)

	raceDriverDT = sumByRDT[DTList$raceDriverDT, on = c('aggregatedRr', 'driver', 'team')]

	raceDriverDT[(!hasOtherData), smoothWgt := 0]

	# but if we've rescaled things to smooth them, need to undo that for predictions
	raceDriverDT = f1smoothing:::PostSmoothRescaleDCoef(raceDriverDT, smoothParam)

	if (smoothParam$qrToPredict == 'rfinpos') {
		expectedFinPosDT = f1smoothing:::CalculateExpectedFinPos(mycoef,
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
	return(list(raceDriverDT = raceDriverDT,
				OOSRaceDriverDT = OOSRaceDriverDT))
}

LikFunct = function(theta, smoothParam, DTList) {
	mycoef = f1smoothing:::InitialiseAndProcessCoef(theta, smoothParam)$mycoef
	raceDriverDT = f1smoothing:::CalculateSmoothRaceDriverPlusEFinPos(mycoef,
											smoothParam,
											DTList)$raceDriverDT
	meanSqDiff=with(raceDriverDT, mean(sqDiff[isPredValid]))

	assign('iterCount', iterCount + 1, env = globalenv())
	if ( (iterCount %% 10) == 0) {
		message('Have done ', iterCount, ' iterations, latest tried coefs:')
		print(mycoef)
		message('Current meanSqDiff:')
		print(meanSqDiff)
	}
	return(meanSqDiff)
}

MakeIsPredValid = function(smoothParam, raceDriverDT) {

	if (smoothParam$qrToPredict == 'q') {
		raceDriverDT$isPredValid =
			with(raceDriverDT, hasOtherData & haveRunQualModel & modQualPredNValid > 0)
	}
	if (smoothParam$qrToPredict == 'r') {
		raceDriverDT$isPredValid =
			with(raceDriverDT, hasOtherData & haveRunRaceModel & racePredNValid > 0)
	}
	if (smoothParam$qrToPredict == 'rfinpos') {
		raceDriverDT$isPredValid =
			with(raceDriverDT, hasOtherData & !is.na(officialFinishingPosition))
	}

	return(raceDriverDT)
}

OptimiseParam = function(smoothParam, DTList) {

	theta = f1smoothing:::InitialiseAndProcessCoef(theta = NULL, smoothParam)$theta

	cat('About to obtain smoothing parameters via optimisation\n')

	maxInfo=nlm2(f1smoothing:::LikFunct,
					p = theta,
					smoothParam = smoothParam,
					DTList = DTList)
	maxTheta=maxInfo$est

	message('Maximum has been obtained, coefficients are:')
	mycoef = f1smoothing:::InitialiseAndProcessCoef(maxTheta, smoothParam, display = TRUE)

	return(maxTheta)
}

CheckSmoothValidity = function(rddf, smoothParam) {
	stopWithError = FALSE
	if (smoothParam$useQual) {
		badRow = with(rddf, isQualFitValid &
									(near(modQualPredNValid, 0) | is.na(modQualPredNValid)))
		if (any(badRow, na.rm = TRUE)) stopWithError = TRUE
	}
	if (smoothParam$useRace) {
		badRow = with(rddf, isRaceFitValid &
									(near(racePredNValid, 0) | is.na(racePredNValid)))
		if (any(badRow, na.rm = TRUE)) stopWithError = TRUE
	}
	if (stopWithError) {
		stop('Some of the coefficients to smooth have weighting of zero or are NA, exiting...\n')
	}
}

GetSmooth = function(qrToFit, qrToPredict, fwbw, useStretch,
					modelChoice = NULL, customSmoothInfo = NULL,
					resetMaxTheta = FALSE,
					smoothDCoefName = NULL, smoothWgtName = NULL, expectedFinPosName = NULL) {

	## qrToFit can be 'q','r' or 'qr'
	## qrToPredict can be 'q', 'r' or 'rfinpos'
	## fwbw is 'fwbw' or 'bw' or 'flat'
	## useStretch is T or F
	## resetMaxTheta will optimise the parameters if you haven't saved them yet
	## useExistingMaxTheta: set this to a race name. This is useful when you haven't yet stored the coefs for the current race but still want to do a smooth for any other reason
	## stopAtRace: if you're in the process of looping through races, you might not have all the predNValids filled out. use this to tell smoother not to try to fit any races beyond stopAtRace, because they will be NA, which it's not designed to deal with
		## you might thing useExistingMaxTheta and stopAtRace are the same but there is a messy situation which we need them to be different. After 5 races overall e.g., we don't have enough races to estimate a maxTheta. However, we might want to use maxTheta from a recent race in order to smooth just the first 5 races. so stopAtRace would be race 5, but useExistingMaxTheta would be nrace - 1
	## customSmoothInfo incoudes qualRace, dCoefName and predNValidName

	smoothParam=list(qrToFit = qrToFit,
					qrToPredict = qrToPredict,
					useStretch = useStretch,
					fwbw = fwbw)
	smoothParamName = f1smoothing:::GetSmoothParamName()

	f1smoothing:::DisplayChoice(smoothParam, modelChoice, customSmoothInfo)

	whatToSmooth = customSmoothInfo
	whatToSmooth$modelChoice = modelChoice

	### going to want to know this constantly, so define these variables just for brevity
	smoothParam$useQual=smoothParam$qrToFit %in% c('q', 'qr')
	smoothParam$useRace=smoothParam$qrToFit %in% c('r', 'qr')

	f1smoothing:::CheckValidSmoothInfo(smoothParam, whatToSmooth)

	# this checks that the model has been run. could quite conceivably (in fact we do, when fitting model 4) be in situation where you want to smooth all the races but current model has not yet been fit.
	rddf = f1smoothing:::DetectModelHasBeenRun(rddf, raceDF, whatToSmooth)

	# this creates racePredNValid, raceDCoef, qualDCoef, stretch
	rddf = f1smoothing:::MakeGeneralColumnName(rddf, smoothParam, whatToSmooth)

	# this calculates when a driver/team/season has done any other races, whether they have a valid estimate today etc
	rddf = f1smoothing:::DetectDriverHasOtherData(rddf, smoothParam)

	### now build up our lists saying how we're going to weight qualy/race when optimising

	rddf = f1smoothing:::MakePredNValidBin(rddf, smoothParam)
	rddf = f1smoothing:::MakeNumobForPredict(rddf, smoothParam, whatToSmooth)

	### now we need to define what we're going to be smoothing - it depends on whther we're doing stretching, or mixing qs with rs
	### if we're fitting qr, then ought to adjline correct the coefs of whatever we're not predicting

	rddf = f1smoothing:::MakeQualRaceRescaleColumn(rddf, smoothParam)
	rddf = f1smoothing:::PreSmoothRescaleDCoef(rddf, smoothParam)

	OOSRddf = f1smoothing:::MakeOOSRddf(smoothParam, rddf)

	dum = f1smoothing:::MakeDTFromTibble(rddf, OOSRddf, smoothParam)
	raceDriverDT = dum$raceDriverDT
	OOSRaceDriverDT = dum$OOSRaceDriverDT
	QRRaceDriverDT = dum$QRRaceDriverDT

	raceDriverDT = f1smoothing:::MakeIsPredValid(smoothParam, raceDriverDT)

	if (smoothParam$qrToPredict %in% c('q', 'r')) {
		pairedDriverDT = NULL
		numFinisherByRace = NULL
	}
	if (smoothParam$qrToPredict == 'rfinpos') {
		pairedDriverDT = f1smoothing:::MakePairedDriverDT(raceDriverDT)
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

	iterCount = 0
	assign('iterCount', iterCount, env = globalenv())
	# NB need to do this, it can't increase in separate Likfunct calls otherwise

	maxTheta = f1smoothing:::RetrieveMaxTheta(smoothParam, whatToSmooth)
	if (is.null(maxTheta) | resetMaxTheta) {
		optParam = TRUE
	}
	if (!is.null(maxTheta) & !resetMaxTheta) {
		optParam = FALSE
		cat('Using previously saved parameter values for smoothing:\n')
		f1smoothing:::InitialiseAndProcessCoef(maxTheta, smoothParam, display = TRUE)
	}

	if (optParam) {
		maxTheta = f1smoothing:::OptimiseParam(smoothParam, DTList)
	}

	### we're about up to there, no need to do any more right now
	### then generate the smooths to go with these
	mycoef = f1smoothing:::InitialiseAndProcessCoef(theta = maxTheta, smoothParam)$mycoef
	dum = f1smoothing:::CalculateSmoothRaceDriverPlusEFinPos(mycoef,	smoothParam, DTList)
	raceDriverDT = dum$raceDriverDT
	OOSRaceDriverDT = dum$OOSRaceDriverDT
	# v annoying not have the race attached to that, let's add it
	OOSRaceDriverDT$aggregatedRace = raceDF$race[match(OOSRaceDriverDT$aggregatedRr, raceDF$rr)]
	if (qrToPredict %in% c('q', 'r')) {
		smoothDF = as_tibble(raceDriverDT[,c('race', 'driver', 'smoothDCoef', 'smoothWgt')])
	}
	if (qrToPredict == 'rfinpos') {
		smoothDF = as_tibble(raceDriverDT[,c('race', 'driver', 'smoothDCoef', 'smoothWgt', 'expectedFinPos')])
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

	sqDiff = f1smoothing:::LikFunct(maxTheta, smoothParam, DTList)
	# NB we return rddf just in case you want some of the intermediate columns, e.g adjline/stretch corrected qualifying times

	return(list(smoothDF = smoothDF, OOSRaceDriverDT = OOSRaceDriverDT, optcoef = maxTheta, mycoef =mycoef, sqDiff = sqDiff, rddf = rddf))
}

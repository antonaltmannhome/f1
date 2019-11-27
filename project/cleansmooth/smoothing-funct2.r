
ProcessCoef=function(theta, modelinfo, display=F) {
	# runmode can be 'lik' or 'display'
	mycoef=NULL
	count=1
	if (modelinfo$qrtofit %in% c('q','qr')) {
		mycoef$qualtdwcoef=exp(theta[count])
		count=count+1
	}
	if (modelinfo$qrtofit %in% c('r','qr')) {
		mycoef$racetdwcoef=exp(theta[count])
		count=count+1
	}
	if (modelinfo$qrtofit=='qr') {
		mycoef$qrmix=invlogit(theta[count])
		count=count+1
	}
	if (display) {
		cat(paste(names(mycoef),mycoef,sep=': '),sep='\n')
	}
	return(mycoef)
}

MakeGeneralColumnName = function(rddf, modelinfo) {
	
	if (modelinfo$modelchoice == 'qual') {
		
		rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, 'qual', 'qual')
	
		rddf$racePredNValid=NULL
		rddf$normRaceDCoef=NULL
		rddf$normQualDCoef=rddf$modQualQualDCoef
		if (modelinfo$usestretch) {
			rddf$stretch=raceDF$stretchQual[rddf$rr]
		}
	}
	
	if (modelinfo$modelchoice != 'qual') {

		rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, modelinfo$modelchoice, 'qual')
		rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, modelinfo$modelchoice, 'race')

		rddf = rddf %>%
				mutate(racePredNValid = get(paste0('mod',ToUpperFirstLetter(modelinfo$modelchoice),'PredNValid')),
						normRaceDCoef = get(paste0('mod',ToUpperFirstLetter(modelinfo$modelchoice),'DCoef')),
						normQualDCoef = get(paste0('mod',ToUpperFirstLetter(modelinfo$modelchoice),'QualDCoef')))
		if (modelinfo$usestretch) {
			stretchname = paste('stretch',ToUpperFirstLetter(modelinfo$modelchoice),sep='')
			rddf = lazy_left_join(rddf, raceDF, 'race', stretchname) %>%
					mutate(stretch = get(stretchname))
		}
	}

	return(rddf)
}

FilterRddfByTime = function(ri, rddf, modelinfo) {
	myYear = raceDF$year[ri]
	if (modelinfo$fwbw == 'bw') {
		rddf$isTimeValid = with(rddf, year == myYear & rr < ri)
	}
	if (modelinfo$fwbw == 'fwbw') {
		rddf$isTimeValid = with(rddf, year == myYear & rr != ri)
	}
	if (modelinfo$useQual) {
		rddf$isQualFitValid = with(rddf, isTimeValid & modQualPredNValid > 0)
	}
	if (modelinfo$useRace) {
		rddf$isRaceFitValid = with(rddf, isTimeValid & racePredNValid > 0)
	}
	return(rddf)
}

DetectDriverHasOtherData = function(rddf, modelinfo) {
	
	rddf[,c('hasOtherQualiData', 'hasOtherRaceData')] = NA
	for (yi in 1:length(unYear)) {
		for (ri in which(raceDF$year==unYear[yi])) {
			currentRaceIndex = with(rddf, which(race == raceDF$race[ri]))
			rddf = FilterRddfByTime(ri, rddf, modelinfo)
			if (modelinfo$useQual) {
				qualiAppearance = rddf %>%
									filter(isQualFitValid) %>%
									distinct(driver, team)
				rddf$hasOtherQualiData[currentRaceIndex] =
					with(rddf[currentRaceIndex,], paste(driver, team)) %in%
						with(qualiAppearance, paste(driver, team))
			}
			if (modelinfo$useRace) {
				raceAppearance = rddf %>%
									filter(isRaceFitValid) %>%
									distinct(driver, team)
				rddf$hasOtherRaceData[currentRaceIndex] =
					with(rddf[currentRaceIndex,], paste(driver, team)) %in%
						with(raceAppearance, paste(driver, team))
			}
		}
	}
	
	if (modelinfo$qrtofit == 'q') {
		rddf$hasOtherData = rddf$hasOtherQualiData
	}
	if (modelinfo$qrtofit == 'r') {
		rddf$hasOtherData = rddf$hasOtherRaceData
	}
	if (modelinfo$qrtofit == 'qr') {
		rddf$hasOtherData = with(rddf, hasOtherQualiData | hasOtherRaceData)
	}
	
	return(rddf)
}

MakeFitWeight = function(rddf, modelinfo) {
	
	rddf$qualnumobwgtvec=rddf$racenumobwgtvec=rep(NA,dim(rddf)[1])
	
	if (modelinfo$useQual) {
		rddf$qualnumobwgtvec[which(rddf$modQualPredNValid<=1)]=0.8
		rddf$qualnumobwgtvec[which(rddf$modQualPredNValid>1)]=1
	}
	if (modelinfo$useRace) {
		rddf$racenumobwgtvec[which(rddf$racePredNValid<=30)]=0.5 + 0.5*rddf$racePredNValid[which(rddf$racePredNValid<=30)]/30
		rddf$racenumobwgtvec[which(rddf$racePredNValid>30)]=1
	}

	return(rddf)
}

MakeNumobForPredict = function(rddf, myqrtopredict) {
	if (myqrtopredict == 'q') {
		rddf$numobForPredict = rddf$modQualPredNValid > 0
	}
	if (myqrtopredict == 'r') {
		rddf = rddf %>%
				mutate(numobForPredict = case_when(
								racePredNValid < 1E-06 ~ 0,
								between(racePredNValid, 1E-06, 5 - 1E-06) ~ 0.25,
								racePredNValid >=5 ~ 0.5))
	}
	
	return(rddf)
}

MakeQualRaceRescaleColumn = function(rddf, modelinfo) {
	if (modelinfo$qrtofit %in% c('q', 'r')) {
		rddf$qToRRescale = 1
		rddf$rToQRescale = 1
	}
	if (modelinfo$qrtofit == 'qr') {
		rddf$isGoodQualRaceRescale = with(rddf, racePredNValid>5 & rddf$modQualPredNValid>0)
		if (modelinfo$qrtopredict == 'q') {
			mod = lm(normQualDCoef ~ normRaceDCoef - 1,
						data = rddf %>% filter(isGoodQualRaceRescale))
			rddf$rToQRescale = coef(mod)
			rddf$qToRRescale = 1 / coef(mod)
		}
		if (modelinfo$qrtopredict == 'r') {
			mod = lm(normRaceDCoef ~ normQualDCoef - 1,
						data = rddf %>% filter(isGoodQualRaceRescale))
			rddf$qToRRescale = coef(mod)
			rddf$rToQRescale = 1 / coef(mod)
		}
	}
	
	return(rddf)
}

PreSmoothRescaleDCoef = function(rddf, modelinfo) {
	
	rddf$qualDCoefToSmooth=rddf$raceDCoefToSmooth=NA
	
	### firstly any adjline corrections
	if (modelinfo$qrtofit=='q') {
		rddf$preStretchQualDCoef = rddf$normQualDCoef
	}
	if (modelinfo$qrtofit=='r') {
		rddf$preStretchRaceDCoef = rddf$normRaceDCoef
	}
	if (modelinfo$qrtofit=='qr') {
		if (modelinfo$qrtopredict=='q') {
			rddf$preStretchQualDCoef = rddf$normQualDCoef
			rddf$preStretchRaceDCoef=with(rddf, rToQRescale * normRaceDCoef)
		}
		if (modelinfo$qrtopredict=='r') {
			rddf$preStretchQualDCoef = with(rddf, qToRRescale * normQualDCoef)
			rddf$preStretchRaceDCoef = rddf$normRaceDCoef
		}
	}
	
	if (!modelinfo$usestretch) {
		if (modelinfo$useQual) {
			rddf$qualDCoefToSmooth=rddf$preStretchQualDCoef
		}
		if (modelinfo$useRace) {
			rddf$raceDCoefToSmooth=rddf$preStretchRaceDCoef
		}
	}
	
	if (modelinfo$usestretch) {
		if (modelinfo$useQual) {
			rddf$qualDCoefToSmooth=rddf$preStretchQualDCoef/rddf$stretch
		}
		if (modelinfo$useRace) {
			rddf$raceDCoefToSmooth=rddf$preStretchRaceDCoef/rddf$stretch
		}
	}
	
	return(rddf)
}	

PostSmoothRescaleDCoef = function(raceDriverDT, modelinfo) {
	
	### as model stands, we don't need to reverse the qual/race adjline, because we never actualyl try to predict the thing that we rescaled
	### but for stretching we do
	
	if (!modelinfo$usestretch) {
		raceDriverDT$rescaledSmoothDCoef = raceDriverDT$smoothDCoef
	}
	if (modelinfo$usestretch) {
		raceDriverDT$rescaledSmoothDCoef = with(raceDriverDT, stretch * smoothDCoef)
	}

	return(raceDriverDT)
}

MakeOOSRddf = function(modelinfo, rddf, myRace) {
	myrr = with(raceDF, rr[race == myRace])
	### now we do the smoothing/timeseries for all races
	### let's have theta in  a nice form:
		
	myList = NULL
		
	for (ri in 1:myrr) {
		
		rddf = FilterRddfByTime(ri, rddf, modelinfo)
		CheckSmoothValidity(rddf, modelinfo)
		if (modelinfo$qrtofit == 'q') anyDataToSmooth = any(rddf$isQualFitValid)
		if (modelinfo$qrtofit == 'r') anyDataToSmooth = any(rddf$isRaceFitValid)
		if (modelinfo$qrtofit == 'qr') {
			anyDataToSmooth = with(rddf, any(isQualFitValid | isRaceFitValid))
		}
		
		if (anyDataToSmooth) {
			# get this race's version of rddf
			raceDF$daynumDelta = abs(raceDF$daynum[ri] - raceDF$daynum)
			if (modelinfo$useQual) {
				# this might look like it would be suitable for dplyr, but don't be tempted, it's too slow
				isQualFitValidIndex = with(rddf, isQualFitValid)
				qualRddf = rddf[isQualFitValidIndex,]
				qualRddf$daynumDelta = raceDF$daynumDelta[match(qualRddf$rr, raceDF$rr)]
			}
			if (modelinfo$useRace) {
				isRaceFitValidIndex = with(rddf, isRaceFitValid)
				raceRddf = rddf[isRaceFitValidIndex,]
				raceRddf$daynumDelta = raceDF$daynumDelta[match(raceRddf$rr, raceDF$rr)]
			}
			# we want to smooth quali, or race, or both together, which is why this is all a bit ugly
			if (modelinfo$qrtofit == 'q') {
				myOOSRddf = qualRddf %>%
								select(rr, driver, team, daynumDelta) %>%
								mutate(targetRr = ri,
										qualRace = 'qual')
			}
			if (modelinfo$qrtofit == 'r') {
				myOOSRddf = raceRddf %>%
								select(rr, driver, team, daynumDelta) %>%
								mutate(targetRr = ri,
										qualRace = 'race')
			}
			if (modelinfo$qrtofit == 'qr') {
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
	
	if (modelinfo$useQual) {
		qualRaceDriverDT = raceDriverDT[,c('OOSRr', 'driver', 'qualnumobwgtvec', 'qualDCoefToSmooth')]
		oldName = c('qualnumobwgtvec', 'qualDCoefToSmooth')
		newName = c('numobwgtvec', 'dCoef')
		for (j in 1:length(oldName)) {
			names(qualRaceDriverDT)[names(qualRaceDriverDT) == oldName[j]] = newName[j]
		}
		qualRaceDriverDT$qualRace = 'qual'
	}
	
	if (modelinfo$useRace) {
		raceRaceDriverDT = raceDriverDT[,c('OOSRr', 'driver', 'racenumobwgtvec', 'raceDCoefToSmooth')]
		oldName = c('racenumobwgtvec', 'raceDCoefToSmooth')
		newName = c('numobwgtvec', 'dCoef')
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


AugmentOOSRaceDriverDTWithModel = function(theta, modelinfo, OOSRaceDriverDT, QRRaceDriverDT) {

	mycoef=ProcessCoef(theta, modelinfo)

	OOSRaceDriverDT = QRRaceDriverDT[OOSRaceDriverDT, on = c('OOSRr', 'driver', 'qualRace')]
	
	OOSRaceDriverDT$tdwCoef = 0
	if (modelinfo$useQual) {
		OOSRaceDriverDT[qualRace == 'qual',tdwCoef := mycoef$qualtdwcoef]
	}
	if (modelinfo$useRace) {
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

CalculateSmoothRaceDriver = function(theta, modelinfo, raceDriverDT, OOSRaceDriverDT, QRRaceDriverDT) {
	
	OOSRaceDriverDT = AugmentOOSRaceDriverDTWithModel(theta,
														modelinfo,
														OOSRaceDriverDT,
														QRRaceDriverDT)

	dCoefSum = OOSRaceDriverDT[,.(sumDCoef = sum(dCoef * wgt),
									numobval = sum(wgt)), .(aggregatedRr, driver, team)]
	dCoefSum$smoothDCoef = with(dCoefSum, sumDCoef / numobval)

	raceDriverDT = dCoefSum[raceDriverDT, on = c('aggregatedRr', 'driver', 'team')]

	raceDriverDT[(!hasOtherData), numobval := 0]
	
	# but if we've rescaled things to smooth them, need to undo that for predictions
	raceDriverDT = PostSmoothRescaleDCoef(raceDriverDT, modelinfo)
	
	if (modelinfo$qrtopredict == 'q') {
		raceDriverDT$sqDiff =
			with(raceDriverDT, numobForPredict * (rescaledSmoothDCoef - normQualDCoef)^2)
	}
	if (modelinfo$qrtopredict == 'r') {
		raceDriverDT$sqDiff =
		with(raceDriverDT, numobForPredict * (rescaledSmoothDCoef - normRaceDCoef)^2)
	}
		
	## ugh, get rid of this when possible
	raceDriverDT$smval = raceDriverDT$rescaledSmoothDCoef
	return(raceDriverDT)
}

LikFunct = function(theta, modelinfo, raceDriverDT, OOSRaceDriverDT, QRRaceDriverDT, myRace) {
	raceDriverDT = CalculateSmoothRaceDriver(theta,
											modelinfo,
											raceDriverDT,
											OOSRaceDriverDT,
											QRRaceDriverDT)
	myrr = with(raceDF, rr[race == myRace])
	if (modelinfo$qrtopredict == 'q') {
		raceDriverDT$isPredValid =
			with(raceDriverDT, hasOtherData & modQualPredNValid > 0 & aggregatedRr <= myrr)
	}
	if (modelinfo$qrtopredict == 'r') {
		raceDriverDT$isPredValid =
			with(raceDriverDT, hasOtherData & racePredNValid > 0 & aggregatedRr <= myrr)
	}
	meanSqDiff=with(raceDriverDT, mean(sqDiff[isPredValid]))
	return(meanSqDiff)
}

OptimiseParam = function(modelinfo, raceDriverDT, OOSRaceDriverDT, QRRaceDriverDT, myRace) {
	itercount=0
	assign('itercount',itercount,env=globalenv())
	
	cat('About to obtain smoothing parameters via optimisation\n')
	if (modelinfo$qrtofit %in% c('q','r')) {
		maxinfo=optimise(LikFunct,
						interval=c(log(0.00001),log(0.1)),
						modelinfo=modelinfo,
						raceDriverDT=raceDriverDT,
						OOSRaceDriverDT=OOSRaceDriverDT,
						QRRaceDriverDT=QRRaceDriverDT,
						myRace=myRace)
		maxtheta=maxinfo$min
	}
	if (modelinfo$qrtofit=='qr') {
		maxinfo=nlm2(LikFunct,
						p=c(log(c(0.01,0.01)),0),
						modelinfo=modelinfo,
						raceDriverDT=raceDriverDT,
						OOSRaceDriverDT = OOSRaceDriverDT,
						QRRaceDriverDT = QRRaceDriverDT,
						myRace=myRace)
		maxtheta=maxinfo$est
	}
	message('Maximum has been obtained, coefficients are:')
	ProcessCoef(maxtheta, modelinfo, display = TRUE)
	
	return(maxtheta)
}

CheckSmoothValidity = function(rddf, modelinfo) {
	stopWithError = FALSE
	if (modelinfo$useQual) {
		badRow = with(rddf, isQualFitValid &
									(near(modQualPredNValid, 0) | is.na(modQualPredNValid)))
		if (any(badRow)) stopWithError = TRUE
	}
	if (modelinfo$useRace) {
		badRow = with(rddf, isRaceFitValid &
									(near(racePredNValid, 0) | is.na(racePredNValid)))
		if (any(badRow)) stopWithError = TRUE
	}
	if (stopWithError) {
		stop('Some of the coefficients to smooth have weighting of zero or are NA, exiting...\n')
	}
}

RetrieveMaxTheta = function(myRace, modelinfo) {
	
	smoothmodelfile=MakeRaceFile(myRace, 'smoothmodel.csv')
	modelparamname = GetModelParamName()
	### so, has this model already been run?
	if (!file.exists(smoothmodelfile)) {
		alreadyDone = FALSE
		maxtheta = NULL
	}
	if (file.exists(smoothmodelfile)) {

		allcombo=ReadF1Data(smoothmodelfile, 'smoothing')
		allcombo$fileisdone = !is.na(allcombo$sqdiff)

		modelinfoAsRow = purrr::map_df(modelinfo, function(x) x)
		allcombo = indicate_overlapping_combination(
					allcombo,
					modelinfoAsRow,
					modelparamname,
					'isCurrentModel')
		
		alreadyDone = with(allcombo, fileisdone[isCurrentModel])
		if (!alreadyDone) {
			maxtheta = NULL
		}
		if (alreadyDone) {
			maxtheta=as.numeric(unlist(strsplit(as.character(with(allcombo, maxtheta[isCurrentModel])),split=',')))
		}
	}
	
	return(maxtheta)
}

GetSmooth=function(qrtofit, qrtopredict, fwbw, modelchoice, usecurrentrace=FALSE, usestretch, resetMaxTheta = FALSE, myRace) {
	
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

	maxtheta = RetrieveMaxTheta(myRace, modelinfo)
	if (is.null(maxtheta) | resetMaxTheta) {
		optparam = TRUE
	}
	if (!is.null(maxtheta) & !resetMaxTheta) {
		optparam = FALSE
		cat('Using previously saved parameter values for smoothing:\n')
	}

	if (optparam) {
		# maxtheta = OptimiseParam(rddf, modelinfo, myRace)
		maxtheta = OptimiseParam(modelinfo, raceDriverDT, OOSRaceDriverDT, QRRaceDriverDT, myRace)
	}
	
	### then generate the smooths to go with these
	#sminfo=fitbyrace(maxtheta, modelinfo=modelinfo, OOSRddf, runmode='fit')
	dum=CalculateSmoothRaceDriver(maxtheta, modelinfo, raceDriverDT, OOSRaceDriverDT, QRRaceDriverDT)
	sqdiff = LikFunct(maxtheta, modelinfo, raceDriverDT, OOSRaceDriverDT, QRRaceDriverDT, myRace)

	sminfo = list(smval = dum$smval, numobval = dum$numobval, sqdiff = sqdiff)
	
	return(list(smval=sminfo$smval,numobval=sminfo$numobval,optcoef=maxtheta,sqdiff=sminfo$sqdiff))
}

CheckValidModelInfo = function(modelinfo) {
	
	modelparamname = GetModelParamName()
	### check that you haven't chosen a nonsensical combination of choices
	allcombo=GenerateCombo(filterlist=NULL)
	modelinfoAsRow = purrr::map_df(modelinfo, function(x) x)
	modelinfoAsRow = indicate_overlapping_combination(
						modelinfoAsRow, allcombo, modelparamname, 'inAllCombo')
	if (!modelinfoAsRow$inAllCombo) {
		stop('That is a strange combination of choices, assume it\'s a mistake, exiting\n')
	}
	if (!modelinfo$fwbw %in% c('bw','fwbw')) stop('fwbw has to be fwbw or bw\n')
	if (!modelinfo$usestretch %in% c(FALSE,TRUE)) stop('usestretch has to be FALSE or TRUE\n')
}

GenerateCombo=function(filterlist) {
	### let's get in whatever we have done, don't complain about anything that's missing
	combolist=list(method = 'downweight', 
					qrtofit=c('q','r','qr'),
					qrtopredict=c('q','r'),
					usecurrentrace=FALSE,
					modelchoice=c('qual',4,30),
					usestretch=c(FALSE,TRUE),
					fwbw=c('bw','fwbw'))
	## however, there are some combinations that make no sense
	if (!is.null(filterlist)) {
		for (j in 1:length(filterlist)) {
			combolist[names(filterlist)[j]]=filterlist[j]
		}
	}
	allcombo=expand.grid(combolist,stringsAsFactors=F)
	### however, there are combinations within that that make no sense, so let's filter a little more
	getrid=NULL
	getrid[[1]]=with(allcombo,which(qrtofit=='q' & qrtopredict!='q'))
	getrid[[2]]=with(allcombo,which(qrtofit=='r' & qrtopredict!='r'))
	getrid[[3]]=with(allcombo,which(modelchoice=='qual' & (qrtopredict!='q' | qrtofit!='q')))
	getrid=unique(do.call(c,getrid))
	if (length(getrid)>0) allcombo=allcombo[-getrid,]
	rownames(allcombo)=1:nrow(allcombo)
	return(allcombo)
}

GetModelParamName = function() {
	# very small function but total pain not having it
	modelparamname=c('method', 'qrtofit', 'qrtopredict', 'usecurrentrace', 'modelchoice', 'usestretch', 'fwbw')
	return(modelparamname)
}

# awful name, should be something like 'smoothing.storeallcoef'
RunModel=function(myRace, filterlist=NULL, reset=F) {
	### have we already prepared a file for this? if not, make it now
	smoothmodelfile=MakeRaceFile(myRace, 'smoothmodel.csv')
	modelparamname=GetModelParamName()

	filtercombo=GenerateCombo(filterlist=filterlist)

	if (!file.exists(smoothmodelfile)) {
		allcombo=GenerateCombo(filterlist=NULL)

		allcombo$fileisdone = FALSE
		allcombo$maxtheta=allcombo$smoothparam=allcombo$sqdiff=rep(NA,nrow(allcombo))
	}
	
	if (file.exists(smoothmodelfile)) {
		allcombo=ReadF1Data(smoothmodelfile, 'smoothing')
		allcombo$fileisdone = !is.na(allcombo$sqdiff)
	}
		
	allcombo = indicate_overlapping_combination(
				allcombo,
				filtercombo,
				modelparamname,
				'isInFilter')
				
	modeltorun=with(allcombo, which(isInFilter & !fileisdone))
	
	if (length(modeltorun)==0) {
		print('No new models to run, exiting\n')
		return(NULL)
	}
	if (length(modeltorun)>0) {
		cat('Have got',length(modeltorun),'models to run...\n')
		for (j in modeltorun) {
			dum=GetSmooth(qrtofit=allcombo$qrtofit[j],
							qrtopredict=allcombo$qrtopredict[j],
							modelchoice=allcombo$modelchoice[j],
							usestretch=allcombo$usestretch[j],
							fwbw=allcombo$fwbw[j],
							myRace=myRace)
			allcombo$maxtheta[j]=paste(dum$optcoef,collapse=', ')
			cdum=ProcessCoef(dum$optcoef,modelinfo=allcombo[j,modelparamname])
			### now fill out the data frame
			allcombo$smoothparam[j]=paste(paste(names(cdum),round(unlist(cdum),6),sep=': '),collapse=', ')
			### we would also like the sqdiff
			allcombo$sqdiff[j]=dum$sqdiff
		}
	}
	# don't want to write anything that isn't either model parameters or maximum info
	maximumcolumnname = c('sqdiff', 'smoothparam', 'maxtheta')
	allcombo = allcombo %>%
				select(modelparamname, maximumcolumnname)
	write_csv(allcombo, path = smoothmodelfile)
}

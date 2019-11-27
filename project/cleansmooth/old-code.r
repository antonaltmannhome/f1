
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

	smoothByDriverTeam = mySmoothDF %>%
							group_by(driver, team) %>%
							summarise(numobval = sum(wgt),
										smval = sum(dCoef * wgt) / numobval)

	return(smoothByDriverTeam)
}

fitbyrace=function(theta, myRace, modelinfo, rddf, runmode='max') {
	
	myrr = with(raceDF, rr[race == myRace])
	### now we do the smoothing/timeseries for all races
	### let's have theta in  a nice form:
	mycoef=ProcessCoef(theta, modelinfo)
		
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
			rddf = subset_join(rddf,
								currentRaceSmooth,
								c('team', 'driver'),
								toFill)
		}
	}

	### now, are we stretching or not? if we are, need to multiply stretch coef back in here
	if (modelinfo$usestretch==1) {
		rddf$smval= with(rddf, smval * stretch)
	}
	
	### now, if we're doing 'fit' mode, our work is done
	if (runmode=='fit') {
		### we want to return the likelihood stat, but be careful to only calculate it over points that both the more picky qrtofit choice ('q' and 'r')
		sax=with(rddf,which(isvalidpred & rr<=myrr))
		sqdiff=with(rddf[sax,], mean( (numobForPredict * (smval - dCoefForPredict)^2)))
		retval=list(smval=rddf$smval,numobval=rddf$numobval,sqdiff=sqdiff)
	}
	if (runmode=='max') {
		### we now use our validpred vectors
		sax=with(rddf,which(isvalidpred & rr<=myrr))
		sqdiff=with(rddf[sax,], mean( (numobForPredict * (smval - dCoefForPredict)^2)))
		retval=sqdiff
		
		### update function count ans display if necessary
		assign('itercount',itercount+1,env=globalenv())
		if ( (itercount %% 10)==0) {
			cat('Have done',itercount,'iterations so far, sqdiff is',sqdiff,'...\n')
			cat('Latest thetas tried are:\n')
			dum=ProcessCoef(theta, modelinfo, display=T)
		}
		
	}
	
	return(retval)
}

OptimiseParam = function(rddf, modelinfo, myRace) {
	itercount=0
	assign('itercount',itercount,env=globalenv())
	
	cat('About to obtain smoothing parameters via optimisation\n')
	if (modelinfo$qrtofit %in% c('q','r')) {
		maxinfo=optimise(fitbyrace,interval=c(log(0.00001),log(0.1)), myRace=myRace, modelinfo=modelinfo, rddf=rddf, runmode='max')
		maxtheta=maxinfo$min
	}
	if (modelinfo$qrtofit=='qr') {
		maxinfo=nlm2(fitbyrace,p=c(log(c(0.01,0.01)),0), myRace=myRace, modelinfo=modelinfo, rddf=rddf, runmode='max')
		maxtheta=maxinfo$est
	}
	message('Maximum has been obtained, coefficients are:')
	ProcessCoef(maxtheta, modelinfo, display = TRUE)
	
	return(maxtheta)
}

AugmentOOSRddfWithModel = function(theta, modelinfo, rddf, OOSRddf) {
	mycoef=ProcessCoef(theta, modelinfo)
	
	OOSRddf[,c('dCoef', 'qualnumobwgtvec', 'racenumobwgtvec',
				'timeWgt', 'numobTimeWgt', 'wgt')] = NA
	if (modelinfo$usequal) {
		qualIndex = which(OOSRddf$qualRace == 'qual')
		OOSRddf$dCoef[qualIndex] = rddf$qualDCoefToSmooth[OOSRddf$rddfMap[qualIndex]]
		OOSRddf$qualnumobwgtvec[qualIndex] = rddf$qualnumobwgtvec[OOSRddf$rddfMap[qualIndex]]
		OOSRddf$timeWgt[qualIndex] =
			exp(-mycoef$qualtdwcoef*OOSRddf$daynumDelta[qualIndex])
		OOSRddf$numobTimeWgt[qualIndex] =
			with(OOSRddf[qualIndex,], timeWgt * qualnumobwgtvec)
		if (modelinfo$qrtofit == 'q') {
			OOSRddf$wgt[qualIndex] = OOSRddf$numobTimeWgt[qualIndex]
		}
		if (modelinfo$qrtofit == 'qr') {
			OOSRddf$wgt[qualIndex] =
				with(OOSRddf[qualIndex,], mycoef$qrmix * numobTimeWgt)
		}
	}
	if (modelinfo$userace) {
		raceIndex = which(OOSRddf$qualRace == 'race')
		OOSRddf$dCoef[raceIndex] = rddf$raceDCoefToSmooth[OOSRddf$rddfMap[raceIndex]]
		OOSRddf$racenumobwgtvec[raceIndex] = rddf$racenumobwgtvec[OOSRddf$rddfMap[raceIndex]]
		OOSRddf$timeWgt[raceIndex] =
			exp(-mycoef$racetdwcoef*OOSRddf$daynumDelta[raceIndex])
		OOSRddf$numobTimeWgt[raceIndex] =
			with(OOSRddf[raceIndex,], timeWgt * racenumobwgtvec)
		if (modelinfo$qrtofit == 'r') {
			OOSRddf$wgt[raceIndex] = OOSRddf$numobTimeWgt[raceIndex]
		}
		if (modelinfo$qrtofit == 'qr') {
			OOSRddf$wgt[raceIndex] =
				with(OOSRddf[raceIndex,], (1 - mycoef$qrmix) * numobTimeWgt)
		}
	}

	return(OOSRddf)
}

CalculateSmoothRddf = function(theta, modelinfo, rddf, OOSRddf) {
	
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

LikFunct = function(theta, modelinfo, rddf, OOSRddf, myRace) {
	rddf = CalculateSmoothRddf(theta, modelinfo, rddf, OOSRddf)
	myrr = with(raceDF, rr[race == myRace])
	sax=with(rddf,which(isvalidpred & rr<=myrr))
	meansqdiff=mean(rddf$sqDiff[sax])
	return(meansqdiff)
}

OptimiseParam = function(rddf, OOSRddf, modelinfo, myRace) {
	itercount=0
	assign('itercount',itercount,env=globalenv())
	
	cat('About to obtain smoothing parameters via optimisation\n')
	if (modelinfo$qrtofit %in% c('q','r')) {
		maxinfo=optimise(LikFunct,
						interval=c(log(0.00001),log(0.1)),
						modelinfo=modelinfo,
						rddf=rddf,
						OOSRddf=OOSRddf,
						myRace=myRace)
		maxtheta=maxinfo$min
	}
	if (modelinfo$qrtofit=='qr') {
		maxinfo=nlm2(LikFunct,
						p=c(log(c(0.01,0.01)),0),
						modelinfo=modelinfo,
						rddf=rddf,
						OOSRddf=OOSRddf,
						myRace=myRace)
		maxtheta=maxinfo$est
	}
	message('Maximum has been obtained, coefficients are:')
	ProcessCoef(maxtheta, modelinfo, display = TRUE)
	
	return(maxtheta)
}
		
		
				rddf$qualDCoefToSmooth=rddf$acQualDCoef
				rddf$raceDCoefToSmooth=rddf$normRaceDCoef
			}

			if (!modelinfo$usestretch) {
				rddf$qualDCoefToSmooth=rddf$normQualDCoef
				rddf$raceDCoefToSmooth=rddf$acRaceDCoef
			}
			if (modelinfo$usestretch) {
				rddf$qualDCoefToSmooth=rddf$normQualDCoef/rddf$stretch
				rddf$raceDCoefToSmooth=rddf$acRaceDCoef/rddf$stretch
			}
		}
		if (modelinfo$qrtopredict=='r') {
			rddf$acQualDCoef = with(rddf, qToRRescale * normQualDCoef)
			if (!modelinfo$usestretch) {
				rddf$qualDCoefToSmooth=rddf$acQualDCoef
				rddf$raceDCoefToSmooth=rddf$normRaceDCoef
			}
			if (modelinfo$usestretch) {
				rddf$qualDCoefToSmooth=rddf$acQualDCoef/rddf$stretch
				rddf$raceDCoefToSmooth=rddf$normRaceDCoef/rddf$stretch
			}
		}
	}

	
	if (modelinfo$qrtofit=='q') {
		if (!modelinfo$usestretch) rddf$qualDCoefToSmooth=rddf$normQualDCoef
		if (modelinfo$usestretch) rddf$qualDCoefToSmooth=rddf$normQualDCoef/rddf$stretch
	}
	if (modelinfo$qrtofit=='r') {
		if (!modelinfo$usestretch) rddf$raceDCoefToSmooth=rddf$normRaceDCoef
		if (modelinfo$usestretch) rddf$raceDCoefToSmooth=rddf$normRaceDCoef/rddf$stretch
	}
	if (modelinfo$qrtofit=='qr') {
		if (modelinfo$qrtopredict=='q') {
			rddf$acRaceDCoef=with(rddf, rToQRescale * normRaceDCoef)
			if (!modelinfo$usestretch) {
				rddf$qualDCoefToSmooth=rddf$normQualDCoef
				rddf$raceDCoefToSmooth=rddf$acRaceDCoef
			}
			if (modelinfo$usestretch) {
				rddf$qualDCoefToSmooth=rddf$normQualDCoef/rddf$stretch
				rddf$raceDCoefToSmooth=rddf$acRaceDCoef/rddf$stretch
			}
		}
		if (modelinfo$qrtopredict=='r') {
			rddf$acQualDCoef = with(rddf, qToRRescale * normQualDCoef)
			if (!modelinfo$usestretch) {
				rddf$qualDCoefToSmooth=rddf$acQualDCoef
				rddf$raceDCoefToSmooth=rddf$normRaceDCoef
			}
			if (modelinfo$usestretch) {
				rddf$qualDCoefToSmooth=rddf$acQualDCoef/rddf$stretch
				rddf$raceDCoefToSmooth=rddf$normRaceDCoef/rddf$stretch
			}
		}
	}

#### the 'stretchiness' of races is important. the concepts are developed in modelphase54, let's formalise it now into part of the update
### upgraded dec 15, want to make it use the new smoothing methods and also work for qual and model4

GetStretchCoef = function(modelchoice) {
	
	LoadAllData()
	
	userace = (modelchoice %in% c(4,30))

	## could we maybe display what columns are to be used? very hard to read code below
	## should make a functino that does things like: paste0('stretch', ToCamel(modelchoice))
	## then assign nice variable names all in one go with it, then use those within code
	
	doneStretchName = paste0('doneStretch', ToCamel(modelchoice))
	stretchName = paste0('stretch', ToCamel(modelchoice))
	isValidRaceName = paste0('isValidRace', ToCamel(modelchoice))
	
	### what we will need:
	
	raceDF = MakeIsValidQualifying(raceDF = raceDF,
								qualifyingSessionDF = qualifyingSessionDF) %>%
				mutate(isValidToFit = isValidQual)
	if (modelchoice != 'qual') {
		raceDF = raceDF %>%
					mutate(isValidToFit = isValidQual | get(isValidRaceName))
	}
	
	rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, modelchoice, 'qual')
	if (userace) {
		rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, modelchoice, 'race')
	}
	
	raceDF = raceDF %>%
				mutate(qualInterceptToUse = get(paste0('mod',ToCamel(modelchoice),'QualIntercept')))
	rddf = rddf %>%
				mutate(qualDCoefToUse = get(paste0('mod',ToCamel(modelchoice),'QualDCoef')))
	rddf = lazy_left_join(rddf, raceDF, 'race', 'isValidQual')
	
	if (userace) {
		raceDF = raceDF %>%
					mutate(raceInterceptToUse = get(paste0('mod',ToCamel(modelchoice),'Intercept')))
		rddf = rddf %>%
				mutate(raceDCoefToUse = get(paste0('mod',ToCamel(modelchoice),'DCoef')),
						racePredNValid = get(paste0('mod',ToCamel(modelchoice),'PredNValid')))
		rddf = lazy_left_join(rddf, raceDF, 'race', isValidRaceName)
		
	}
	
	
	### so what is slowness of race: here we divide the lap time by track perimeter
		
	raceDF$qavgsp=raceDF$qualInterceptToUse/raceDF$perim
	if (userace) raceDF$ravgsp=raceDF$raceInterceptToUse/raceDF$perim

	### a few dodgy qualifyings that skew things too much
	
	rddf$qualOverride=FALSE
	rddf$qualOverride[with(rddf,which(driver=='vliuzzi' & race=='2011australia'))]=TRUE
	rddf$qualOverride[with(rddf,which(driver=='nkarthikeyan' & race=='2011australia'))]=TRUE
	rddf$qualOverride[with(rddf,which(driver=='egutierrez' & race=='2014australia'))]=TRUE
	
	### let's get hold of some smoothed values
	if (modelchoice=='qual') {
		dum=f1smoothing::GetSmooth(qrToFit = 'q',
								qrToPredict = 'q',
								modelChoice = 'qual',
								fwbw = 'fwbw',
								useStretch = FALSE,
								smoothDCoefName = 'qsmDCoef')
		rddf = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'qsmDCoef')
	}
	if (modelchoice %in% c(4,30)) {
		dum=f1smoothing::GetSmooth(qrToFit = 'qr',
								qrToPredict = 'q',
								modelChoice = modelchoice,
								fwbw = 'fwbw',
								useStretch = FALSE,
								smoothDCoefName = 'qsmDCoef')
		rddf = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'qsmDCoef')
		dum=f1smoothing::GetSmooth(qrToFit = 'qr',
								qrToPredict = 'r',
								modelChoice = modelchoice,
								fwbw = 'fwbw',
								useStretch = FALSE,
								smoothDCoefName = 'rsmDCoef')
		rddf = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'rsmDCoef')
	}
	
	qualStrCoef = rddf %>%
					filter(modQualPredNValid>0 &
							!qualOverride & 
							isValidQual &
							!is.na(rddf$qsmDCoef)) %>%
					group_by(race) %>%
					summarise(topLine = sum(qsmDCoef * qualDCoefToUse),
								bottomLine = sum(qsmDCoef^2),
								qualStrCoef = topLine / bottomLine,
								qualNumob = sum(modQualPredNValid))
	qualStrCoef = complete(qualStrCoef, expand(raceDF, nesting(race)), fill = list(qualNumob = 0))
	raceDF = lazy_left_join(raceDF, qualStrCoef, 'race', c('qualStrCoef', 'qualNumob'))
	
	if (userace) {

		raceStrCoef = rddf %>%
						filter(racePredNValid > 0 &
								get(isValidRaceName) &
								!is.na(rddf$rsmDCoef)) %>%
						group_by(race) %>%
						summarise(topLine = sum(rsmDCoef * raceDCoefToUse),
									bottomLine = sum(rsmDCoef^2),
									raceStrCoef = topLine / bottomLine,
									raceNumob = sum(racePredNValid))
		raceStrCoef = complete(raceStrCoef, expand(raceDF, nesting(race)), fill = list(raceNumob = 0))
		raceDF = lazy_left_join(raceDF, raceStrCoef, 'race', c('raceStrCoef', 'raceNumob'))
		
		### sanity check:
		plot(raceDF$qualStrCoef,raceDF$raceStrCoef)
		text(raceDF$qualStrCoef,raceDF$raceStrCoef,raceDF$race,cex=0.6)
	}
	
	### some of these are better than others though - want to put less weight on races with wet/dry qualifying or races that we got horribly wrong
	
	### normalise it
	raceDF$nqualwgt=with(raceDF, qualNumob/mean(qualNumob[qualNumob > 0]))
	
	if (userace) {
		# stop('is racwgt exactly the same as raceNumob?')
		### but can use racemse too - no not sure we should, seems to adjust too much for normal deviations
		#rmeanwgt=1/sqrt(raceDF[,'racemsetouse'])
		#raceDF$racewgt=as.numeric(tapply(rddf$racePredNValid,rddf$rr,sum,na.rm=T))
		#raceDF$nracewgt=raceDF$racewgt/mean(raceDF$racewgt[raceDF[,'raceisvalidtouse']==1])
		raceDF = raceDF %>%
					mutate(nracewgt = raceNumob / mean(raceNumob[raceNumob > 0]))
	}
	
	### monaco is terrible generally for the races.
	### don't have a use for the qualifying one as things stand but the code is there if it becomes necessary
	
	### we don't trust the race coefs, they're too variable - but we can take advantage of them in order to fit a model on perimeter and slowness
	### but ignore monaco, bit of a special case
	qualimod=with(raceDF,lm(qualStrCoef~perim + qavgsp, weight=nqualwgt))
	if (!userace) {
		myqualimodelfit = predict(qualimod,raceDF %>% filter(isValidToFit))
		raceDF = raceDF %>%
					mutate_cond(isValidQual,
								!!stretchName := myqualimodelfit)
	}
	if (userace) {
		racemod = lm(raceStrCoef~perim + ravgsp, weight=nracewgt,
						data = raceDF %>% filter(get(isValidRaceName) & circuit!='monte carlo'))

		myqualimodelfit = predict(qualimod,raceDF %>% filter(isValidToFit))
		myracemodelfit = predict(racemod,raceDF %>% filter(isValidToFit))
		mymodelfit = rowMeans(cbind(myqualimodelfit,myracemodelfit),na.rm=T)
		raceDF = raceDF %>%
					mutate_cond(isValidToFit,
								!!stretchName := mymodelfit)
	}
	
	raceDF[,doneStretchName] = TRUE
	
	sqlLazyUpdate(raceDF %>% filter(isValidToFit), 'race', 'race', stretchName)
	sqlLazyUpdate(raceDF, 'race', 'race', doneStretchName)
	if (FALSE) {
		sqlUpdate_multikey('race',
						stretchName,
						'race',
						raceDF %>% filter(isValidToFit) %>% select(stretchName),
						raceDF %>% filter(isValidToFit) %>% select(race))
	sqlUpdate_multikey('race',
						doneStretchName,
						'race',
						raceDF %>% select(doneStretchName),
						raceDF %>% select(race))
	}
}

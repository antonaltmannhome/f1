
SetUpModel = function() {
  LoadAllData()
  lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE)
  message('Have processed gaps/overtakings/pit stop deltas etc')
  lbl = f1validity:::MakeIsRogue(lbl)
  lbl = f1validity:::MakeInTraffic(30, lbl)
  message('Have processed isRogue and inTraffic indicators')
  lbl = f1laptimelm:::MakePredSec(lbl, 30, adjustForCarProblem = TRUE)
  rddf = f1laptimelm:::MakeNormDriverCoef(rddf, raceDF, 30, 'race')
  message('Have processed predicted laps times and driver coefficients')
  lbl = lazy_left_join(lbl, raceDF, 'race', 'isValidRace30')

  assign('lbl', lbl, envir = globalenv())
  assign('rddf', rddf, envir = globalenv())
}

MakeIsValidQualifying = function(raceDF,
							qualifyingSessionDF) {
	dum = qualifyingSessionDF %>%
			group_by(race) %>%
			summarise(anyDrySession = any(!isWet))
	raceDF = lazy_left_join(raceDF, dum, 'race')
	raceDF$isValidQual = raceDF$anyDrySession
	return(raceDF)
}

SmoothAndStretch = function(modelChoice) {

	LoadAllData()
	mostRecentRace = raceDF$race[nrace]
	f1smoothing::RunModel(mostRecentRace,
					filterList = list(modelChoice = modelChoice,
										useStretch = FALSE,
										qrToPredict = c('q', 'r')))
	f1stretching::GetStretchCoef(modelChoice)
	LoadAllData()
	f1smoothing::RunModel(mostRecentRace,
					filterList = list(modelChoice = modelChoice,
										useStretch = TRUE,
										qrToPredict = c('q', 'r')))
}

MixQualifyingRaceSmoothDCoef=function(modelchoice, rddf, fwbw = 'bw', includeIntermediateColumn = FALSE) {

	# need to make the normalised driver coefs
	rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, modelchoice, 'race')
	rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, modelchoice, 'qual')

	predNValid=paste('mod',modelchoice,'PredNValid',sep='')
	raceDCoef=paste('mod',modelchoice,'DCoef',sep='')
	qualDCoef=paste('mod',modelchoice,'QualDCoef',sep='')

	### let's adjline correct the qualifying numbers
	rddf$isGoodQualAC = with(rddf, get(predNValid) > 5 & modQualPredNValid > 0)
	mod = lm(get(raceDCoef) ~ get(qualDCoef), data = rddf %>% filter(isGoodQualAC))
	rddf = rddf %>%
			mutate(acQualDCoef = coef(mod)[[1]] + coef(mod)[[2]] * get(qualDCoef))

	dum=f1smoothing::GetSmooth(qrToFit = 'qr',
						qrToPredict = 'r',
						modelChoice = modelchoice,
						fwbw = fwbw,
						useStretch = TRUE,
						smoothDCoefName = 'smoothedRaceDCoef',
						smoothWgtName = 'smoothedRaceNumob')

	rddf = left_join(rddf, dum$smoothDF, c('race', 'driver'))

	### just to avoid horrible NA problem
	rddf = rddf %>%
			mutate(acQualDCoef2 = ifelse(!is.na(acQualDCoef), acQualDCoef, -10E06),
					smoothedRaceDCoef2 = ifelse(!is.na(smoothedRaceDCoef), smoothedRaceDCoef, -10E06),
					### total pain, but we have to adjust the mixing coefs for NAs
					qualSmoothedRaceMixCoef = case_when(
									modQualPredNValid > 0 & !is.na(smoothedRaceDCoef) > 0 ~ 0.55,
									modQualPredNValid > 0 & is.na(smoothedRaceDCoef) ~ 1,
									near(modQualPredNValid, 0) & !is.na(smoothedRaceDCoef) ~ 0),
					qualSmoothedRaceDCoef = qualSmoothedRaceMixCoef * acQualDCoef2 +
												(1 - qualSmoothedRaceMixCoef) * smoothedRaceDCoef2)
	# but we don't want -10E06 whenever race and qualy are bad, wew want NA, so need to do this:
	rddf = mutate_cond(rddf,
						near(modQualPredNValid, 0) & is.na(smoothedRaceDCoef),
						qualSmoothedRaceDCoef = NA)

	if (!includeIntermediateColumn) {
		rddf = rddf %>%
				remove_column(c(raceDCoef, qualDCoef, 'isGoodQualAC', 'acQualDCoef',
								'smoothedRaceDCoef', 'smoothedRaceNumob', 'qualSmoothedRaceMixCoef'))
	}

	return(rddf)
}

CheckSensibleModel = function(modelchoice) {
  LoadAllData()
  rawDCoefName = paste0('mod', modelchoice, 'RawDCoef')
  predNValidName = paste0('mod', modelchoice, 'PredNValid')
  return( rddf %>%
           filter(race == raceDF$race[nrace]) %>%
           select(driver, rawDCoefName, predNValidName) %>%
           arrange(get(rawDCoefName)))
}

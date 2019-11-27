CleanOvertakingGlmCoef = function(overtakingGlm) {

	### store the coefs nicely
	coefType = rep(NA,length(coef(overtakingGlm)))
	coefType[grep('Intercept', names(coef(overtakingGlm)))] = 'intercept'
	coefType[grep('circuit', names(coef(overtakingGlm)))] = 'circuit'
	coefType[names(coef(overtakingGlm)) == 'overlap'] = 'overlap'
	coefType[names(coef(overtakingGlm)) == 'factor(is2010)TRUE'] = 'year2010'
	coefName = names(coef(overtakingGlm))
	coefName[coefType == 'circuit'] = gsub('^factor\\(circuit\\)', '', names(coef(overtakingGlm))[coefType == 'circuit'])
	coefName[coefType == 'year2010'] = 'year2010'
	coefValue = as.numeric(coef(overtakingGlm))
	### but add in line for default circuit
	missingCircuit = setdiff(overtakingGlm$xlevels[['factor(circuit)']], coefName[coefType == 'circuit'])
	coefType = c(coefType,'circuit')
	coefName = c(coefName,missingCircuit)
	coefValue = c(coefValue, 0)
	coefDF = tibble(coefName = coefName, coefType = coefType, mle = coefValue)
	coefDF = coefDF %>%
				arrange(coefType, coefValue)

	return(coefDF)
}

GotOvertakenCostFunct = function(theta, minimumTimeLost) {
	linearMix = invlogit(theta[1] * minimumTimeLost)
	LHIntercept = exp(theta[2])
	LHSlope = 0
	RHIntercept = exp(theta[3])
	RHSlope = exp(theta[4])
	gotOvertakenCost = (1 - linearMix) * (LHIntercept + LHSlope * minimumTimeLost) +
												linearMix * (RHIntercept + RHSlope * minimumTimeLost)

	return(gotOvertakenCost)
}

GotOvertakenCostLikelihood = function(theta, binnedGotOvertakenValue, runMode = 'max') {

  binnedGotOvertakenValue$fittedTimeLost = f1blockedovertakingmodel:::GotOvertakenCostFunct(theta, binnedGotOvertakenValue$minimumTimeLost)
	if (runMode == 'plot') {
	  with(binnedGotOvertakenValue,
  		plot(minimumTimeLost, actualTimeLost,
		                                   xlab = 'mean minimum time lost',
		                                   ylab = 'mean actual time lost',
		                                   main = 'mean time lost when overtaken,\ngiven minimum time lost'))
    with(binnedGotOvertakenValue, points(minimumTimeLost, fittedTimeLost, col = 'red'))
		toReturn = NULL
	}
	if (runMode == 'max') {
		sqDiff = with(binnedGotOvertakenValue, sum( numOb * (fittedTimeLost - actualTimeLost)^2))
		toReturn = sqDiff
	}
	return(toReturn)
}

BlockedMeanFunct = function(myOverlap, theta) {
	blockedMean = exp(theta[1]) *
				log(exp(theta[2]) +
							exp(exp(theta[3]) * myOverlap))
	return(blockedMean)
}

BlockedVarFunct = function(myOverlap, theta) {
	blockedVar = exp(theta[1]) +
			exp(theta[2]) * invlogit(exp(theta[3]) *
			(myOverlap - theta[4]))
	return(blockedVar)
}

BlockedMeanLikelihood = function(theta, binnedBlockedValue, runMode) {
  binnedBlockedValue$fittedMeanGapAtEOL =
    f1blockedovertakingmodel:::BlockedMeanFunct(binnedBlockedValue$meanOverlap, theta)
  if (runMode == 'plot') {
    with(binnedBlockedValue,
				plot(meanOverlap, meanGapAtEOL,
							xlab = 'mean overlap',
							ylab = 'mean of gap at EOL',
							main = 'mean of gap when blocked,\ngiven overlap'))
    with(binnedBlockedValue, points(meanOverlap, fittedMeanGapAtEOL, col = 'green'))
    toReturn = NULL
  }
  if (runMode == 'max') {
    sqDiff = with(binnedBlockedValue, sum( (fittedMeanGapAtEOL - meanGapAtEOL)^2))
    toReturn = sqDiff
  }
  return(toReturn)
}

BlockedVarLikelihood = function(theta, binnedBlockedValue, runMode) {
  binnedBlockedValue$fittedVarGapAtEOL =
      f1blockedovertakingmodel:::BlockedVarFunct(binnedBlockedValue$meanOverlap, theta)
  if (runMode == 'plot') {
    with(binnedBlockedValue,
				plot(meanOverlap, varGapAtEOL,
							xlab = 'mean overlap',
							ylab = 'var of gap at EOL',
							main = 'variance of gap when blocked,\ngiven overlap'))
    with(binnedBlockedValue, points(meanOverlap, fittedVarGapAtEOL, col = 'green'))
    toReturn = NULL
  }
  if (runMode == 'max') {
    sqDiff = with(binnedBlockedValue, sum( (fittedVarGapAtEOL - varGapAtEOL)^2))
    toReturn = sqDiff
  }
  return(toReturn)
}


ProcessBlockedOvertakingModel = function() {

  # NB this is a big function but it's all a bit interlinked, so probably not suitable for breaking down into smaller functions. e.g gotOvertakenCost relies on the overtaking model
  # also, we'd like to plot them at the end, bit fiddly to do that when everything is in a separate function
  LoadAllData()
  lbl = f1validity:::MakeGotOtBlockedIsGood(lbl)
  lbl = f1laptimelm:::MakePredSec(lbl, 30, adjustForCarProblem = TRUE)

  possibleOvertakingDF = f1data:::LoadPossibleOvertakingDF()

  ### and now we need to look at the revised code in old world
  possibleOvertakingDF$overlap = with(possibleOvertakingDF, timeElapsedDelta + predSecDelta)

  possibleOvertakingDF = lazy_left_join(possibleOvertakingDF,
  										lbl,
  										c('race', 'lap', 'driver1' = 'driver'),
  										c('isCarProblem', 'impsec', 'mod30PredSec')) %>%
  										rename(isCarProblem1 = isCarProblem,
  												impsec1 = impsec,
  												mod30PredSec1 = mod30PredSec) %>%
  						lazy_left_join(lbl,
  										c('race', 'lap', 'driver2' = 'driver'),
  										c('isCarProblem', 'impsec', 'mod30PredSec')) %>%
  										rename(isCarProblem2 = isCarProblem,
  												impsec2 = impsec,
  												mod30PredSec2 = mod30PredSec)

  possibleOvertakingDF$anyCarProblem = with(possibleOvertakingDF, isCarProblem1 | isCarProblem2)

  possibleOvertakingDF = possibleOvertakingDF %>%
  						mutate(secLoss1 = impsec1 - mod30PredSec1,
  								secLoss2 = impsec2 - mod30PredSec2)

  possibleOvertakingDF = lazy_left_join(possibleOvertakingDF,
  										raceDF,
  										'race',
  										c('circuit', 'year')) %>%
  						mutate(is2010 = (year == 2010))

  possibleOvertakingDF$isOTModelValid = with(possibleOvertakingDF, secLoss1 < 3 & !anyCarProblem)
  overtakingGlm = glm(gotOvertaken ~ overlap + factor(circuit) + factor(is2010),
  											family = binomial,
  											data = possibleOvertakingDF %>% filter(isOTModelValid))
  possibleOvertakingDF$overtakeProb = invlogit(predict(overtakingGlm, possibleOvertakingDF))

  overtakingCoefDF = f1blockedovertakingmodel:::CleanOvertakingGlmCoef(overtakingGlm)


  # what about when driver DOES overtake, what gap can he expect to have depending on overlap (and overtakeProb possibly)
  didOvertakeCost = with(possibleOvertakingDF, mean(secLoss2[!anyCarProblem & gotOvertaken]))
  # NB is makes a bit of a difference (0.11 sec) if you add in the secLoss < 3 condition, why is that?

  # now we get the cost of being overtaken. We can't have it as a constant, because you could simulate a lap time that is faster than that of the driver who has overtaken you - so we're going to model how much time you lose compared to the optimal time you could have done ie if you'd been just a nose behind the car that overtook you as you cross the line

  gotOvertakenDF = possibleOvertakingDF %>%
                    mutate(isValid = overtakeProb > 0.005 & !anyCarProblem) %>%
                    filter(isValid & gotOvertaken)

  gotOvertakenDF = gotOvertakenDF %>%
                    mutate(secLimit1 = impsec2 + timeElapsedDelta,
                            minimumTimeLost = mod30PredSec1 - secLimit1,
                            actualTimeLost = impsec1 - secLimit1)

  gotOvertakenDF$inBin = with(gotOvertakenDF,
                              cut(minimumTimeLost,
                                    seq(min(minimumTimeLost), max(minimumTimeLost), le = 21),
                             labels = FALSE,
                             include.lowest = TRUE))

  binnedGotOvertakenValue = gotOvertakenDF %>%
                              group_by(inBin) %>%
                              summarise(minimumTimeLost = mean(minimumTimeLost),
                                        actualTimeLost = mean(actualTimeLost),
                                        numOb = n())

  gotOvertakenCostMaxInfo = nlm(f1blockedovertakingmodel:::GotOvertakenCostLikelihood,
																	p = c(1, 0, 0, 0),
																	binnedGotOvertakenValue = binnedGotOvertakenValue,
																	stepmax = 0.5)
	gotOvertakenCostCoef = gotOvertakenCostMaxInfo$est
	# NB overall effect plotted at end of function


  ### next, we want to know what sort of lap time you should get if you didn't overtake the car in front

  blockedLbl = lbl %>% filter(isGoodBlocked)

  blockedLbl$inBin = cut(blockedLbl$overlap,
  												quantile(blockedLbl$overlap, pr = seq(0, 1, length = 21)),
  												labels = FALSE,
  												include.lowest = TRUE)
  binnedBlockedValue = blockedLbl %>%
                group_by(inBin) %>%
                summarise(meanOverlap = mean(overlap),
  												meanGapAtEOL = mean(impsec - secLimit),
                          varGapAtEOL = var(impsec - secLimit))

  blockedMeanMaxInfo = nlm(f1blockedovertakingmodel:::BlockedMeanLikelihood,
														binnedBlockedValue = binnedBlockedValue,
														p = c(0, 0, 0),
														runMode = 'max')
	blockedMeanCoef = blockedMeanMaxInfo$est

  blockedVarMaxInfo = nlm(f1blockedovertakingmodel:::BlockedVarLikelihood,
														binnedBlockedValue = binnedBlockedValue,
														p = c(log(0.3), log(0.2), 0, 0),
														runMode = 'max')
	blockedVarCoef = blockedVarMaxInfo$est

	# do a nice plot at the end to display all of our models
	resetpar()
	par(mfrow = c(2, 2))
	f1blockedovertakingmodel:::GotOvertakenCostLikelihood(gotOvertakenCostCoef, binnedGotOvertakenValue,
	                                                        runMode = 'plot')

	f1blockedovertakingmodel:::BlockedMeanLikelihood(blockedMeanCoef, binnedBlockedValue,
	                                                        runMode = 'plot')

	f1blockedovertakingmodel:::BlockedVarLikelihood(blockedVarCoef, binnedBlockedValue,
	                                                        runMode = 'plot')

	blockedOvertakingModelCoef = list(overtakingCoefDF = overtakingCoefDF,
      															didOvertakeCost = didOvertakeCost,
      															gotOvertakenCostCoef = gotOvertakenCostCoef,
      															blockedMeanCoef = blockedMeanCoef,
      															blockedVarCoef = blockedVarCoef)

	fileOut = MakeRaceFile(raceDF$race[nrace], 'blocked-overtaking-model-coef.dat')
	dput(file = fileOut, blockedOvertakingModelCoef)

	message('Have written latest blocked/overtaking coefs to disk')
}

GetBlockedOvertakingModelCoef = function() {
	fileIn = MakeRaceFile(raceDF$race[nrace], 'blocked-overtaking-model-coef.dat')
	blockedOvertakingModelCoef = dget(file = fileIn)

	return(blockedOvertakingModelCoef)
}

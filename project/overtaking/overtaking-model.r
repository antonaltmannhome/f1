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

overtakingCoefDF = CleanOvertakingGlmCoef(overtakingGlm)

# what about when driver DOES overtake, what gap can he expect to have depending on overlap (and overtakeProb possibly)
meanOvertakingCost = with(possibleOvertakingDF, mean(secLoss2[!anyCarProblem & gotOvertaken]))
# NB is makes a bit of a difference (0.11 sec) if you add in the secLoss < 3 condition, why is that?

# now we get the cost of being overtaken. We can't have it as a constant, because you could simulate a lap time that is faster than that of the driver who has overtaken you - so we're going to model how much time you lose compared to the optimal time you could have done ie if you'd been just a nose behind the car that overtook you as you cross the line

gotOvertakenDF = possibleOvertakingDF %>%
                  filter(gotOvertaken)
gotOvertakenDF$secLimit1 = with(gotOvertakenDF, impsec2 + timeElapsedDelta)

## NB we'll restrict to overtakings that seemed possible under normal conditions, we see effect of cars in trouble dominating things otherwise
gotOvertakenDF$isValid = with(gotOvertakenDF, overtakeProb > 0.005 & !anyCarProblem)

GotOvertakenCost = function(sparePace) {
	### seems to be you generally cross the line 1 second behind car in front - except if you can't keep up with that lap time, then you
	linearMix = invlogit(5 * sparePace)
	LHIntercept = 1
	LHSlope = 0
	RHIntercept = 1
	RHSlope = 1
	gotOvertakenCost = (1 - linearMix) * (LHIntercept + LHSlope * sparePace) +
	                    linearMix * (RHIntercept + RHSlope * sparePace)
	return(gotOvertakenCost)
}

# just display that in action - in old worl, we optimised that '5' in the first line, but there's no point, it just wants to be any reasonably large number - although the LH and RH intercepts could surely have been optimised, no reason why they should automatically be 1
with(gotOvertakenDF %>% filter(isValid),
      calibplot(mod30PredSec1 - secLimit1, impsec1 - secLimit1,
                main = 'got overtaken',
                xlab = 'spare pace',
                ylab= 'lap time - secLimit'))
curve(GotOvertakenCost(x), add = TRUE, col = 'green')

### next, we want to know what sort of lap time you should get if you didn't overtake the car in front

blockedLbl = lbl %>% filter(isGoodBlocked)

FollowMeanFunct = function(myOverlap, theta) {
	followMean = exp(theta[1]) *
				log(exp(theta[2]) +
							exp(exp(theta[3]) * myOverlap))
	return(followMean)
}

FollowVarFunct = function(myOverlap, theta) {
	followVar = exp(theta[1]) +
			exp(theta[2]) * invlogit(exp(theta[3]) *
			(myOverlap - theta[4]))
	return(followVar)
}

FindBlockParam = function(theta) {
	print(paste(theta, collapse = ','))
	blockedLbl$meanValue = FollowMeanFunct(blockedLbl$overlap, theta[1:3])
	blockedLbl$sdValue = FollowSDFunct(blockedLbl$overlap, theta[4:7])

	blockedLbl$secProb = with(blockedLbl, dgammaMuSigma(impsec - secLimit, meanValue, sdValue))
	blockedLbl$logLik = with(blockedLbl, log(secProb))

	meanLogLik = mean(blockedLbl$logLik)

	#print(meanLogLik)
	return(-meanLogLik)
}

nlm(FindBlockParam, p = rep(0, 7), stepmax = 0.5)
# getting code 5, that's not healthy and it looks like it would be healthy, so let's investigate that next
# problem seems to come when the driver sets much closer than you would expect, my function seems to force the probabilities close to zero far too low

# no, let's stick to previous plan, bin up the gaps and fit line of best fit

blockedLbl$inBin = cut(blockedLbl$overlap,
												quantile(blockedLbl$overlap, pr = seq(0, 1, length = 21)),
												labels = FALSE,
												include.lowest = TRUE)
binnedValue = blockedLbl %>%
              group_by(inBin) %>%
              summarise(meanOverlap = mean(overlap),
												meanGapAtEOL = mean(impsec - secLimit),
                        varGapAtEOL = var(impsec - secLimit))

FollowMeanLikelihood = function(theta, runMode) {
	binnedValue$fittedMeanGapAtEOL = FollowMeanFunct(binnedValue$meanOverlap, theta)
	if (runMode == 'plot') {
		with(binnedValue, plot(meanOverlap, meanGapAtEOL))
		with(binnedValue, points(meanOverlap, fittedMeanGapAtEOL, col = 'green'))
		toReturn = NULL
	}
	if (runMode == 'max') {
		sqDiff = with(binnedValue, sum( (fittedMeanGapAtEOL - meanGapAtEOL)^2))
		toReturn = sqDiff
	}
	return(toReturn)
}

followMeanMaxInfo = nlm(FollowMeanLikelihood, p = c(0, 0, 0), runMode = 'max')
FollowMeanLikelihood(followMeanMaxInfo$est, runMode = 'plot')

FollowVarLikelihood = function(theta, runMode) {
	binnedValue$fittedVarGapAtEOL = FollowVarFunct(binnedValue$meanOverlap, theta)
	if (runMode == 'plot') {
		with(binnedValue, plot(meanOverlap, varGapAtEOL))
		with(binnedValue, points(meanOverlap, fittedVarGapAtEOL, col = 'green'))
		toReturn = NULL
	}
	if (runMode == 'max') {
		sqDiff = with(binnedValue, sum( (fittedVarGapAtEOL - varGapAtEOL)^2))
		toReturn = sqDiff
	}
	return(toReturn)
}

followVarMaxInfo = nlm(FollowVarLikelihood, p = c(log(0.3), log(0.2), 0, 0), runMode = 'max')
FollowVarLikelihood(followVarMaxInfo$est, runMode = 'plot')

dgammaMuSigma=function(myImpsec, mu, sigma) {
	alphaparam = mu^2/sigma^2
	betaparam = mu/sigma^2
	secProb = dgamma(myImpsec, alphaparam, betaparam)
	### that occasionally simulates something ludicrous, cap it at mean+2sds
	return(secProb)
}


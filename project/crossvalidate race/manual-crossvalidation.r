
LoadAllData()

lbl = indicate_overlapping_combination(lbl,
										rddf %>%
										filter(mod30PredNValid >=5),
										c('race', 'driver'),
										'driverDidGEQ5Laps')

lbl$isGoodCV = with(lbl, isGood30 & driverDidGEQ5Laps)

# but htere's another problem, sometimes there are so few laps by one tyre in a race that the model can't be fit OOS. in fact it's not that rare. i think we need to have some sort of prioring maybe?
# don't think it's a good idea to get rid of races where a tyre hasn't done many laps, because that defeats the point of the thing we're trying to test
## which brings us full circle back to doing priors. need to understand all that code, but won't bother explaining it all, just use the main bullet point, e.g drop from multi slope to single slope

lblSampleInfo = lbl %>%
				filter(isGoodCV) %>%
				group_by(race, driver) %>%
				mutate(inSample = rep(sample(1:2), c(ceiling(n() / 2), floor(n() / 2)))) %>%
				ungroup()
lbl = lazy_left_join(lbl, lblSampleInfo, c('race', 'driver', 'lap'), 'inSample')

FitLm = function(myRace, mySample, modelName) {
	toInclude = with(lbl, which(race == myRace & inSample == mySample))
	if (modelName == 'fuelTyreTyreLap') {
		numTyre = length(unique(lbl$tyre[toInclude]))
		if (numTyre == 1) {
			mod = lm(sec ~ factor(driver) + fuel + tyreLap - 1,
						data = lbl[toInclude,])
		}
		if (numTyre > 1) {
			mod = lm(sec ~ factor(driver) + fuel + factor(tyre) * tyreLap - 1,
						data = lbl[toInclude,])
		}
	}
	if (modelName == 'fuelTyreCommonSlope') {
		numTyre = length(unique(lbl$tyre[toInclude]))
		if (numTyre == 1) {
			mod = lm(sec ~ factor(driver) + fuel + tyreLap - 1,
						data = lbl[toInclude,])
		}
		if (numTyre > 1) {
			mod = lm(sec ~ factor(driver) + fuel + factor(tyre) + tyreLap - 1,
						data = lbl[toInclude,])
		}
	}
	if (modelName == 'fuelTyreLap') {
		mod = lm(sec ~ factor(driver) + fuel + tyreLap - 1,
						data = lbl[toInclude,])
	}
	if (modelName == 'fuel') {
		mod = lm(sec ~ factor(driver) + fuel - 1,
						data = lbl[toInclude,])
	}
	return(mod)
}

MakeOOSPredSec = function(modelName) {
	inSample1Mod = purrr::map(with(raceDF, race[isValidRace30]),
								FitLm, mySample = 1, modelName = modelName)
	names(inSample1Mod) = with(raceDF, race[isValidRace30])
	inSample2Mod = purrr::map(with(raceDF, race[isValidRace30]),
								FitLm, mySample = 2, modelName = modelName)
	names(inSample2Mod) = with(raceDF, race[isValidRace30])

	lbl$OOSPredSec = NA
	for (ri in which(raceDF$isValidRace30)) {
		toPredict = with(lbl, which(rr == ri & inSample == 2))
		lbl$OOSPredSec[toPredict] = predict(inSample1Mod[[raceDF$race[ri]]], lbl[toPredict,])
		toPredict = with(lbl, which(rr == ri & inSample == 1))
		lbl$OOSPredSec[toPredict] = predict(inSample2Mod[[raceDF$race[ri]]], lbl[toPredict,])
	}

	return(lbl$OOSPredSec)
}

lbl$OOSPredSecFTT = MakeOOSPredSec('fuel')
with(lbl, mean( (sec - OOSPredSecFTT)[isGoodCV]^2))
# 0.6495908
lbl$OOSPredSecFTT = MakeOOSPredSec('fuelTyreLap')
with(lbl, mean( (sec - OOSPredSecFTT)[isGoodCV]^2))
# 0.4237865
lbl$OOSPredSecFTT = MakeOOSPredSec('fuelTyreCommonSlope')
with(lbl, mean( (sec - OOSPredSecFTT)[isGoodCV]^2))
# 0.4026394 # oooh that's interesting. makes model simpler, totally makes sense too
lbl$OOSPredSecFTT = MakeOOSPredSec('fuelTyreTyreLap')
with(lbl, mean( (sec - OOSPredSecFTT)[isGoodCV]^2))
# 0.4048666

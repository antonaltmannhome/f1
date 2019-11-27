### so it looks like validating via finishing position isn't a great idea. Let's try doing within race. I think it'll be quicker to start this again than try to root out the spaghetti code that had been done before. There might be a package that does it already in R?

library(DAAG)

LoadAllData()
lbl = f1validity:::MakeIsRogue(lbl)
lbl = f1validity:::MakeInTraffic(30, lbl)

sax = with(lbl, which(race == '2018abudhabi' & isGood30))

dum = suppressWarnings(cv.lm(data = lbl[sax,], form.lm = formula(sec ~ factor(driver) + fuel), m = 10))

### that's seriously temperamental. let's try glmnet instead

xmat = model.matrix(~factor(driver) + fuel + factor(tyre) * tyreLap - 1, data = lbl[sax,])
netmod = glmnet(xmat, lbl$sec[sax])
uglyCoef = coef(netmod, s = 0.00001)
netCoefDF = tibble(coefName = names(uglyCoef[,1]), coefValue = as.numeric(uglyCoef[,1]))
netCoefDF$isDriverCoef = grepl('factor.+driver', netCoefDF$coefName)
netCoefDF$coefName = gsub('^.+\\)', '', netCoefDF$coefName)
# then get rid of intercept, add to drivers
netCoefDF$coefValue[netCoefDF$isDriverCoef] = with(netCoefDF, coefValue[isDriverCoef] + coefValue[coefName == ''])
# get rid of intercept
netCoefDF = netCoefDF[netCoefDF$coefName != '',]

lmmod = lm(sec ~ factor(driver) + fuel + factor(tyre) * tyreLap - 1, data = lbl[sax,])
lmCoefDF = tibble(coefName = names(coef(lmmod)), coefValue = as.numeric(coef(lmmod)))
lmCoefDF$isDriverCoef = grepl('factor.+driver', lmCoefDF$coefName)
lmCoefDF$coefName = gsub('^.+\\)', '', lmCoefDF$coefName)

# ok so those are close. but can we look at the cross v predictions now?

lbl$cvpredsec = NA

sax = with(lbl, which(race == '2013belgium' & isGood30))

date()
for (i in 1:100) {
xmat = model.matrix(~factor(driver) + fuel + factor(tyre) * tyreLap - 1, data = lbl[sax,])
cvmod = cv.glmnet(xmat, lbl$sec[sax], nfolds = 10)

lbl$cvpredsec[sax] = predict.cv.glmnet(cvmod, xmat, s="lambda.min")
}
date()

# how long does that take to do? 0.06 for each race, that's good enough

# let's audition a few models:
# bullshit function. if a driver has only done 1 lap, the prediction is the lap time they set. whatever it's doing, it's clearly not right. let's switch to cv.glm from boot

sax = with(lbl, which(race == '2013belgium' & isGood30))
mod = glm(sec ~ factor(driver) + fuel + factor(tyre) * tyreLap - 1, data = lbl[sax,])

cvdum = cv.glm(lbl[sax,], mod)
# well that crashes when a drier only has 1 lap. which is good. so filter out such drivers, in fact let's insist on 5 clear laps

lbl = indicate_overlapping_combination(lbl,
										rddf %>%
										filter(mod30PredNValid >=5),
										c('race', 'driver'),
										'driverDidGEQ5Laps')

sax = with(lbl, which(race == '2013belgium' & isGood30 & driverDidGEQ5Laps))
mod = glm(sec ~ factor(driver) + fuel + factor(tyre) * tyreLap - 1, data = lbl[sax,])

cvdum = cv.glm(lbl[sax,], mod)$delta[1]
# delta is 0.187 for that

# ditch different tyre types
sax = with(lbl, which(race == '2013belgium' & isGood30 & driverDidGEQ5Laps))
mod = glm(sec ~ factor(driver) + fuel + tyreLap - 1, data = lbl[sax,])

cvdum = cv.glm(lbl[sax,], mod)$delta[1]
# 0.213

### just fuel
sax = with(lbl, which(race == '2013belgium' & isGood30 & driverDidGEQ5Laps))
mod = glm(sec ~ factor(driver) + fuel - 1, data = lbl[sax,])

cvdum = cv.glm(lbl[sax,], mod)$delta[1]
# 0.525

sax = with(lbl, which(race == '2013belgium' & isGood30 & driverDidGEQ5Laps))
mod = glm(sec ~ factor(driver) - 1, data = lbl[sax,])

cvdum = cv.glm(lbl[sax,], mod)$delta[1]
# 2.16

# ok, pretty clear for that race. but function's a bit of a pain to use, i want the actual predictions, how do i sum up cross v errors for all races?

library(lmvar)

LoadAllData()

lbl = indicate_overlapping_combination(lbl,
										rddf %>%
										filter(mod30PredNValid >=5),
										c('race', 'driver'),
										'driverDidGEQ5Laps')

lbl$isGoodCV = with(lbl, isGood30 & driverDidGEQ5Laps)

# no, lmvar is also shit. let's just do the bloody thing ourselves

### need to be care

# lbl$inSample = with(lbl, (isGood30 & driverDidGEQ5Laps) * sample(1:2, nrow(lbl), replace = TRUE))
# want to make sure every driver has equal as possible number of laps in both samples
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

# let's shove in the SC adjusted fuel and tyre wear now

### so we need one vector for 'how many laps behind safety car in this race until this point, which we will use for fuel
### and another for 'how many laps beind the safety car so far in this stint'

lbl = lbl %>%
		group_by(race, driver) %>%
		mutate(cumSafetyCarLapInRace = cumsum(isSafetyCar)) %>%
		ungroup()
lbl = lbl %>%
		group_by(race, driver, stint) %>%
		mutate(cumSafetyCarInStint = cumsum(isSafetyCar)) %>%
		ungroup()

# we want the reduction to apply globally


FitLmGivenSCAdjustment = function(ri, lbl) {
	toInclude = with(lbl, which(isGood30 & rr == ri))
	numTyre = length(unique(lbl$tyre[toInclude]))
	if (numTyre == 1) {
		mod = with(lbl[toInclude,], lm(sec ~ factor(driver) + fuel2 + tyreLap2))
	}
	if (numTyre > 1) {
		mod = with(lbl[toInclude,], lm(sec ~ factor(driver) + fuel2 + factor(tyre) * tyreLap2))
	}
	return(summary(mod)$sigma)
}

TrialAdjustmentValue = function(theta) {
	fuelAdjustment = theta[1]
	tyreAdjustment = theta[2]
	print(c(fuelAdjustment,tyreAdjustment))
	lbl$fuel2 = lbl$fuel - fuelAdjustment * lbl$cumSafetyCarLapInRace
	lbl$tyreLap2 = lbl$tyreLap - tyreAdjustment * lbl$cumSafetyCarInStint
	
	byRaceSigma=rep(NA,nrace)
	byRaceSigma[which(raceDF$isValidRace30)] =
		sapply(with(raceDF, rr[isValidRace30]), FitLmGivenSCAdjustment, lbl = lbl)
	meanSigma = mean(byRaceSigma[raceDF$isValidRace30])
	print(meanSigma)
	return(meanSigma)
}
maxInfo = nlm(TrialAdjustmentValue, p = c(0,0))
$estimate
[1] -0.03904568  0.72418944
# so 0 for tyres, but about 0.7 for fuel. 

### values it comes up with are basically 1 for tyres, and -0.64 for fuel
lbl$fuelSCAdjusted = lbl$fuel + 0.72*lbl$cumSafetyCarLapInRace


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

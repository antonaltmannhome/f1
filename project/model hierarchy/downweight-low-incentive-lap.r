### let's look into downweighting low incentive laps

source('project/model hierarchy/model-hierarchy-setup.r')


rdlFile = paste0(USERPATH, 'project/validate via finpos/race-driver-lap-simulation.csv')
raceDriverLapSimulation = read.csv(rdlFile, as.is = TRUE) %>%
							rename(race = racename,
									meanFinPos = mod34meanfinpos,
									modalFinPosProb = mod34modalfinposprob)

lbl = left_join(lbl, raceDriverLapSimulation, c('race', 'driver', 'lap'))

lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE)


FitFuelTyreTypeAndAgeModelByRace = function(myRace, isValidName, wgtColName) {
	
	myLbl = lbl %>%
			filter(race == myRace & get(isValidName)) %>%
			mutate(lapWeight = get(wgtColName))
	numTyre = length(unique(myLbl$tyre))
	if (numTyre == 1) {
		mod = lm(sec ~ factor(driver) + fuel + tyreLap - 1, weights = lapWeight, data = myLbl)
	}
	if (numTyre > 1) {
		mod = lm(sec ~ factor(driver) + fuel + factor(tyre) * tyreLap - 1, weights = lapWeight, data = myLbl)
	}
	driverCoefDF = tibble::enframe(coef(mod)[grep('factor\\(driver\\)', names(coef(mod)))],
									name = 'driver',
									value = 'rawDCoef')
	driverCoefDF$driver = gsub('.+\\)', '', driverCoefDF$driver)
	driverCoefDF$race = myRace
	
	# missingDriver = setdiff(with(myLbl, driver[isValid0]), driverCoefDF$driver)
	return(driverCoefDF)
}

lbl$lapWeight = 1
modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelTyreTypeAndAgeModelByRace, 'isGood30', 'lapWeight')
rddf$dCoef30 = NormaliseRawDCoef(modelDF)
rddf = SmoothAndCalculateSqDiff(rddf, 'dCoef30', 'mod30PredNValid', 'efp30', 'sqDiff30')

lbl$lapWeight = with(lbl, ifelse(modalFinPosProb < 0.95, 1, 0.1))
modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelTyreTypeAndAgeModelByRace, 'isGood30', 'lapWeight')
rddf$dCoefAdj = NormaliseRawDCoef(modelDF)
rddf = SmoothAndCalculateSqDiff(rddf, 'dCoefAdj', 'mod30PredNValid', 'efpAdj', 'sqDiffAdj')
# 3.980585 - best i've got so far

# what about laps in the (0.9, 0.95) range?
lbl = lbl %>%
		mutate(lapWeight = case_when(modalFinPosProb < 0.9 ~ 1,
									between(modalFinPosProb, 0.8, 0.95) ~ 0.8,
									modalFinPosProb > 0.95 ~ 0.1))
modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelTyreTypeAndAgeModelByRace, 'isGood30', 'lapWeight')
rddf$dCoefAdj = NormaliseRawDCoef(modelDF)
rddf = SmoothAndCalculateSqDiff(rddf, 'dCoefAdj', 'mod30PredNValid', 'efpAdj', 'sqDiffAdj')

# no nothing happening there. but what about adjusting for driers higher up the ranking?

lbl = lbl %>%
		mutate(lapWeight = case_when(modalFinPosProb < 0.95 ~ 1,
									modalFinPosProb > 0.95 & startRank <= 6 ~ 0.1,
									modalFinPosProb > 0.95 & startRank >= 7 ~ 0.5))
modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelTyreTypeAndAgeModelByRace, 'isGood30', 'lapWeight')
rddf$dCoefAdj = NormaliseRawDCoef(modelDF)
rddf = SmoothAndCalculateSqDiff(rddf, 'dCoefAdj', 'mod30PredNValid', 'efpAdj', 'sqDiffAdj')
# nope, nothing there either

lbl = lazy_left_join(lbl, raceDF, 'race', 'nlap')
lbl$isOverHalfWay = with(lbl, lap > nlap/2)
lbl = lbl %>%
		mutate(lapWeight = case_when(modalFinPosProb < 0.95 ~ 1,
									modalFinPosProb > 0.95 & isOverHalfWay ~ 0.1,
									modalFinPosProb > 0.95 & !isOverHalfWay ~ 0.5))
modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelTyreTypeAndAgeModelByRace, 'isGood30', 'lapWeight')
rddf$dCoefAdj = NormaliseRawDCoef(modelDF)
rddf = SmoothAndCalculateSqDiff(rddf, 'dCoefAdj', 'mod30PredNValid', 'efpAdj', 'sqDiffAdj')
# nothign there, although there aren't that many situations where someone's locked in but less than halfway

# that 0.95 was rather plucked out of the air and is yet another cutoff. let's see if we can smooth it at all

lbl$incWeight = cut(lbl$modalFinPosProb, br = c(-0.1, seq(0.9, 1, 0.01)), labels = FALSE)

FindCurve = function(theta) {
	#incTheta = c(1, invlogit(theta))
	incTheta = c(1, theta)
	print(incTheta)
	lbl = lbl %>%
		mutate(lapWeight = incTheta[incWeight])
	assign('lbl', lbl, env = globalenv())
	
	modelDF = purrr::map_df(with(raceDF, race[isValidRace30]), FitFuelTyreTypeAndAgeModelByRace, isValidName = 'isGood30', wgtColName = 'lapWeight')
	rddf$dCoefAdj = NormaliseRawDCoef(modelDF)
	assign('rddf', rddf, env = globalenv())
	
	mySmooth = f1smoothing:::GetSmooth(qrToFit = 'r',
										qrToPredict = 'rfinpos',
										useStretch = FALSE,
										fwbw = 'flat',
									customSmoothInfo = list(qualRace = 'race',
															dCoefName = 'dCoefAdj',
															predNValidName = 'mod30PredNValid'))
	
	return(mySmooth$sqDiff)
}

lbl$incWeight = cut(lbl$modalFinPosProb, br = c(-0.1, seq(0.9, 1, 0.01)), labels = FALSE)

maxInfo = nlm(FindCurve, p = rep(0, 10))

36.7485264 -10.8201548  -2.5456052  -0.2242453  -9.6691737 -32.3829290 -17.0162864  66.2194921 -76.7084505  -2.4383413

round(invlogit(maxInfo$est),3)
[1] 1.000 0.000 0.073 0.444 0.000 0.000 0.000 1.000 0.000 0.080

# well that's as clear as mud, great

# let's try a different set of boundaries then

lbl$incWeight = cut(lbl$modalFinPosProb, br = c(-0.1, seq(0.8, 0.95, 0.05), 0.99, 1.01), labels = FALSE)
numBin = length(unique(lbl$incWeight[!is.na(lbl$incWeight)])) - 1

maxInfo = nlminb(start = rep(0.5 , numBin), objective = FindCurve,
				upper = rep(1, numBin), lower = rep(0, numBin))
$`par`
[1] 0.00000000 1.00000000 1.00000000 0.41727456 0.07048131
# A tibble: 6 x 3
  incWeight meanProb       wgt
      <int>    <dbl>     <dbl>
1         1 0.421922 1        
2         2 0.826008 0        
3         3 0.875770 1        
4         4 0.926663 1        
5         5 0.973541 0.417275 
6         6 0.998867 0.0704813

## the first one is a bit discouraging. but otherwise, looks like a sensible curve
incTheta = c(1, maxInfo$par)
dum = lbl %>%
		group_by(incWeight) %>%
		filter(!is.na(modalFinPosProb)) %>%
		summarise(meanProb = mean(modalFinPosProb))
dum$wgt = incTheta

plot(dum$meanProb, dum$wgt)

# how about from 0.95 onwards we smoothly downweight to nothing

lbl$lapWeight = with(lbl, ifelse(modalFinPosProb < 0.95, 1, 1 - 20 * (modalFinPosProb - 0.95)))
lbl$lapWeight[which(lbl$lapWeight < 0.01)] = 0.01
lbl$lapWeight[which(lbl$lapWeight > 1)] = 1
lbl$lapWeight[is.na(lbl$lapWeight)] = 1

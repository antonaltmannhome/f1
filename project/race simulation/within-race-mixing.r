# let's find optimal mixture of qual/previous race smooth/actual race so far

# fir that, we need actual race so far

source('model code/model-startup.r')

rddf = f1simulation:::InitialisePreRaceCoef(rddf)
rddf$teamPriorDCoef = with(rddf, ifelse(!is.na(teamMateQualDCoef), teamMateQualDCoef, teamSmoothCoef))
rddf = rddf %>%
        rename(qualDCoef = preStretchQualDCoef,
                smoothRaceDCoef = smoothDCoef)
lbl = f1laptimelm:::CalculateFuelTyreEffect(lbl, 30)
lbl = lazy_left_join(lbl, raceDF, 'race', 'mod30Intercept')

# let's do this slowly first, then worry about making it fast

cumMeanDCoef = lbl %>%
                group_by(race, driver) %>%
                arrange(lap) %>%
                mutate(nonNAFuelTyreEffect = ifelse(isGood30, fuelTyreEffect, -999),
                      cumSumNormDCoef = cumsum( (impsec - nonNAFuelTyreEffect) * isGood30),
                      cumSumGoodLap = cumsum(as.numeric(isGood30)),
                      cumMeanDCoef = cumSumNormDCoef / cumSumGoodLap,
                      legalMeanDCoef = lag(cumMeanDCoef),
                      cumSumLegalLap = lag(cumSumGoodLap)) %>%
                ungroup()
lbl = lazy_left_join(lbl, cumMeanDCoef, c('race', 'driver', 'lap'), c('cumSumLegalLap', 'legalMeanDCoef'))
lbl$cumSumLegalLap[which(lbl$lap == 1)] = 0

# now the mixing bit
# this is fiddly because we won't have everything we need so need to put in -99s and 0s
rddf = rddf %>%
        mutate(numQualDCoef = as.numeric(!is.na(qualDCoef)),
                numSmoothRaceDCoef = as.numeric(!is.na(smoothRaceDCoef)))
rddf$qualDCoef = with(rddf, ifelse(!is.na(qualDCoef), qualDCoef, -999))
rddf$smoothRaceDCoef = with(rddf, ifelse(!is.na(smoothRaceDCoef), smoothRaceDCoef, -999))
lbl = lazy_left_join(lbl,
                      rddf,
                     c('driver', 'race'),
                     c('teamPriorDCoef', 'qualDCoef', 'smoothRaceDCoef', 'numQualDCoef', 'numSmoothRaceDCoef'))
lbl = lbl %>%
        mutate(teamPriorPredictedSec = teamPriorDCoef + fuelTyreEffect + mod30Intercept,
               qualPredictedSec = qualDCoef + fuelTyreEffect + mod30Intercept,
               smoothRacePredictedSec = smoothRaceDCoef + fuelTyreEffect + mod30Intercept,
               currentRacePredictedSec = legalMeanDCoef + fuelTyreEffect)
lbl = lbl %>%
        mutate_cond(numQualDCoef == 0, qualPredictedSec = -999) %>%
        mutate_cond(numSmoothRaceDCoef == 0, smoothRacePredictedSec = -999) %>%
        mutate_cond(cumSumLegalLap == 0, currentRacePredictedSec = -999)

# useful line to check all is ok:
# View(lbl %>% filter(race == '2018australia' & driver=='lhamilton') %>% select(lap, impsec, fuelTyreEffect, isGood30, cumSumLegalLap, legalMeanDCoef, qualDCoef, numQualDCoef, smoothRaceDCoef, numSmoothRaceDCoef))
# View(lbl %>% filter(race == '2018australia' & driver=='lhamilton') %>% select(lap, impsec, fuelTyreEffect, isGood30, cumSumLegalLap, currentRacePredictedSec, qualPredictedSec, smoothRacePredictedSec))


lbl$usingPrior = with(lbl, numQualDCoef == 0 & numSmoothRaceDCoef == 0)

FindPriorWeight = function(theta, runMode = 'max') {
  teamPriorWeight = exp(theta[1])
  qualWeight = exp(theta[2])
  smoothRaceWeight = exp(theta[3])

  lbl$teamPriorWeight = ifelse(lbl$usingPrior, teamPriorWeight, 0)
  lbl$qualWeight = lbl$numQualDCoef * qualWeight
  lbl$smoothRaceWeight = lbl$numSmoothRaceDCoef * smoothRaceWeight

  lbl$priorTopLine = with(lbl, teamPriorWeight * teamPriorPredictedSec +
                                qualWeight * qualPredictedSec +
                                smoothRaceWeight * smoothRacePredictedSec +
                                cumSumLegalLap * currentRacePredictedSec)
  lbl$priorBottomLine = with(lbl, teamPriorWeight + qualWeight + smoothRaceWeight + cumSumLegalLap)
  lbl$predictedSec = with(lbl, priorTopLine / priorBottomLine)

  # View(lbl %>% filter(race == '2018australia' & driver=='lhamilton') %>% select(lap, impsec, isGood30, cumSumLegalLap, currentRacePredictedSec, qualPredictedSec, smoothRacePredictedSec, predictedSec))
  # View(lbl %>% filter(race == '2016australia' & driver=='egutierrez') %>% select(lap, impsec, isGood30, cumSumLegalLap, currentRacePredictedSec, teamPriorPredictedSec, qualPredictedSec, smoothRacePredictedSec, predictedSec))

  if (runMode == 'max') {
    # then we measure how well it does on the valid laps
    lbl$isPredictValid = with(lbl, cumSumLegalLap > 0 & isGood30)
    lbl$sqDiff = with(lbl, (predictedSec - impsec) ^ 2)
    meanSqDiff = with(lbl, mean(sqDiff[isPredictValid]))

    toReturn = meanSqDiff
  }

  if (runMode == 'fit') {
    toReturn = lbl$predictedSec
  }

  return(toReturn)
}

maxInfo = nlm(FindPriorWeight, p = rep(log(5), 3))
# good god, qualy and smoothRace are each worth about 0.75 of a race lap. seriously?
# that either means the race model is awesome or that i've messed up with them

with(lbl[lbl$isGood30 & lbl$numQualDCoef > 0,], plot(qualPredictedSec, impsec, pch = '.'))
with(lbl[lbl$isGood30 & lbl$numSmoothRaceDCoef > 0,], plot(smoothRacePredictedSec, impsec, pch = '.'))
# doesn't look like i have though
# and when i set the contribution of race so far to 0, nothing looks particularly bad
# this function  is so fast, no need to store estimates i don't think

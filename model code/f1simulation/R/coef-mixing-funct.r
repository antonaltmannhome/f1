
AlignPreRaceCoefWithLbl = function(rddf, lbl) {

  lbl = lazy_left_join(lbl,
                        rddf,
                       c('driver', 'race'),
                       c('teamPriorDCoef', 'qualDCoef', 'smoothRaceDCoef', 'numQualDCoef', 'numSmoothRaceDCoef'))

  # but what about if there's a car problem? then the pre-race coefs aren't so good, just add on 1 second as a quick fudge

  # except when there's a car problem, qualDCoef and smoothRaceDCoef are not helpful any more
  # but only apply it at end of lap that car problem first occurred, because we didn't know they were going to have a carproblem at the start of the first lap it occurred
  lbl = lbl %>%
        group_by(race, driver) %>%
        arrange(lap) %>%
        mutate(isSOLCarProblem = isCarProblem & lag(isCarProblem, default = FALSE)) %>%
        ungroup()
  lbl = lbl %>%
          mutate_cond(isSOLCarProblem,
                      teamPriorDCoef = teamPriorDCoef + 1,
                      qualDCoef = qualDCoef + 1,
                      smoothRaceDCoef = smoothRaceDCoef + 1)

  return(lbl)
}

MakeSingleRaceInRunningData = function(myLbl) {
  cumMeanDCoef = myLbl %>%
                  group_by(driverCarProblem) %>%
                  arrange(lap) %>%
                  mutate(isGood = isGood30 | isCarProblemButGood30,
                        nonNAFuelTyreEffect = ifelse(isGood, fuelTyreEffect, -999),
                        fuelTyreAdjustedSec = impsec - nonNAFuelTyreEffect - mod30Intercept,
                        cumSumNormDCoef = cumsum( fuelTyreAdjustedSec * isGood * miscLapWeight),
                        cumSumGoodLap = cumsum(as.numeric(isGood) * miscLapWeight),
                        cumMeanDCoef = cumSumNormDCoef / cumSumGoodLap,
                        inRunningRaceDCoef = lag(cumMeanDCoef),
                        cumSumLegalLap = lag(cumSumGoodLap, default = 0)) %>%
                  ungroup()

  cumMeanDCoef = cumMeanDCoef %>%
                  mutate_cond(cumSumLegalLap == 0,
                              inRunningRaceDCoef = -999)
  myLbl = lazy_left_join(myLbl,
                          cumMeanDCoef,
                          c('driverCarProblem', 'lap'),
                          c('cumSumLegalLap', 'inRunningRaceDCoef'))

  return(myLbl)
}

PrepareLblForCoefMixing = function(lbl, useIncentiveWeightColumn = FALSE) {

  lbl = f1laptimelm:::CalculateFuelTyreEffect(lbl, 30)
  lbl = lazy_left_join(lbl, raceDF, 'race', 'mod30Intercept')

  # don't want to get potential confusion with whether incentiveWeight exists or not

  if (useIncentiveWeightColumn) {
    lbl$miscLapWeight = lbl$incentiveWeight
  }
  if (!useIncentiveWeightColumn) {
    lbl$miscLapWeight = 1
  }

  lbl$driverCarProblem = with(lbl, paste(driver, isCarProblem))

  return(lbl)
}

MakeWithinRacePredictedSec = function(dCoefSourceWeightCoef, myLbl, runMode = 'fit',
                                      includeIntermediateColumn = FALSE) {
  teamPriorWeight = exp(dCoefSourceWeightCoef[1])
  qualWeight = exp(dCoefSourceWeightCoef[2])
  smoothRaceWeight = exp(dCoefSourceWeightCoef[3])

  myLbl$usingPrior = with(myLbl, numQualDCoef == 0 & numSmoothRaceDCoef == 0)
  myLbl$teamPriorWeight = ifelse(myLbl$usingPrior, teamPriorWeight,0)
  myLbl$qualWeight = myLbl$numQualDCoef * qualWeight
  myLbl$smoothRaceWeight = myLbl$numSmoothRaceDCoef * smoothRaceWeight

  # except when there's a car problem, qualDCoef and smoothRaceDCoef are not helpful any more
  myLbl = myLbl %>%
          mutate_cond(isSOLCarProblem,
                      teamPriorWeight = 0.01 * usingPrior,
                      qualWeight = 0.01 * numQualDCoef,
                      smoothRaceWeight = 0.01 * numSmoothRaceDCoef)

  myLbl$priorTopLine = with(myLbl, teamPriorWeight * teamPriorDCoef +
                                qualWeight * qualDCoef +
                                smoothRaceWeight * smoothRaceDCoef +
                                cumSumLegalLap * inRunningRaceDCoef)
  myLbl$totalWeight = with(myLbl, teamPriorWeight + qualWeight + smoothRaceWeight + cumSumLegalLap)
  myLbl$updatingDCoef = with(myLbl, priorTopLine / totalWeight)
  # this is useful further down the line:

  # View(lbl %>% filter(race == '2018australia' & driver=='lhamilton') %>% select(lap, isGood30, cumSumLegalLap, teamPriorWeight, qualWeight, smoothRaceWeight))
  if (runMode == 'max') {
    # then we measure how well it does on the valid laps
    myLbl$predictedSec = with(myLbl,updatingDCoef + fuelTyreEffect + mod30Intercept)
    myLbl$isPredictValid = with(myLbl, cumSumLegalLap > 0 & isGood30)
    myLbl$sqDiff = with(myLbl, (predictedSec - impsec) ^ 2)
    meanSqDiff = with(myLbl, mean(sqDiff[isPredictValid]))

    toReturn = meanSqDiff
  }

  if (runMode == 'fit') {
    myLbl = within(myLbl, rm(priorTopLine))
    # you might or might not want to keep these ones
    toReturn = myLbl
  }

  return(toReturn)
}

FindOptimalWeightCoef = function(lbl) {
  maxInfo = nlm(MakeWithinRacePredictedSec, p = rep(log(5), 3), myLbl = lbl, runMode = 'max')
  message('Have obtained optimal weight coefs, they are:')
  message('team prior: ', round(exp(maxInfo$estimate[1]), 4))
  message('qualifying: ', round(exp(maxInfo$estimate[2]), 4))
  message('race smooth: ', round(exp(maxInfo$estimate[3]), 4))
  return(maxInfo$est)
}

CalculateVarForTotalWeight = function(theta, myTotalWeight) {
  exp(theta[1]) * exp(-exp(theta[2]) * myTotalWeight ^ exp(theta[3]))
}

.FindVarCurve = function(theta, deltaByLapWeight) {
  candidateVarCurve = f1simulation:::CalculateVarForTotalWeight(theta, deltaByLapWeight$meanTotalWeight)
  sqdiff=sum( (candidateVarCurve - deltaByLapWeight$meanCoefSqDiff) ^ 2)
  return(sqdiff)
}

FindOptimalVarByTotalWeightCoef = function(lbl) {
  # NB assumes you've already done PrepareLblForCoefMixing and MakeWithinRacePredictedSec
  lbl = lazy_left_join(lbl, rddf, c('driver', 'race'), 'mod30DCoef')
  # we want to know, how well does this predict the final coef
  # but is that the correct way to judge the reliability? it assumes that that final coef has zero variance
  totalWeightBreak = with(lbl[lbl$isGood30,], seq(min(totalWeight), max(totalWeight), le = 20))
  deltaByLapWeight = lbl %>%
                      filter(isGood30) %>%
                      mutate(totalWeightBin = cut(totalWeight,
                                                    breaks = totalWeightBreak,
                                                    include.lowest = TRUE,
                                                    label = FALSE)) %>%
                      group_by(totalWeightBin) %>%
                      summarise(meanTotalWeight = mean(totalWeight),
                                meanCoefSqDiff = mean((updatingDCoef - mod30DCoef) ^ 2))

  # we now fit a line of best fit through that
  maxInfo = nlm(f1simulation:::.FindVarCurve, p = c(log(0.15),log(0.1),0), deltaByLapWeight)

  varByTotalWeightCoef = maxInfo$estimate

  # overlay to check
  with(deltaByLapWeight, plot(meanTotalWeight, meanCoefSqDiff))
  curve(f1simulation:::CalculateVarForTotalWeight(varByTotalWeightCoef, x), add = TRUE)

  return(varByTotalWeightCoef)
}

OverrideFirstCarProblemLap = function(myLbl) {
  # but if you develop a car problem mid race, then the first lap, you should use whatever coef you had before hand, because at the start of the lap, you were not expecting the problem
  myLbl = myLbl %>%
            group_by(driver) %>%
            arrange(lap) %>%
            mutate(lagUpdatingDCoef = lag(updatingDCoef)) %>%
            mutate_cond(lap > 1 & isCarProblem & !isSOLCarProblem,
                        updatingDCoef = lagUpdatingDCoef) %>%
            ungroup()

  return(myLbl)
}

ProcessInRunningWeightModel = function() {

  SetUpModel()
  rddf = f1smoothing:::InitialisePreRaceCoef(rddf, 30)
  lbl = f1simulation:::AlignPreRaceCoefWithLbl(rddf, lbl)
  lbl = f1simulation:::PrepareLblForCoefMixing(lbl)

  lblList = lbl %>% split(lbl$race)
  lbl = purrr::map_df(lblList, f1simulation:::MakeSingleRaceInRunningData)

  dCoefSourceWeightCoef = f1simulation:::FindOptimalWeightCoef(lbl)
  lbl = f1simulation:::MakeWithinRacePredictedSec(dCoefSourceWeightCoef, lbl,
                                                 includeIntermediateColumn = TRUE)
  # but we want the variance of the estimates as well
  varByTotalWeightCoef = f1simulation:::FindOptimalVarByTotalWeightCoef(lbl)
  lbl$varOfUpdatingDCoef =
    f1simulation:::CalculateVarForTotalWeight(varByTotalWeightCoef, lbl$totalWeight)

  inRunningWeightCoefFile = MakeRaceFile(raceDF$race[nrace], 'in-running-weight-coef.dat')
  dput(file = inRunningWeightCoefFile,
        list(dCoefSourceWeightCoef = dCoefSourceWeightCoef,
              varByTotalWeightCoef = varByTotalWeightCoef))
}

GetInRunningWeightCoef = function() {

  fileIn = MakeRaceFile(raceDF$race[nrace], 'in-running-weight-coef.dat')
  inRunningWeightCoef = dget(fileIn)

  return(inRunningWeightCoef)
}

InitialisePreRaceCoef = function(rddf) {

  # first step, get hold of best pre-race driver coef
  dum = f1smoothing:::GetSmooth('qr', 'r', 'bw', useStretch = TRUE, modelChoice = 30)

  rddf = lazy_left_join(rddf, dum$rddf, c('race', 'driver'), 'preStretchQualDCoef')
  rddf = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'smoothDCoef')

  rddf = :GetTeamSmoothCoef(rddf)
  rddf = :GetTeamMateDCoef(rddf)

  return(rddf)
}

GetTeamSmoothCoef = function(rddf) {

  # there's a hierarchy of things we use: mixture of smoothDCoef and preStretchQualDCoef if available
  # then fall back on team-mate's pace
  # then fall back on guess based on previous season

  teamSmoothCoef = rddf %>%
    group_by(year, race, rr, team) %>%
    summarise(teamSmoothCoef = mean(smoothDCoef, na.rm = TRUE)) %>%
    ungroup()
  # the fiddly bit is, at the start of the season it's not defined, so we need to fudge that
  # actually, it's not including pace shown at the very last race of previous season - feel like that should be a possible addition to the smoothing. just the useCurrentRace option could be added, but obvs you don't optimise over it
  teamSmoothCoef = teamSmoothCoef %>%
    group_by(team) %>%
    arrange(rr) %>%
    mutate(isFirstRaceOfSeason = year != lag(year),
           teamSmoothCoef = ifelse(isFirstRaceOfSeason,
                                   lag(teamSmoothCoef),
                                   teamSmoothCoef))
  teamSmoothCoef = teamSmoothCoef %>%
    mutate_cond(year == 2010 &
                  team %in% c('caterham', 'marussia', 'hrt') &
                  race == '2010bahrain',
                teamSmoothCoef = 2)
  teamSmoothCoef = teamSmoothCoef %>%
    mutate_cond(year == 2016 &
                  team == 'haas' &
                  race == '2016australia',
                teamSmoothCoef = 0.2)
  rddf = lazy_left_join(rddf, teamSmoothCoef, c('rr','team'), 'teamSmoothCoef')

  return(rddf)
}

.CollectTeamMateCoef = function(myRddf) {
  myRddf = myRddf %>%
    group_by(team) %>%
    mutate(alloc = 1:n())
  tmMatchIndex = with(myRddf, match(paste(team, alloc), paste(team, 3 - alloc)))
  myRddf$teamMateQualDCoef = myRddf$preStretchQualDCoef[tmMatchIndex]
  myRddf = within(myRddf, rm(alloc))
  return(myRddf)
}

GetTeamMateDCoef = function(rddf) {

  teamMateCoef = rddf %>%
    group_by(race) %>%
    select(race, team, driver, preStretchQualDCoef) %>%
    do(:.CollectTeamMateCoef(.))

  rddf = lazy_left_join(rddf, teamMateCoef, c('race', 'driver'), 'teamMateQualDCoef')

  return(rddf)
}

PrepareLblForCoefMixing = function(lbl, useIncentiveWeightColumn = FALSE) {

  rddf = :InitialisePreRaceCoef(rddf)
  rddf$teamPriorDCoef = with(rddf, ifelse(!is.na(teamMateQualDCoef), teamMateQualDCoef, teamSmoothCoef))
  rddf = rddf %>%
          rename(qualDCoef = preStretchQualDCoef,
                  smoothRaceDCoef = smoothDCoef)
  lbl = f1laptimelm:::CalculateFuelTyreEffect(lbl, 30)
  lbl = lazy_left_join(lbl, raceDF, 'race', 'mod30Intercept')

  # this looks like making an extra column for no good reason, but it avoids an ifelse in a much more complicated place later on
  if (!useIncentiveWeightColumn) {
    lbl$extraLapWeight = 1
  }
  if (useIncentiveWeightColumn) {
    lbl$extraLapWeight = lbl$incentiveWeight
  }
  # let's do this slowly first, then worry about making it fast

  lbl$driverCarProblem = with(lbl, paste(driver, isCarProblem))
  cumMeanDCoef = lbl %>%
                  group_by(race, driverCarProblem) %>%
                  arrange(lap) %>%
                  mutate(isGood = isGood30 | isCarProblemButGood30,
                        nonNAFuelTyreEffect = ifelse(isGood, fuelTyreEffect, -999),
                        fuelTyreAdjustedSec = impsec - nonNAFuelTyreEffect,
                        cumSumNormDCoef = cumsum( fuelTyreAdjustedSec * isGood * extraLapWeight),
                        cumSumGoodLap = cumsum(as.numeric(isGood) * extraLapWeight),
                        cumMeanDCoef = cumSumNormDCoef / cumSumGoodLap,
                        legalMeanDCoef = lag(cumMeanDCoef, default = 0),
                        cumSumLegalLap = lag(cumSumGoodLap, default = 0)) %>%
                  ungroup()
  lbl = lazy_left_join(lbl,
                        cumMeanDCoef,
                       c('race', 'driverCarProblem', 'lap'),
                       c('cumSumLegalLap', 'legalMeanDCoef'))

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

  lbl$qualPredictedSec[which(lbl$numQualDCoef == 0)] = -999
  lbl$smoothRacePredictedSec[which(lbl$numSmoothRaceDCoef == 0)] = -999
  lbl$currentRacePredictedSec[which(lbl$cumSumLegalLap == 0)] = -999

  # useful line to check all is ok:
  # View(lbl %>% filter(race == '2018australia' & driver=='lhamilton') %>% select(lap, impsec, fuelTyreEffect, isGood30, cumSumLegalLap, legalMeanDCoef, qualDCoef, numQualDCoef, smoothRaceDCoef, numSmoothRaceDCoef))
  # View(lbl %>% filter(race == '2018australia' & driver=='lhamilton') %>% select(lap, impsec, fuelTyreEffect, isGood30, cumSumLegalLap, currentRacePredictedSec, qualPredictedSec, smoothRacePredictedSec))

  lbl$usingPrior = with(lbl, numQualDCoef == 0 & numSmoothRaceDCoef == 0)

  # lots of columns created, we'll try to clean up a bit, don't think you'll be needing these ever again:
  lbl = within(lbl, rm(teamPriorDCoef, qualDCoef, smoothRaceDCoef,
                          legalMeanDCoef, extraLapWeight))

  return(lbl)
}

MakeWithinRacePredictedSec = function(dCoefSourceWeightCoef, lbl, runMode = 'fit',
                                      includeIntermediateColumn = FALSE) {
  teamPriorWeight = exp(dCoefSourceWeightCoef[1])
  qualWeight = exp(dCoefSourceWeightCoef[2])
  smoothRaceWeight = exp(dCoefSourceWeightCoef[3])

  lbl$teamPriorWeight = ifelse(lbl$usingPrior, teamPriorWeight,0)
  lbl$qualWeight = lbl$numQualDCoef * qualWeight
  lbl$smoothRaceWeight = lbl$numSmoothRaceDCoef * smoothRaceWeight

  lbl$priorTopLine = with(lbl, teamPriorWeight * teamPriorPredictedSec +
                                qualWeight * qualPredictedSec +
                                smoothRaceWeight * smoothRacePredictedSec +
                                cumSumLegalLap * currentRacePredictedSec)
  lbl$totalWeight = with(lbl, teamPriorWeight + qualWeight + smoothRaceWeight + cumSumLegalLap)
  lbl$predictedSec = with(lbl, priorTopLine / totalWeight)
  # this is useful further down the line:
  lbl$updatingDCoef = with(lbl, predictedSec - fuelTyreEffect - mod30Intercept)

  # View(lbl %>% filter(race == '2018australia' & driver=='lhamilton') %>% select(lap, isGood30, cumSumLegalLap, teamPriorWeight, qualWeight, smoothRaceWeight))
  if (runMode == 'max') {
    # then we measure how well it does on the valid laps
    lbl$isPredictValid = with(lbl, cumSumLegalLap > 0 & isGood30)
    lbl$sqDiff = with(lbl, (predictedSec - impsec) ^ 2)
    meanSqDiff = with(lbl, mean(sqDiff[isPredictValid]))

    toReturn = meanSqDiff
  }

  if (runMode == 'fit') {
    lbl = within(lbl, rm(priorTopLine))
    # you might or might not want to keep these ones
    if (!includeIntermediateColumn) {
      lbl = within(lbl, rm(teamPriorPredictedSec, smoothRaceDCoef, teamPriorDCoef,
                           legalMeanDCoef))
    }
    toReturn = lbl
  }

  return(toReturn)
}

FindOptimalWeightCoef = function(lbl) {
  maxInfo = nlm(MakeWithinRacePredictedSec, p = rep(log(5), 3), lbl = lbl, runMode = 'max')
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
  candidateVarCurve = :CalculateVarForTotalWeight(theta, deltaByLapWeight$meanTotalWeight)
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
  maxInfo = nlm(:.FindVarCurve, p = c(log(0.15),log(0.1),0), deltaByLapWeight)

  varByTotalWeightCoef = maxInfo$estimate

  # overlay to check
  with(deltaByLapWeight, plot(meanTotalWeight, meanCoefSqDiff))
  curve(:CalculateVarForTotalWeight(varByTotalWeightCoef, x), add = TRUE)

  return(varByTotalWeightCoef)
}

SetUpUpdatingCoef = function(lbl) {
  lbl = :PrepareLblForCoefMixing(lbl)
  dCoefSourceWeightCoef = FindOptimalWeightCoef(lbl)
  lbl = MakeWithinRacePredictedSec(dCoefSourceWeightCoef, lbl,
                                                 includeIntermediateColumn = TRUE)
  # but we want the variance of the estimates as well
  varByTotalWeightCoef = :FindOptimalVarByTotalWeightCoef(lbl)
  lbl$varOfUpdatingDCoef =
    :CalculateVarForTotalWeight(varByTotalWeightCoef, lbl$totalWeight)

  return(list(lbl = lbl,
              dCoefSourceWeightCoef = dCoefSourceWeightCoef,
              varByTotalWeightCoef = varByTotalWeightCoef))
}

# no no, we are not done yet, we need to adjust for carproblem. eg vettel bahrain 2010 does not slow down when h gets his car problem
if (FALSE) {
  source('model code/model-startup.r')
  lbl = :PrepareLblForCoefMixing(lbl)
  weightCoef = FindOptimalWeightCoef(lbl)
  lbl = MakeWithinRacePredictedSec(weightCoef, lbl,
                                                 includeIntermediateColumn = TRUE)
  # but we want the variance of the estimates as well
  varByTotalWeightCoef = :FindOptimalVarByTotalWeightCoef(lbl)
  lbl$varOfUpdatingDCoef =
    :CalculateVarForTotalWeight(varByTotalWeightCoef, lbl$totalWeight)

  # View(lbl %>% filter(race == '2010bahrain' & driver=='svettel') %>% select(lap, impsec, fuelTyreEffect, isGood30, isCarProblemButGood30, predictedSec))
  # this all looks great. i'd just suggest adding a frundtion that lets you browse a driver like in that comment. it should somehow include the intermediate columns too, which shouldbe doable. might involve calling MakeWithinRacePredictedSec again but that's ok, it take no time to run
}

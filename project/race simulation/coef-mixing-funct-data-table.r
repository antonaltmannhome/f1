InitialisePreRaceCoef = function(rddf, lbl) {

  # first step, get hold of best pre-race driver coef
  dum = f1smoothing:::GetSmooth('qr', 'r', 'bw', useStretch = TRUE, modelChoice = 30)

  rddf = lazy_left_join(rddf, dum$rddf, c('race', 'driver'), 'preStretchQualDCoef')
  rddf = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'smoothDCoef')

  rddf = GetTeamSmoothCoef(rddf)
  rddf = GetTeamMateDCoef(rddf)

  rddf$teamPriorDCoef = with(rddf, ifelse(!is.na(teamMateQualDCoef), teamMateQualDCoef, teamSmoothCoef))

  rddf = rddf %>%
          rename(qualDCoef = preStretchQualDCoef,
                  smoothRaceDCoef = smoothDCoef)

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
    do(.CollectTeamMateCoef(.))

  rddf = lazy_left_join(rddf, teamMateCoef, c('race', 'driver'), 'teamMateQualDCoef')

  return(rddf)
}

MakeSingleRaceInRunningData = function(myLbl) {
  dplyrCumMeanDCoef = myLbl %>%
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
doit = function(myLbl) {
	myLbl$isGood = with(myLbl, isGood30 | isCarProblemButGood30)
	myLbl$nonNAFuelTyreEffect = with(myLbl, ifelse(isGood, fuelTyreEffect, -999))
    myLbl$fuelTyreAdjustedSec = with(myLbl, impsec - nonNAFuelTyreEffect - mod30Intercept)
	myLbl = data.table::setorder(myLbl, driverCarProblem, lap)
	# how do i get all four columns done at once. failing that could do them all separately and join but seems awkward
	cumMeanDCoef = myLbl[,.(cumSumNormDCoef = cumsum(fuelTyreAdjustedSec * isGood * miscLapWeight)),
							'driverCarProblem']
							
	cumMeanDCoef = myLbl[,c('cumSumNormDCoef', 'cumSumGoodLap') :=
							list(cumsum(fuelTyreAdjustedSec * isGood * miscLapWeight),
									cumsum(as.numeric(isGood) * miscLapWeight)),
									'driverCarProblem']
	cumMeanDCoef$cumMeanDCoef = with(cumMeanDCoef, cumSumNormDCoef / cumSumGoodLap)
	cumMeanDCoef = cumMeanDCoef[,c('inRunningRaceDCoef', 'cumSumLegalLap') :=
							list(lag(cumMeanDCoef),
									lag(cumSumGoodLap, default = 0)),
									'driverCarProblem']
	}
	# looks good so far! but no faster than dplyr...?
							
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
  candidateVarCurve = CalculateVarForTotalWeight(theta, deltaByLapWeight$meanTotalWeight)
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
  maxInfo = nlm(.FindVarCurve, p = c(log(0.15),log(0.1),0), deltaByLapWeight)

  varByTotalWeightCoef = maxInfo$estimate

  # overlay to check
  with(deltaByLapWeight, plot(meanTotalWeight, meanCoefSqDiff))
  curve(CalculateVarForTotalWeight(varByTotalWeightCoef, x), add = TRUE)

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

  lbl = InitialisePreRaceCoef(rddf, lbl)
  lbl = PrepareLblForCoefMixing(lbl)

  lblList = lbl %>% split(lbl$race)
  lbl = purrr::map_df(lblList, MakeSingleRaceInRunningData)

  dCoefSourceWeightCoef = FindOptimalWeightCoef(lbl)
  lbl = MakeWithinRacePredictedSec(dCoefSourceWeightCoef, lbl,
                                                 includeIntermediateColumn = TRUE)
  # but we want the variance of the estimates as well
  varByTotalWeightCoef = FindOptimalVarByTotalWeightCoef(lbl)
  lbl$varOfUpdatingDCoef =
    CalculateVarForTotalWeight(varByTotalWeightCoef, lbl$totalWeight)

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

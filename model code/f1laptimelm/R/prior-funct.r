MakeFuelPrior = function(modelchoice, lbl, raceDF, rddf) {

  # do one quick lm of everything, in order just to get fuel and tyre coefs

  lbl = lazy_left_join(lbl, rddf, c('race', 'driver'), 'team')
  lbl$dty = with(lbl, paste(driver, team, year, sep = '~'))
  lbl$raceTyre = with(lbl, paste(race, tyre, sep = '~'))

  modelLabel = f1data:::MakeModelToUseName(paste0('validity', modelchoice),
                    modelchoice)

  lbl$isGood = lbl %>% pull(modelLabel$isGood)

  raceDF$crudeFuelCoef = NA
  for (myYear in unYear) {
    mod = lm(sec ~ factor(dty) + fuel * factor(race) + tyreLap * factor(race),
              data = lbl %>% filter(isGood & year == myYear))
    # then we extract the fuel coefs from that
    fuelCoef = coef(mod)[grep('fuel', names(coef(mod)))]
    # the model has an 'intercept' fuel coef, then all others are differences from that one
    fuelCoef[2:length(fuelCoef)] = fuelCoef[2:length(fuelCoef)] + fuelCoef[1]
    names(fuelCoef) = mod$xlevels[['factor(race)']]
    raceDF$crudeFuelCoef[match(names(fuelCoef), raceDF$race)] = as.numeric(fuelCoef)
  }
  raceDF = left_join(raceDF,
                  lbl %>%
                    group_by(race) %>%
                    summarise(numIsGood = sum(isGood)),
                  'race')

  sax1013 = with(raceDF,which(year <= 2013))
  sax14plus = with(raceDF,which(year >= 2014))
  mod1013 = lm(crudeFuelCoef ~ perim, weights = sqrt(numIsGood), data = raceDF[sax1013,])
  mod14plus = lm(crudeFuelCoef ~ perim, weights = sqrt(numIsGood), data = raceDF[sax14plus,])
  raceDF$fuelPrior = rep(NA,nrace)
  raceDF$fuelPrior[sax1013] = predict(mod1013, data.frame(raceDF[sax1013,'perim']))
  raceDF$fuelPrior[sax14plus] = predict(mod14plus, data.frame(raceDF[sax14plus,'perim']))

  raceDF = within(raceDF, rm(numIsGood, crudeFuelCoef))

  return(raceDF)
}

GetCrudeYearlyCoef = function(rddf, myYear) {
  mod = lm(modQualRawDCoef ~ factor(race) + factor(dty) - 1,
                              data = rddf %>% filter(year == myYear))
  dtyCoef = coef(mod)[grep('factor\\(dty\\)', names(coef(mod)))]
  names(dtyCoef) = gsub('.+\\)', '', names(dtyCoef))
  # but of course sutil or whoever is missing, let's add them in
  dtyCoef = c(0, dtyCoef)
  names(dtyCoef)[1] = mod$xlevels[['factor(dty)']][1]
  offset = mean(dtyCoef)

  raceIntercept = coef(mod)[grep('factor\\(race\\)', names(coef(mod)))]
  names(raceIntercept) = gsub('.+\\)', '', names(raceIntercept))

  dtyCoef = dtyCoef - offset
  raceIntercept = raceIntercept + offset

  return(list(dtyCoef = dtyCoef,
              raceIntercept = raceIntercept))
}

.MakeTeamSummaryFromDTYCoef = function(dtyCoef) {
  dtyCoefDF = list_to_tibble(dtyCoef, 'coef', 'dty')
  dtyCoefDF$team = gsub('(^[a-z]+ )(.+)( [0-9]{4}$)', '\\2', dtyCoefDF$dty)
  teamCoefDF = dtyCoefDF %>%
                  group_by(team) %>%
                  summarise(teamCoef = mean(coef))

  return(teamCoefDF)
}

GetDriverPrior = function(rddf, myYear) {
  rddf$smoothDCoef = NA
  rddf$dty = with(rddf, paste(driver, team, year))

  dum = f1laptimelm:::GetCrudeYearlyCoef(rddf, myYear)
  rddf$normQualDCoef = rddf$modQualRawDCoef - dum$raceIntercept[rddf$race]
  rddf$dtyQualDCoef = dum$dtyCoef[rddf$dty]

  thisYearTeamCoefDF = f1laptimelm:::.MakeTeamSummaryFromDTYCoef(dum$dtyCoef)
  rddf$isCurrentYear = (rddf$year == myYear)
  rddf = subset_join(rddf,
                      thisYearTeamCoefDF,
                      'team',
                      isCurrentYear)

  # but we might not have either of those if eg qualy is wet for the first race of the season
  # so we need to use team's average in the previous season if that happens
  # remember, this really doesn't have to be brilliantly accurate, it only matters in the edgiest of edge cases

  rddf$lastYearTeamCoef = rep(-99, nrow(rddf))
  if (myYear == unYear[1]) {
    # do nothing, never needed
  }
  if (myYear > unYear[1]) {
    dum = f1laptimelm:::GetCrudeYearlyCoef(rddf, myYear - 1)
    lastYearTeamCoefDF = f1laptimelm:::.MakeTeamSummaryFromDTYCoef(dum$dtyCoef) %>%
                          rename(lastYearTeamCoef = teamCoef)
    rddf = subset_join(rddf,
                        lastYearTeamCoefDF,
                        'team',
                        isCurrentYear)
  }

  # so if the driver set a time, we use normQualDCoef, otherwise fall back on dtyQualDCoef
  rddf$preRaceDCoef = with(rddf, case_when(
                            !is.na(normQualDCoef) ~ normQualDCoef,
                      is.na(normQualDCoef) & !is.na(dtyQualDCoef) ~ dtyQualDCoef,
                  is.na(normQualDCoef) & is.na(dtyQualDCoef) & !is.na(teamCoef) ~ teamCoef,
                  is.na(normQualDCoef) & is.na(dtyQualDCoef) & is.na(teamCoef) ~ lastYearTeamCoef))

  # there could still be the odd NA, but that's fine

  return(rddf)
}

MakePriorDF = function(myRace, thisRaceLbl, thisRaceTyre, raceDF, rddf, priorScale, phase) {

  # need to have a row for every driver/carproblem combination which is a bit of a faff unfortunately
  driverPriorDF = thisRaceLbl %>%
    distinct(driver, isCarProblem, driverCarProblem)
  driverPriorDF = driverPriorDF %>%
    left_join(rddf %>%
                  filter(race == myRace) %>%
                  select(driver, preRaceDCoef) %>%
                  mutate(noDriverPrior = is.na(preRaceDCoef)) %>%
                  mutate_cond(noDriverPrior, preRaceDCoef = -99) %>%
                  rename(prior = preRaceDCoef),
                'driver') %>%
    mutate_cond(isCarProblem,
                prior = prior + 1) %>%
    mutate(priorScale = ifelse(!isCarProblem & !noDriverPrior, priorScale$driver, 0)) %>%
    remove_column(c('driver', 'isCarProblem'))

  fuelPriorDF = tibble(prior = with(raceDF, fuelPrior[race == myRace])) %>%
    mutate(priorScale = priorScale$fuel)

  tyrePriorDF = tibble(tyre = thisRaceTyre,
                         prior = 0,
                         priorScale = priorScale$tyre)
  if (phase == 1) {
    tyreLapPriorDF = tibble(tyre = 'tyre',
                           prior = 0.05,
                           priorScale = priorScale$tyreLap)
  }
  if (phase == 2) {
    tyreLapPriorDF = tibble(tyre = thisRaceTyre,
                            prior = 0.05,
                            priorScale = priorScale$tyreLap)
  }

  return(list(driver = driverPriorDF,
              fuel = fuelPriorDF,
              tyre = tyrePriorDF,
              tyreLap = tyreLapPriorDF))
}

ExtractCoefFromTheta = function(theta, priorList, phase, intercept = NULL) {

  numTyre = nrow(priorList$tyre)
  if (phase == 1) {
    numDriver = 0
    index = list(fuel = numDriver + 1,
                 tyre = (numDriver + 2):(numDriver + numTyre + 1),
                 tyreLap = (numDriver + numTyre + 2):(numDriver + numTyre + 2))
  }
  if (phase == 2) {
    numDriver = nrow(priorList$driver)
    index = list(fuel = numDriver + 1,
                 tyre = (numDriver + 2):(numDriver + numTyre),
                 tyreLap = (numDriver + numTyre + 1):(numDriver + 2 * numTyre))
    index$driver = 1:numDriver
  }

  if (phase == 1) {
    coef = list(fuel = theta[index$fuel],
                tyre = exp(theta[index$tyre]),
                tyreLap = theta[index$tyreLap])
  }

  if (phase == 2) {
    coef = list(driver = theta[index$driver],
                fuel = theta[index$fuel],
                tyre = c(intercept, intercept + theta[index$tyre]),
                tyreLap = theta[index$tyreLap])
  }

  return(coef)
}

FixedDriverLikFunct = function(theta, priorList, thisRaceLbl) {

  coef = f1laptimelm:::ExtractCoefFromTheta(theta, priorList, 1)

  thisRaceLbl$fuelCoef = coef$fuel
  thisRaceLbl$tyreCoef = coef$tyre[thisRaceLbl$tyreMap]
  thisRaceLbl$tyreLapCoef = coef$tyreLap

  thisRaceLbl$predSec = with(thisRaceLbl, driverPrior +
                                fuelCoef * fuel +
                                tyreCoef +
                                tyreLapCoef * tyreLap)

  dataSqDiff = with(thisRaceLbl, (sec - predSec)^2)
  sumDataSqDiff = sum(dataSqDiff)

  priorList$fuel$theta = coef$fuel
  priorList$tyreLap$theta = coef$tyreLap

  sumPriorSqDiff = with(priorList,
                        fuel$priorScale * (fuel$theta - fuel$prior)^2 +
                          sum( tyreLap$priorScale * (tyreLap$theta - tyreLap$prior)^2))

  sumSqDiff = sumDataSqDiff + sumPriorSqDiff

  return(sumSqDiff)
}

FixedDriverGradFunct = function(theta, priorList, thisRaceLbl) {

  coef = f1laptimelm:::ExtractCoefFromTheta(theta, priorList, 1)

  thisRaceLbl$fuelCoef = coef$fuel
  thisRaceLbl$tyreCoef = coef$tyre[thisRaceLbl$tyreMap]
  thisRaceLbl$tyreLapCoef = coef$tyreLap

  thisRaceLbl$predSec = with(thisRaceLbl, driverPrior +
                               fuelCoef * fuel +
                               tyreCoef +
                               tyreLapCoef * tyreLap)

  fuelCoefGrad = with(thisRaceLbl, 2 * sum(fuel * (predSec - sec)))
  tyreCoefGrad = with(thisRaceLbl, 2 * tapply(tyreCoef * (predSec - sec), tyreMap, sum))
  tyreLapCoefGrad = with(thisRaceLbl, 2 * sum(tyreLap * (predSec - sec)))

  # but then add on the prior stuff of course
  fuelCoefGrad = fuelCoefGrad + with(priorList$fuel, 2 * priorScale * (coef$fuel - prior))
  # tyreCoef is free, so isn't priored
  tyreLapCoefGrad = tyreLapCoefGrad + with(priorList$tyreLap, 2 * priorScale * (coef$tyreLap - prior))

  combinedGrad = c(fuelCoefGrad, tyreCoefGrad, tyreLapCoefGrad)

  return(combinedGrad)
}



LapTimeLikFunct = function(theta, priorList, thisRaceLbl, intercept) {

  coef = f1laptimelm:::ExtractCoefFromTheta(theta, priorList, 2, intercept)

  thisRaceLbl$fuelCoef = coef$fuel
  thisRaceLbl$driverCoef = coef$driver[thisRaceLbl$driverMap]
  thisRaceLbl$tyreCoef = coef$tyre[thisRaceLbl$tyreMap]
  thisRaceLbl$tyreLapCoef = coef$tyreLap[thisRaceLbl$tyreMap]

  thisRaceLbl$predSec = with(thisRaceLbl, driverCoef +
                               fuelCoef * fuel +
                               tyreCoef +
                               tyreLapCoef * tyreLap)

  dataSqDiff = with(thisRaceLbl, (sec - predSec)^2)
  sumDataSqDiff = sum(dataSqDiff)

  priorList$driver$theta = coef$driver
  priorList$fuel$theta = coef$fuel
  priorList$tyre$theta = coef$tyre
  priorList$tyreLap$theta = coef$tyreLap

  sumPriorSqDiff = with(priorList,
                        sum( driver$priorScale * (driver$theta - driver$prior)^2) +
                          fuel$priorScale * (fuel$theta - fuel$prior)^2 +
                          sum( tyre$priorScale[-1] * (tyre$theta[-1] - tyre$prior[-1])^2) +
                          sum( tyreLap$priorScale * (tyreLap$theta - tyreLap$prior)^2))

  sumSqDiff = sumDataSqDiff + sumPriorSqDiff

  #print(paste(round(theta, 3), collapse = ', '))
  #print(sumSqDiff)
  return(sumSqDiff)
}

LapTimeGradFunct = function(theta, priorList, thisRaceLbl, intercept) {
  coef = f1laptimelm:::ExtractCoefFromTheta(theta, priorList, 2, intercept)

  thisRaceLbl$fuelCoef = coef$fuel
  thisRaceLbl$driverCoef = coef$driver[thisRaceLbl$driverMap]
  thisRaceLbl$tyreCoef = coef$tyre[thisRaceLbl$tyreMap]
  thisRaceLbl$tyreLapCoef = coef$tyreLap[thisRaceLbl$tyreMap]

  thisRaceLbl$predSec = with(thisRaceLbl, driverCoef +
                               fuelCoef * fuel +
                               tyreCoef +
                               tyreLapCoef * tyreLap)
  driverCoefGrad = with(thisRaceLbl, 2 * tapply(predSec - sec, driverMap, sum))
  fuelCoefGrad = with(thisRaceLbl, 2 * sum(fuel * (predSec - sec)))
  tyreCoefGrad = with(thisRaceLbl, 2 * tapply(predSec - sec, tyreMap, sum))
  tyreLapCoefGrad = with(thisRaceLbl, 2 * tapply(tyreLap * (predSec - sec), tyreMap, sum))

  # but then add on the prior stuff of course
  driverCoefGrad = driverCoefGrad + with(priorList$driver, 2 * priorScale * (coef$driver - prior))
  fuelCoefGrad = fuelCoefGrad + with(priorList$fuel, 2 * priorScale * (coef$fuel - prior))
  tyreCoefGrad = tyreCoefGrad + with(priorList$tyre, 2 * priorScale * (coef$tyre - prior))
  tyreLapCoefGrad = tyreLapCoefGrad + with(priorList$tyreLap, 2 * priorScale * (coef$tyreLap - prior))

  tyreCoefGrad = tyreCoefGrad[-1]

  combinedGrad = c(driverCoefGrad, fuelCoefGrad, tyreCoefGrad, tyreLapCoefGrad)

  return(combinedGrad)
}

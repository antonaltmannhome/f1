
LoadAllData()

modelchoice = 4
rddf = f1smoothing:::InitialisePreRaceCoef(rddf, modelchoice)
lbl = f1validity:::MakeIsGoodPreValidRace(modelchoice, lbl, raceDF)

sax1013 = with(raceDF,which(year <= 2013))
sax14plus = with(raceDF,which(year >= 2014))
raceDF$modFuelCoef = raceDF %>% pull(paste0('mod', modelchoice, 'FuelCoef'))
mod1013 = lm(raceDF$modFuelCoef[sax1013] ~ raceDF$perim[sax1013])
mod14plus = lm(raceDF$modFuelCoef[sax14plus] ~ raceDF$perim[sax14plus])
raceDF$fuelPrior = rep(NA,nrace)
raceDF$fuelPrior[sax1013] = predict(mod1013, data.frame(raceDF[sax1013,'perim']))
raceDF$fuelPrior[sax14plus] = predict(mod14plus, data.frame(raceDF[sax14plus,'perim']))

MakePriorDF = function(myRace, thisRaceLbl, thisRaceTyre, priorScale) {

  # need to have a row for every driver/carproblem combination which is a bit of a faff unfortunately
  driverPriorDF = thisRaceLbl %>%
    distinct(driver, isCarProblem, driverCarProblem)
  driverPriorDF = driverPriorDF %>%
    left_join(rddf %>%
                  filter(race == myRace) %>%
                  select(driver, preRaceDCoef) %>%
                  rename(prior = preRaceDCoef),
                'driver') %>%
    mutate_cond(isCarProblem,
                prior = prior + 1) %>%
    mutate(priorScale = ifelse(!isCarProblem, priorScale$driver, 0)) %>%
    remove_column(c('driver', 'isCarProblem'))

  fuelPriorDF = tibble(prior = with(raceDF, fuelPrior[race == myRace])) %>%
    mutate(priorScale = priorScale$fuel)

  tyrePriorDF = tibble(tyre = thisRaceTyre,
                         prior = 0,
                         priorScale = priorScale$tyre)
  tyreLapPriorDF = tibble(tyre = thisRaceTyre,
                            prior = 0.05,
                            priorScale = priorScale$tyreLap)

  return(list(driver = driverPriorDF,
              fuel = fuelPriorDF,
              tyre = tyrePriorDF,
              tyreLap = tyreLapPriorDF))
}

ExtractCoefFromTheta = function(theta, priorList, phase) {

  numTyre = nrow(priorList$tyre)
  if (phase == 1) {
    numDriver = 0
  }
  if (phase == 2) {
    numDriver = nrow(priorList$driver)
  }
  index = list(fuel = numDriver + 1,
               tyre = (numDriver + 2):(numDriver + numTyre + 1),
               tyreLap = (numDriver + numTyre + 2):(numDriver + 2 * numTyre + 1))

  if (phase == 1) {
    coef = list(fuel = theta[index$fuel],
                tyre = exp(theta[index$tyre]),
                tyreLap = theta[index$tyreLap])
  }

  if (phase == 2) {
    index$driver = 1:numDriver
    coef = list(driver = theta[index$driver],
                fuel = theta[index$fuel],
                tyre = exp(theta[index$tyre]),
                tyreLap = theta[index$tyreLap])
  }

  return(coef)
}

FixedDriverLikFunct = function(theta, priorList, thisRaceLbl) {

  coef = ExtractCoefFromTheta(theta, priorList, 1)

  thisRaceLbl$fuelCoef = coef$fuel
  thisRaceLbl$tyreCoef = coef$tyre[thisRaceLbl$tyreMap]
  thisRaceLbl$tyreLapCoef = coef$tyreLap[thisRaceLbl$tyreMap]

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

LapTimeLikFunct = function(theta, priorList, thisRaceLbl) {

  coef = ExtractCoefFromTheta(theta, priorList, 2)

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
                          sum( tyre$priorScale * (tyre$theta - tyre$prior)^2) +
                          sum( tyreLap$priorScale * (tyreLap$theta - tyreLap$prior)^2))

  sumSqDiff = sumDataSqDiff + sumPriorSqDiff

  #print(paste(round(theta, 3), collapse = ', '))
  #print(sumSqDiff)
  return(sumSqDiff)
}

FitLapTimeWithPrior = function(myRace, modelchoice) {

  if (modelchoice == 4) {
    thisRaceLbl = lbl %>%
      filter(race == myRace & isGoodPreValidRace) %>%
      select(driver, fuel, tyre, tyreLap, sec) %>%
      mutate(isCarProblem = FALSE,
              driverCarProblem = paste(driver, 'FALSE'))
  }
  if (modelchoice == 30) {
    thisRaceLbl = lbl %>%
      filter(race == myRace & isGoodPreValidRace) %>%
      select(driver, isCarProblem, fuel, tyre, tyreLap, sec) %>%
      mutate(driverCarProblem = paste(driver, isCarProblem))
  }

  priorScale = list(driver = 0.1, tyre = 0.001, tyreLap = 50, fuel = 0.001)
  # priorScale = list(driver = 10, tyre = 100,tyreLap = 1000, fuel = 1000)

  thisRaceTyre = thisRaceLbl %>%
                  count(tyre) %>%
                  arrange(-n) %>%
                  pull(tyre)
  priorList = MakePriorDF(myRace, thisRaceLbl, thisRaceTyre, priorScale)

  thisRaceLbl$tyreMap = match(thisRaceLbl$tyre, priorList$tyre$tyre)
  thisRaceLbl$driverMap = match(thisRaceLbl$driverCarProblem, priorList$driver$driverCarProblem)

  thisRaceLbl = lazy_left_join(thisRaceLbl, priorList$driver, 'driverCarProblem', 'prior') %>%
                  rename(driverPrior = prior)
  theta = c(log(mean(thisRaceLbl$sec)), 0.05, 0.1)

  theta = c(0.05,
              rep(log(mean(thisRaceLbl$sec)), nrow(priorList$tyre)),
              rep(0.1, nrow(priorList$tyre)))

  phase1MaxInfo = nlm(FixedDriverLikFunct, p = theta,
                      priorList = priorList, thisRaceLbl = thisRaceLbl,
                      iterlim = 500,
                      hessian = TRUE)
  phase1Coef = ExtractCoefFromTheta(phase1MaxInfo$estimate, priorList, 1)
  priorList$tyre$prior = phase1Coef$tyre[1]

  if (FALSE) {
    # is this the same as
    mod = Bolstad::bayes.lm(sec ~ tyre * tyreLap + fuel,
                            data = thisRaceLbl,
                            offset = driverPrior)
    # no because you can't send offset to bayeslm
  }

  if (FALSE) {
    thisRaceTyre = thisRaceLbl %>% count(tyre) %>% arrange(-n) %>% pull(tyre)
    thisRaceLbl$tyreFactor = factor(thisRaceLbl$tyre, levels = thisRaceTyre)
    mod = lm(sec ~ tyreFactor * tyreLap + fuel, offset = driverPrior, data = thisRaceLbl)

    priorList$tyre$prior = coef(mod)[['(Intercept)']]
  }
  priorList$tyre$priorScale[priorList$tyre$tyre == thisRaceTyre[1]] = 10E3

  # right, now we move the intercept term into priorList
  # why are we doing this? it's because for the driver priors to be in the right location, they need to have the appropriate intercept. so, we obtain the intercept that is appropriate for the driver priors in phase 1, then effectively force that to be the intercept for phase 2. thus the driver priors are in the right location in phase 2
  # but we also obtain the tyreLap prior into the bargain by doing this, which is useful for the occasional race where e.g a driver does 20 laps, almost all in traffic, on a tyre that has very few clear laps on it thus its tyreLap coef is liable to be wild. e.g mexico 2018. that would be a problem for the simulations

  theta = c(rep(0, nrow(priorList$driver)),
            0.05,
            log(priorList$tyre$prior),
            rep(0.1, nrow(priorList$tyre)))

  # nlm is sturggling with the maximisation, think we need to switch to optim
  phase2MaxInfo = nlm(LapTimeLikFunct, p = theta,
                      priorList = priorList, thisRaceLbl = thisRaceLbl,
                      iterlim = 500,
                      stepmax = 0.5,
                      hessian = TRUE)
  phase2MaxCoef = ExtractCoefFromTheta(phase2MaxInfo$estimate, priorList, 2)
  tempStdError = sqrt(diag(solve(phase2MaxInfo$hessian)))
  phase2StandardError = ExtractCoefFromTheta(tempStdError, priorList, 2)

  driverCoefDF = tibble(driverCarProblem = priorList$driver$driverCarProblem,
                        coef = phase2MaxCoef$driver)
  # but we want the intercept added back in for now, which is silly but it would require a major overhaul to avoid
  driverCoefDF$coef = driverCoefDF$coef
  driverCoefDF$stdError = phase2StandardError$driver
  driverCoefDF = left_join(driverCoefDF,
                           thisRaceLbl %>% count(driverCarProblem),
                           'driverCarProblem')
  # the standard errors just seem way too big...about twice the size as what we see in the lm. maybe it's to do with not having an intercept
  # as long as we consider them in relative terms maybe it's not such a problem
  # we've yet to ever actually use it

  fuelCoef = phase2MaxCoef$fuel
  tyreCoefDF = tibble(tyre = priorList$tyre$tyre,
                      int = phase2MaxCoef$tyre,
                      slo = phase2MaxCoef$tyreLap)

  if (FALSE) {
    # compare these
    driverCoefDF %>% arrange(coef)
    # with this
    rddf %>% filter(race == myRace) %>%
      select(driver, mod4RawDCoef, mod4PredNValid) %>%
      arrange(mod4RawDCoef)
    # or this
    rddf %>% filter(race == myRace) %>%
      select(driver, mod30RawDCoef, mod30PredNValid) %>%
      arrange(mod30RawDCoef)
    #looks sensible, we might just be in business
    # no, something has gone wrong here. mexico 2018, sainz's estimate has changed totally
  }

  return(list(driverCoefDF = driverCoefDF,
                fuelCoef = fuelCoef,
                tyreCoefDF = tyreCoefDF))
}

# so we should loop through the races to find the biggest difference, check there are any nasty problems under the hood
# also, we should check that we don't get any ludicrous tyre slope coefs - don't see why we should, in a way that will actually matter, but maybe for sims that could matter.
# actually, it could happen couldn't it, if a driver does eg 30 laps on the tyre, only 3 of which were clear, then you could have dodgy slope coef which will screw up the sims completely

tyreCoefList = vector('list', nrace)
driverCoefList = vector('list', nrace)
for (ri in 1:nrace) {
  if (raceDF$isValidRace30[ri]) {
    dum = FitLapTimeWithPrior(raceDF$race[ri], modelchoice)
    tyreCoefList[[ri]] = dum$tyreCoefDF
    tyreCoefList[[ri]]$race = raceDF$race[ri]
    driverCoefList[[ri]] = dum$driverCoefDF
    driverCoefList[[ri]]$race = raceDF$race[ri]
  }
  message('Have calculated priors for ', raceDF$race[ri])
}

tyreCoefDF = bind_rows(tyreCoefList)
# worst example i can find in 0.4s a lap, so 8 seconds after 20 laps. that's reasonable, they are all ridiculous tyres
# A tibble: 3 x 4
# tyre          int      slo race
# <chr>       <dbl>    <dbl> <chr>
#   1 soft      97.9019 0.417442 2013china
# 2 hypersoft 79.2563 0.308007 2018mexico
# 3 hypersoft 97.4938 0.444732 2018abudhabi
# so it looks ok. next, let's take a look at biggest effect on driver coefse

driverCoefDF = bind_rows(driverCoefList)
driverCoefDF = driverCoefDF %>%
                tidyr:::separate(driverCarProblem, c('driver', 'carProblem'), convert = TRUE) %>%
                rename(priorDCoef = coef) %>%
                filter(!carProblem)

# we then join to rddf, then normalise
if (FALSE) {
  rddf = lazy_left_join(rddf,
                        driverCoefDF %>%
                          filter(!carProblem) %>%
                          rename(priorDCoef = coef) %>%
                          select(driver, priorDCoef),
                        'driver')
}
# ah no, that's not straightforward. let'#s try other direction
driverCoefDF = lazy_left_join(driverCoefDF, rddf, c('race', 'driver'), 'mod30RawDCoef')
meanMod30RawDCoef = driverCoefDF %>%
                  filter(!is.na(mod30RawDCoef) & !is.na(priorDCoef)) %>%
                  group_by(race) %>%
                  summarise(meanMod30RawDCoef = mean(mod30RawDCoef),
                            meanPriorDCoef = mean(priorDCoef))
driverCoefDF = left_join(driverCoefDF, meanMod30RawDCoef, 'race')
driverCoefDF = driverCoefDF %>%
                mutate(normPriorDCoef = priorDCoef + meanMod30RawDCoef - meanPriorDCoef)

# almost always agrees, onyl ones that disagree particularly are silly races
# i'm happy with this. let's use it

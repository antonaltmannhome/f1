## the summary file has become horribly bloated as always, let's put the required likelihood function and supporting functions here

LoadAllData()
dum = f1smoothing:::GetSmooth(qrToFit = 'qr', qrToPredict = 'r', fwbw = 'fwbw',
                              modelChoice = 30, useStretch = TRUE)
rddf = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'smoothDCoef')

lbl = f1validity:::MakeIsGoodPreValidRace(30, lbl, raceDF)

### so we need one vector for 'how many laps behind safety car in this race until this point, which we will use for fuel

lbl = lbl %>%
  group_by(race, driver) %>%
  mutate(cumSafetyCarLapInRace = cumsum(isSafetyCar)) %>%
  ungroup()
lbl = lbl %>%
  group_by(race, driver, stint) %>%
  mutate(cumSafetyCarLapInStint = cumsum(isSafetyCar)) %>%
  ungroup()

### values it comes up with are basically 1 for tyres, and -0.64 for fuel
lbl$fuel2 = lbl$fuel + 0.64 * lbl$cumSafetyCarLapInRace
lbl$tyreLap2 = lbl$tyreLap - lbl$cumSafetyCarLapInStint
message('Tyre lap adjustment looks suspicious, investigate it')

sax1013 = with(raceDF,which(year <= 2013))
sax14plus = with(raceDF,which(year >= 2014))
mod1013 = lm(raceDF$mod30FuelCoef[sax1013] ~ raceDF$perim[sax1013])
mod14plus = lm(raceDF$mod30FuelCoef[sax14plus] ~ raceDF$perim[sax14plus])
raceDF$fuelPrior = rep(NA,nrace)
raceDF$fuelPrior[sax1013] = predict(mod1013, data.frame(raceDF[sax1013,'perim']))
raceDF$fuelPrior[sax14plus] = predict(mod14plus, data.frame(raceDF[sax14plus,'perim']))

LapTimeLikFunct = function(theta, priorList, thisRaceLbl) {

  coef = ExtractCoefFromTheta(theta, priorList)

  thisRaceLbl$interceptCoef = coef$intercept
  thisRaceLbl$fuelCoef = coef$fuel
  thisRaceLbl$driverCoef = coef$driver[thisRaceLbl$driverMap]
  thisRaceLbl$tyreCoef = coef$tyre[thisRaceLbl$tyreMap]
  thisRaceLbl$tyreLapCoef = coef$tyreLap[thisRaceLbl$tyreMap]

  thisRaceLbl$predSec = with(thisRaceLbl, interceptCoef +
                                           driverCoef +
                                           fuelCoef * fuel2 +
                                           tyreCoef +
                                           tyreLapCoef * tyreLap2)

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

  return(sumSqDiff)
}

MakePriorDF = function(myRace, thisRaceLbl, priorScale, phase, phase1TyreLapCoef = NULL) {

    # need to have a row for every driver/carproblem combination which is a bit of a faff unfortunately
    driverPriorDF = thisRaceLbl %>%
                      distinct(driver, isCarProblem, driverCarProblem)
    driverPriorDF = driverPriorDF %>%
                    subset_join(rddf %>%
                                    filter(race == myRace & !is.na(smoothDCoef)) %>%
                                    select(driver, smoothDCoef) %>%
                                    rename(prior = smoothDCoef),
                                  'driver',
                                  !isCarProblem) %>%
                    mutate_cond(isCarProblem,
                                prior = -99) %>%
                    mutate(priorScale = ifelse(!isCarProblem, priorScale$driver, 0)) %>%
                    remove_column(c('driver', 'isCarProblem'))

    fuelPriorDF = tibble(prior = with(raceDF, fuelPrior[race == myRace])) %>%
                    mutate(priorScale = priorScale$fuel)

    if (phase == 1) {
      myTyrePrior = 0
      myTyreLapPrior = 0.1
      tyrePriorDF = tibble(tyre = 'tyre', prior = myTyrePrior, priorScale = priorScale$tyre)
      tyreLapPriorDF = tibble(tyre = 'tyre', prior = myTyreLapPrior, priorScale = priorScale$tyreLap)
    }

    if (phase == 2) {
      myTyre = raceTyreDF %>%
                filter(race == myRace) %>%
                pull(tyre)
      tyrePriorDF = tibble(tyre = myTyre,
                            prior = 0,
                            priorScale = priorScale$tyre)
      tyreLapPriorDF = tibble(tyre = myTyre,
                                prior = phase1TyreLapCoef,
                                priorScale = priorScale$tyreLap)
    }

    return(list(driver = driverPriorDF,
                fuel = fuelPriorDF,
                tyre = tyrePriorDF,
                tyreLap = tyreLapPriorDF))
}

ExtractCoefFromTheta = function(theta, priorList) {
  numDriver = nrow(priorList$driver)
  numTyre = nrow(priorList$tyre)
  index = list(intercept = 1,
                driver = 2:(numDriver + 1),
                fuel = numDriver + 2,
                tyre = (numDriver + 3):(numDriver + numTyre + 2),
                tyreLap = (numDriver + numTyre + 3):(numDriver + 2 * numTyre + 2))

  coef = list(intercept = theta[index$intercept],
              driver = theta[index$driver],
              fuel = theta[index$fuel],
              tyre = theta[index$tyre],
              tyreLap = theta[index$tyreLap])

  return(coef)
}

FitLapTimePriored = function(myRace) {

  thisRaceLbl = lbl %>%
                  filter(race == myRace & isGoodPreValidRace) %>%
                  select(driver, isCarProblem, fuel2, tyre, tyreLap2, sec) %>%
                  mutate(driverCarProblem = paste(driver, isCarProblem))
  priorScale = list(driver = 0.0001, tyre = 0.001, tyreLap = 0.001, fuel = 0.001)
  # priorScale = list(driver = 10, tyre = 100,tyreLap = 1000, fuel = 1000)

  priorList = MakePriorDF(myRace, thisRaceLbl, priorScale, 1)

  thisRaceLbl$driverMap = match(thisRaceLbl$driverCarProblem, priorList$driver$driverCarProblem)
  thisRaceLbl$tyreMap = 1

  theta = c(mean(thisRaceLbl$sec),
            rep(0, nrow(priorList$driver)),
            0.05, 0, 0.1)

  phase1MaxInfo = nlm(LapTimeLikFunct, p = theta,
                      priorList = priorList, thisRaceLbl = thisRaceLbl,
                      iterlim = 500)
  phase1TyreLapCoef = ExtractCoefFromTheta(phase1MaxInfo$estimate, priorList)$tyreLap

  # so all we want from that is in fact the tyreLap prior
  priorList = MakePriorDF(myRace, thisRaceLbl, priorScale, 2, phase1TyreLapCoef)
  thisRaceLbl$tyreMap = match(thisRaceLbl$tyre, priorList$tyre$tyre)

  theta = c(mean(thisRaceLbl$sec),
            rep(0, nrow(priorList$driver)),
            0.05,
            rep(0, nrow(priorList$tyre)),
            rep(0.1, nrow(priorList$tyre)))

  phase2MaxInfo = nlm(LapTimeLikFunct, p = theta,
                      priorList = priorList, thisRaceLbl = thisRaceLbl,
                      iterlim = 500,
                      hessian = TRUE)

  phase2MaxCoef = ExtractCoefFromTheta(phase2MaxInfo$estimate, priorList)
  tempStdError = sqrt(diag(solve(phase2MaxInfo$hessian)))
  phase2StandardError = ExtractCoefFromTheta(tempStdError, priorList)

  driverCoefDF = tibble(driverCarProblem = priorList$driver$driverCarProblem,
                        coef = phase2MaxCoef$driver)
  # but we want the intercept added back in for now, which is silly but it would require a major overhaul to avoid
  driverCoefDF$coef = driverCoefDF$coef + phase2MaxCoef$intercept
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

  return(list(driverCoefDF = driverCoefDF,
              fuelCoef = fuelCoef,
              tyreCoefDF = tyreCoefDF))
}

# no, the standard errors are all wrong...
# right, this is getting a bit too complicated. there's got to be a simpler way around this. how about, we relax the rule on when a tyre is invalid
# no, that looked to be a terrible idea when i tried it. this actualyl looks good. but just don't use it for standard errors
# think the standard error problem is due to it being so overparameterised. We want idenitifability, not prioring really, so how can we achieve that.

# so, valid races where we have a non-overlapping tyre:
raceTyreDF = lazy_left_join(raceTyreDF, raceDF, 'race', 'isValidRace30')
raceTyreDF %>%
  filter(totalOverlap30 == 0 & isValidRace30)
# it's only italy 2018. so how about, we change the rule so that it's only in the case of a non-overlapping tyre that we declare a tyre invalid
# make sure we rule out the correct tyre of course
# then in the case where that happens, have a separate function so that we can have predicted lap time for the relevant driver

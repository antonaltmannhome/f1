### let's experiment with setups, see if anything restores the standard erros to their true states

myRace = '2018abudhabi'

thisRaceLbl = lbl %>%
  filter(race == myRace & isGoodPreValidRace) %>%
  select(driver, isCarProblem, fuel2, tyre, tyreLap2, sec) %>%
  mutate(driverCarProblem = paste(driver, isCarProblem))
priorScale = list(driver = 0.1, tyre = 0.001, tyreLap = 0.001, fuel = 0.001)
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

# can we be a bit clever here? we can force the most common tyre intercept to be a certain thing surely
# but then that means the driver priors would not be in quite the right place. but it's the driver priors relative to each other that we actually care about


theta = c(rep(0, nrow(priorList$driver)),
          0.05,
          rep(log(meanSec), nrow(priorList$tyre)),
          rep(0.1, nrow(priorList$tyre)))

mod = lm(sec ~ fuel2 + tyre * tyreLap2 + driver - 1, data = thisRaceLbl)


ExtractCoefFromTheta2 = function(theta, priorList) {
  numDriver = nrow(priorList$driver)
  numTyre = nrow(priorList$tyre)
  index = list(driver = 1:(numDriver),
               fuel = numDriver + 1,
               tyre = (numDriver + 2):(numDriver + numTyre + 1),
               tyreLap = (numDriver + numTyre + 2):(numDriver + 2 * numTyre + 1))
  
  coef = list(driver = theta[index$driver],
              fuel = theta[index$fuel],
              tyre = exp(theta[index$tyre]),
              tyreLap = theta[index$tyreLap])
  
  return(coef)
}

LapTimeLikFunct2 = function(theta, priorList, thisRaceLbl) {
  
  coef = ExtractCoefFromTheta2(theta, priorList)
  
  thisRaceLbl$fuelCoef = coef$fuel
  thisRaceLbl$driverCoef = coef$driver[thisRaceLbl$driverMap]
  thisRaceLbl$tyreCoef = coef$tyre[thisRaceLbl$tyreMap]
  thisRaceLbl$tyreLapCoef = coef$tyreLap[thisRaceLbl$tyreMap]
  
  thisRaceLbl$predSec = with(thisRaceLbl, driverCoef +
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
                          sum( tyreLap$priorScale * (tyreLap$theta - tyreLap$prior)^2))
  
  sumSqDiff = sumDataSqDiff + sumPriorSqDiff
  
  return(sumSqDiff)
}

phase2MaxInfo = nlm(LapTimeLikFunct2, p = theta,
                    priorList = priorList, thisRaceLbl = thisRaceLbl,
                    iterlim = 500,
                    hessian = TRUE)
phase2MaxCoef = ExtractCoefFromTheta2(phase2MaxInfo$estimate, priorList)
tempStdError = sqrt(diag(solve(phase2MaxInfo$hessian)))
phase2StandardError = ExtractCoefFromTheta2(tempStdError, priorList)


# Bolstad::bayes.lm looks interesting, let's give it a go
# let's order the tyres by which was most commonly used
myLbl = thisRaceLbl
thisRaceTyre = myLbl %>% count(tyre) %>% arrange(-n) %>% pull(tyre)
myLbl$tyre = factor(myLbl$tyre, levels = thisRaceTyre)
mod = Bolstad::bayes.lm(sec ~ tyre * tyreLap2 + driverCarProblem + fuel2, data = myLbl)
# but we could try sending in a prior, let's try that - need to send drivers alphabetically first
driverPrior = priorList$driver %>% arrange(driverCarProblem) %>% pull(prior)
tyrePrior = c(mean(myLbl$sec), 0, 0)
tyreLapPrior = c(phase1TyreLapCoef, 0, 0)
fuelPrior = priorList$fuel$prior
# but have to put coefs in the order that bayeslm is expecting
priorVec = c(mean(myLbl$sec), driverPrior[-1], fuelPrior, tyrePrior[-1], tyreLapPrior)
priorVarVec = c(10E5, rep(5, length(driverPrior)-1), 10, rep(10E5, length(tyrePrior) - 1), rep(100, length(tyrePrior)))
priorVarMat = diag(priorVarVec)
mod = Bolstad::bayes.lm(sec ~ driverCarProblem + fuel2 + tyre * tyreLap2, data = myLbl,
                          prior = list(b0 = priorVec, V0 = priorVarMat))
# no this doesn't solve the problem, the first driver isn't listed which means none of the other driver priors make sense
# or do they, maybe we can do this, we just have to forget prioring the first driver
# no, the driver priors just don't work, the coefs in the model are always being compared to the first alphabetically listed driver

# what about, force the driver coefs to be the prior. then, estimate the tyre and fuel coefs. then have a stupendously strong prior on the main tyre coef...there should then be no degrees of freedom


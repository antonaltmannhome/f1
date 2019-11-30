SetUpModel()

## idea is, see how sensitive the mean lap time difference is to model assumptions

# so let's take eg hulk and ricciardo in 2019

myDriv1 = 'nhulkenberg'
myDriv2 = 'dricciardo'
myYear = 2019

CheckOne = function(myYear, myDriv1, myDriv2) {
  
  myLbl = lbl %>%
    filter(driver %in% c(myDriv1, myDriv2) & year == myYear & !isRogue)
  myLbl = f1laptimelm::CalculateFuelTyreEffect(myLbl, 30)
  
  colToSpread = c('sec', 'inTraffic', 'isCarProblem', 'fuelTyreEffect', 'modalFinPosProb', 'tyreLap')
  horizLbl = myLbl %>%
    select(race, lap, driver, colToSpread) %>%
    spread_multiple(keyCol = driver, sec, inTraffic, isCarProblem, fuelTyreEffect, modalFinPosProb, tyreLap)
  
  ## horrible names of course, so rename
  badName = paste0('driver', apply(expand.grid(c(myDriv1, myDriv2), colToSpread), 1, paste, collapse = '_'))
  goodName = apply(expand.grid(c(1, 2), colToSpread), 1, function(x) paste(rev(x), collapse = ''))
  
  for (j in 1:length(badName)) {
    names(horizLbl)[names(horizLbl) == badName[j]] = goodName[j]
  }
  
  # so, comparison 1, compare all laps, raw sec
  
  message('All laps:')
  print(horizLbl %>%
    filter(!is.na(sec1) & !is.na(sec2)) %>%
    summarise(mean(sec1 - sec2),
              numLap = n()))
  
  # now filter out laps where either driver had carproblem
  message('Exclude car problem laps:')
  print(horizLbl %>%
    filter(!is.na(sec1) & !is.na(sec2) &
             !isCarProblem1 & !isCarProblem2) %>%
    summarise(mean(sec1 - sec2),
              numLap = n()))
  
  message('Exclude traffic laps:')
  # then, filter out traffic laps
  print(horizLbl %>%
    filter(!is.na(sec1) & !is.na(sec2) &
             !isCarProblem1 & !isCarProblem2 &
             !inTraffic1 & !inTraffic2) %>%
    summarise(mean(sec1 - sec2),
              numLap = n()))
  
  message('Exclude dead rubber laps:')
  # then, filter out traffic laps
  print(horizLbl %>%
          filter(!is.na(sec1) & !is.na(sec2) &
                   !isCarProblem1 & !isCarProblem2 &
                   !inTraffic1 & !inTraffic2 &
                   modalFinPosProb1 < 0.99 & modalFinPosProb2 < 0.99) %>%
          summarise(mean(sec1 - sec2),
                    numLap = n()))
  
  # and what if we take away fuel and tyre effect?
  message('Adjust for fuel and tyres:')
  print(horizLbl %>%
    filter(!is.na(sec1) & !is.na(sec2) &
             !isCarProblem1 & !isCarProblem2 &
             !inTraffic1 & !inTraffic2 &
             modalFinPosProb1 < 0.99 & modalFinPosProb2 < 0.99) %>%
    summarise(mean(sec1 - fuelTyreEffect1 - (sec2 - fuelTyreEffect2)),
              numLap = n(),
              mean(tyreLap1) - mean(tyreLap2)))
}

# yikes, that changes it quite a lot for hulk/ric. that's a little unusual though actually
# this is cool though. next steps:
# exclude worst race for each driver
# include all non-blocked laps
# big job: out of sample prediction, what works best?

lbl = f1laptimelm::CalculateFuelTyreEffect(lbl, 30)
lbl = lazy_left_join(lbl, rddf, c('race', 'driver'), 'team')

CheckTwo = function(myYear, myDriv1, myDriv2, myTeam) {
  
  # same as above but make presentation more attractive
  myLbl = lbl %>%
    filter(driver %in% c(myDriv1, myDriv2) & year == myYear & !isRogue & team == myTeam)
  
  colToSpread = c('sec', 'inTraffic', 'isCarProblem', 'fuelTyreEffect', 'modalFinPosProb', 'tyreLap')
  horizLbl = myLbl %>%
    select(race, lap, driver, colToSpread) %>%
    spread_multiple(keyCol = driver, sec, inTraffic, isCarProblem, fuelTyreEffect, modalFinPosProb, tyreLap)
  
  ## horrible names of course, so rename
  badName = paste0('driver', apply(expand.grid(c(myDriv1, myDriv2), colToSpread), 1, paste, collapse = '_'))
  goodName = apply(expand.grid(c(1, 2), colToSpread), 1, function(x) paste(rev(x), collapse = ''))
  
  for (j in 1:length(badName)) {
    names(horizLbl)[names(horizLbl) == badName[j]] = goodName[j]
  }
  
  # so, comparison 1, compare all laps, raw sec

  numComparison = 5
  compName = c('all laps', 'exclude car problem', 'exclude traffic', 'exclude dead rubber', 'adjust for model')
  deltaVec = rep(NA, numComparison)
  numObVec = rep(NA, numComparison)
  
  for (i in 1:numComparison) {
    if (i == 1) {
      validHorizLbl = horizLbl %>%
                      filter(!is.na(sec1) & !is.na(sec2))
    }
    if (i == 2) {
      validHorizLbl = horizLbl %>%
                      filter(!is.na(sec1) & !is.na(sec2) &
                             !isCarProblem1 & !isCarProblem2)
    }
    if (i == 3) {
      validHorizLbl = horizLbl %>%
                      filter(!is.na(sec1) & !is.na(sec2) &
                             !isCarProblem1 & !isCarProblem2 &
                             !inTraffic1 & !inTraffic2)
    }
    if (i %in% c(4, 5)) {
      validHorizLbl = horizLbl %>%
                      filter(!is.na(sec1) & !is.na(sec2) &
                             !isCarProblem1 & !isCarProblem2 &
                             !inTraffic1 & !inTraffic2 &
                             modalFinPosProb1 < 0.99 & modalFinPosProb2 < 0.99)
    }
    
    dum = validHorizLbl %>%
      summarise(delta = mean(sec1 - sec2),
                numLap = n())
    deltaVec[i] = dum %>% pull(delta)
    numObVec[i] = dum %>% pull(numLap)
    
    if (i == 5) {
      validHorizLbl = validHorizLbl %>%
        mutate(adjSec1 = sec1 - fuelTyreEffect1,
               adjSec2 = sec2 - fuelTyreEffect2)
      deltaVec[i] = with(validHorizLbl, mean( (adjSec1 - adjSec2)))
      meanTyreLap1 = mean(validHorizLbl$tyreLap1)
      meanTyreLap2 = mean(validHorizLbl$tyreLap2)
    }
  }
  
  toReturn = MakeListForFunctionOutput(deltaVec, numObVec, meanTyreLap1, meanTyreLap2)
  return(toReturn)
}

CoerceComparisonToList = function(currentPairingList) {
  secNumObVec = paste0(round(currentPairingList$deltaVec, 3), ' (', currentPairingList$numObVec, ')')
  niceVec = c(secNumObVec,
              round(currentPairingList$meanTyreLap1, 2),
              round(currentPairingList$meanTyreLap2, 2))
  return(niceVec)
}

ViewComparisonByYear = function(myYear) {
  tmPairing = f1data:::GetAllDriverTeamPairingByYear(myYear)
  # worth checking that both drivers actually did some laps, massive pain if not
  allTeamDriver = rddf %>%
    filter(year == myYear) %>%
    distinct(team, driver)
  sumLapByDriverTeam = lbl %>%
    filter(year == myYear & isGood30) %>%
    group_by(team, driver) %>%
    summarise(numLap = n()) %>%
    ungroup() %>%
    complete(allTeamDriver, fill = list(numLap = 0))
  tmPairing = tmPairing %>%
    left_join(sumLapByDriverTeam %>%
              rename(driver1 = driver,
                     numLap1 = numLap),
              c('team', 'driver1')) %>%
    left_join(sumLapByDriverTeam %>%
              rename(driver2 = driver,
                     numLap2 = numLap),
              c('team', 'driver2'))
  # now ditch non driving drivers
  tmPairing = tmPairing %>%
    filter(numLap1 > 0 & numLap2 > 0) %>%
    select(-c(numLap1, numLap2))
  
  compCol = c('all', 'minusCarProb', 'minusTraffic', 'minusDeadRubber', 'fuelAdj', 'meanTyreLap1', 'meanTyreLap2')
  tmPairing[,compCol] = NA
  
  for (ti in 1:nrow(tmPairing)) {
    rawOutput = with(tmPairing[ti,], CheckTwo(myYear, driver1, driver2, team))
    tmPairing[ti, compCol] = CoerceComparisonToList(rawOutput)
  }
  
  return(tmPairing)
}


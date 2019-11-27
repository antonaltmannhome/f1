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
  
  colToSpread = c('sec', 'inTraffic', 'isCarProblem', 'fuelTyreEffect', 'modalFinPosProb')
  horizLbl = myLbl %>%
    select(race, lap, driver, colToSpread) %>%
    spread_multiple(keyCol = driver, sec, inTraffic, isCarProblem, fuelTyreEffect, modalFinPosProb)
  
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
              numLap = n()))
}

# yikes, that changes it quite a lot for hulk/ric. that's a little unusual though actually
# this is cool though. next steps:
# exclude worst race for each driver
# include all non-blocked laps
# big job: out of sample prediction, what works best?


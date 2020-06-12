## tidy up what we've got

SetUpModel()

lbl = f1laptimelm::CalculateFuelTyreEffect(lbl, 30)
lbl = lazy_left_join(lbl, rddf, c('race', 'driver'), 'team')

HeadToHead = function(yearFilter, myDriv1, myDriv2, myTeam) {
  
  # same as above but make presentation more attractive
  myLbl = lbl %>%
    filter(driver %in% c(myDriv1, myDriv2) & year %in% yearFilter & !isRogue & team == myTeam)
  
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

MakeReadableComparison = function(yearFilter, myDriv1, myDriv2, myTeam) {
  myRawOutput = HeadToHead(yearFilter, myDriv1, myDriv2, myTeam)
  mySurname1 = f1data:::GetOtherDriverName(myDriv1)
  mySurname2 = f1data:::GetOtherDriverName(myDriv2)
  myAgeComment = paste0('average age of ',
                        c(mySurname1, mySurname2), '\'s tyres: ',
                        round(c(myRawOutput$meanTyreLap1, myRawOutput$meanTyreLap2), 1),
                        ' laps (during those ', tail(myRawOutput$numObVec, 1), ' laps)')
  
  myReadableTB = tibble(explanation = c('include all laps (except behind safety car, inlaps, outlaps, lap 1)',
                                        'exclude laps where either driver has a car problem',
                                        'exclude laps where either driver is in traffic',
                                        'exclude laps where either driver has no incentive to push',
                                        'adjust for tyre compound and tyre age',
                                        myAgeComment),
                        numOfLaps = c(paste(myRawOutput$numObVec, 'laps'),
                                      rep('', 2)),
                        result = c(paste(ifelse(myRawOutput$deltaVec > 0,
                                                mySurname2,
                                                mySurname1),
                                         round(abs(myRawOutput$deltaVec), 2),
                                         'faster'),
                                   rep('', 2)))
  
  return(myReadableTB)
}

ViewAllHeadToHeadByDriver = function(myDriv1) {
  # these are all the team pairings involving this driver
  myTeamMate = f1data:::GetAllTeamMateByDriver(myDriv1)
  
  yearByYearList = vector('list', nrow(myTeamMate))
  for (j in 1:nrow(collapsedTeamMate)) {
    yearByYearList[[j]] = with(myTeamMate[j,],
                              HeadToHead(yearFilter = year,
                                         myDriv1 = d1,
                                         myDriv2 = d2,
                                         myTeam = team))
  }
  
  
  # but I'd say we want to collapse these so it's one row for each combo
  collapsedTeamMate = myTeamMate %>%
    group_by(d1, d2, team) %>%
    summarise(yearFilter = list(year))
  
  collapsedList = vector('list', nrow(collapsedTeamMate))
  for (j in 1:nrow(collapsedTeamMate)) {
    collapsedList[[j]] = with(collapsedTeamMate[j,],
               HeadToHead(yearFilter = yearFilter[[1]],
                                      myDriv1 = d1,
                                      myDriv2 = d2,
                                      myTeam = team))
  }
  # now the tricky bit, need to make pretty table output
  # but we want 1 row for every season plus an additional row for combined where necessary
  
  combinedDF = 
}

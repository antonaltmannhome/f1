## tidy up what we've got

SetUpModel()
library(gt)

lbl = f1laptimelm::CalculateFuelTyreEffect(lbl, 30)
lbl = lazy_left_join(lbl, rddf, c('race', 'driver'), 'team')

# let's get rid of 2010 data, it's dodgy

HeadToHead = function(yearFilter, myDriv1, myDriv2, myTeam) {
  
  tlabb1 = toupper(substr(with(driverDF, surname[driver == myDriv1]), 1, 3))
  tlabb2 = toupper(substr(with(driverDF, gsub(' ', '', surname[driver == myDriv2])), 1, 3))
  
  numComparison = 5
  compName = c('all laps', 'exclude car problem', 'exclude traffic', 'exclude dead rubber', 'adjust for model')
  deltaVec = rep(NA, numComparison)
  numObVec = rep(0, numComparison)
  
  # same as above but make presentation more attractive
  myLbl = lbl %>%
    filter(driver %in% c(myDriv1, myDriv2) & year %in% yearFilter & !isRogue & team == myTeam)
  # edge case, maybe only one driver appears in that e.g. lotterer's caterham year
  
  haveValidComparisonData = length(unique(myLbl$driver)) == 2
  
  if (!haveValidComparisonData) {
    meanTyreLap1 = NA
    meanTyreLap2 = NA
    toReturn = lst(tlabb1, tlabb2, deltaVec, numObVec, meanTyreLap1, meanTyreLap2)
  }
  
  if (haveValidComparisonData) {
    
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
    
    toReturn = lst(tlabb1, tlabb2, deltaVec, numObVec, meanTyreLap1, meanTyreLap2)
  }
  
  return(toReturn)
}

MakeReadableComparison = function(yearFilter, myDriv1, myDriv2, myTeam) {
  myRawOutput = HeadToHead(yearFilter, myDriv1, myDriv2, myTeam)
  myTlabb1 = f1data:::GetOtherDriverName(myDriv1)
  myTlabb2 = f1data:::GetOtherDriverName(myDriv2)
  myAgeComment = paste0('average age of ',
                        c(myTlabb1, myTlabb2), '\'s tyres: ',
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
                                                myTlabb2,
                                                myTlabb1),
                                         round(abs(myRawOutput$deltaVec), 2),
                                         'faster'),
                                   rep('', 2)))
  
  return(myReadableTB)
}

MakeH2HTibbleOutput = function(myDescription, myList) {
  h2hOutputToVector = function(x)  {
    c(
      paste0(ifelse(x$deltaVec > 0, x$tlabb2, x$tlabb1), ' ',
           niceround(ifelse(x$deltaVec > 0, x$deltaVec, -x$deltaVec), 2),
           ' faster (', x$numObVec, ' laps)'),
      paste0(x$tlabb1, ': ', round(x$meanTyreLap1, 1), ' laps, ',
             x$tlabb2, ': ', round(x$meanTyreLap2, 1), ' laps (for the ', tail(x$numObVec, 1), ' laps)')
    )
  }
  myTB = t(sapply(myList, h2hOutputToVector))
  colnames(myTB) = paste0('tempCol', 1:ncol(myTB))
  myTB = as_tibble(myTB)
  
  myTB = bind_cols(myDescription, myTB)
  
  # however, should we delete the ones with almost no observations? i think so
  numOb = t(sapply(myList, function(x) x$numObVec))
  myTB$toKeep = numOb[,5] > 0
  
  return(myTB)
}

ViewAllHeadToHeadByDriver = function(myDriv1) {
  # these are all the team pairings involving this driver
  myTeamMate = f1data:::GetAllTeamMateByDriver(myDriv1)
  myTeamMate= myTeamMate %>%
    filter(year > 2010)
  
  yearByYearList = vector('list', nrow(myTeamMate))
  for (j in 1:nrow(myTeamMate)) {
    yearByYearList[[j]] = with(myTeamMate[j,],
                              HeadToHead(yearFilter = year,
                                         myDriv1 = d1,
                                         myDriv2 = d2,
                                         myTeam = team))
  }
  yearByYearTB = MakeH2HTibbleOutput(myTeamMate, yearByYearList)
  
  # now forget the !toKeep ones ever existed, to simplify things
  yearByYearTB = yearByYearTB %>%
    filter(toKeep)
  
  # but I'd say we want to collapse these so it's one row for each combo
  collapsedTeamMate = yearByYearTB %>%
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
  # let's make the tibble firstly

  collapsedTB = MakeH2HTibbleOutput(collapsedTeamMate %>%
                                      select(-yearFilter), collapsedList)
  
  # now the really fiddly bit, putting them in order
  maxYearByPairing = yearByYearTB %>%
    group_by(team, d1, d2) %>%
    summarise(minMinRr = min(minRr),
              maxYear = max(year),
              numYear = n())
  
  collapsedTB = left_join(collapsedTB, maxYearByPairing, c('team', 'd1', 'd2'))
  yearByYearTB = left_join(yearByYearTB, maxYearByPairing, c('team', 'd1', 'd2'))
  # no point having the ones that are only for one season
  collapsedTB = collapsedTB %>%
    filter(numYear > 1)

  collapsedTB$isSummary = TRUE
  yearByYearTB$isSummary = FALSE
  
  combinedTB = bind_rows(yearByYearTB,
                         collapsedTB) %>%
    arrange(maxYear, minMinRr, isSummary)
  
  combinedTB$year2 = with(combinedTB, ifelse(isSummary, 'overall', year))
  combinedTB$teamMate = with(driverDF, surname[match(combinedTB$d2, driver)])
  
  cleanCombinedTB = combinedTB %>%
    select(year2, team, teamMate, starts_with('tempCol'), isSummary) %>%
    rename(year = year2)
  
  # useful to number each block to aid colour coding
  teamMateRle = rle(cleanCombinedTB$teamMate)$len
  cleanCombinedTB$index = rep(1:length(teamMateRle), teamMateRle)

  return(cleanCombinedTB)
}

# now the fun bit, making the gt table

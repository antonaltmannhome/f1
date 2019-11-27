# let's try to do a graphical version of the pit stop delta calculations

LoadAllData()

lbl = f1laptimelm::MakePredSec(lbl, 4)
stintDF = f1messystint:::BolsterStint(stintDF)

# want a little table of inlaps and outlaps with relevant times
myRace = '2019australia'

.BolsterCurrentRacePitStopDF = function(myRace) {
  myPitStopDF = stintDF %>%
                filter(race == myRace & !isSafetyCar & !isRed & !stintRetired & isPitStop) %>%
                select(race, driver, endLap, pitStopTime) %>%
                lazy_left_join(lbl %>%
                                mutate(rawInlapSec = sec,
                                      inlapPredSec = mod4PredSec),
                                by = c('race', 'driver', 'endLap' = 'lap'),
                                c('rawInlapSec', 'inlapPredSec')) %>%
                lazy_left_join(lbl %>%
                              mutate(nextLap = lap - 1,
                                      rawOutlapSec = sec,
                                      outlapPredSec = mod4PredSec),
                              by = c('race', 'driver', 'endLap' = 'nextLap'),
                              c('rawOutlapSec', 'outlapPredSec'))


  # let's take out the ones that potentially/probably include the pitstop
  medianInOutlapSec = myPitStopDF %>%
                      filter(!is.na(rawInlapSec + rawOutlapSec)) %>%
                      summarise(medianInlapSec = median(rawInlapSec, na.rm = TRUE),
                                medianOutlapSec = median(rawOutlapSec, na.rm = TRUE))
  ioToAdjust = with(medianInOutlapSec, ifelse(medianInlapSec > medianOutlapSec, 'inlap', 'outlap'))

  if (ioToAdjust == 'inlap') {
    # it's the outlap that includes the pitstoptime
    myPitStopDF = myPitStopDF %>%
                  mutate(inlapSec = rawInlapSec - pitStopTime,
                          outlapSec = rawOutlapSec)
  }
  if (ioToAdjust == 'outlap') {
    # it's the outlap that includes the pitstoptime
    myPitStopDF = myPitStopDF %>%
                  mutate(inlapSec = rawInlapSec,
                          outlapSec = rawOutlapSec - pitStopTime)
  }
  return(myPitStopDF)
}

.DisplayInlapOutlapDelta = function(myPitStopDF, overwrittenUpperYLim = NULL, addedRaceDF) {
  lowerYLim = with(myPitStopDF, min(c(inlapPredSec, outlapPredSec, inlapSec, outlapSec), na.rm = TRUE))
  if (is.null(overwrittenUpperYLim)) {
    upperYLim = with(myPitStopDF, max(c(inlapPredSec, outlapPredSec, inlapSec, outlapSec), na.rm = TRUE))
  }
  if (!is.null(overwrittenUpperYLim)) {
    upperYLim = overwrittenUpperYLim
  }

  with(myPitStopDF, plot(inlapPredSec, inlapPredSec, col = 'red', pch = 16, ylim = c(lowerYLim, upperYLim)))
  with(myPitStopDF, points(inlapPredSec, inlapSec, col = 'red'))
  # that's including the pit stop time potentially
  with(myPitStopDF, points(outlapPredSec, outlapPredSec, col = 'green', pch = 16))
  with(myPitStopDF, points(outlapPredSec, outlapSec, col = 'green'))

  # now display the delta
  rawInlapDelta = with(myPitStopDF, median(inlapSec - inlapPredSec, na.rm = TRUE))
  inlapPredSecRange = with(myPitStopDF, range(inlapPredSec, na.rm = TRUE))
  with(myPitStopDF, lines(inlapPredSecRange, inlapPredSecRange + rawInlapDelta, col = 'red'))

  rawOutlapDelta = with(myPitStopDF, median(outlapSec - outlapPredSec, na.rm = TRUE))
  outlapPredSecRange = with(myPitStopDF, range(outlapPredSec, na.rm = TRUE))
  with(myPitStopDF, lines(outlapPredSecRange, outlapPredSecRange + rawOutlapDelta, col = 'green'))

  if (nrow(addedRaceDF) > 0) {
    for (yi in 1:nrow(addedRaceDF)) {
      with(myPitStopDF, lines(inlapPredSecRange, inlapPredSecRange + addedRaceDF$inlapDelta[yi],
                                col = 'red', lty = 3))
      with(myPitStopDF, lines(outlapPredSecRange, outlapPredSecRange + addedRaceDF$outlapDelta[yi],
                                col = 'green', lty = 3))
      # but need to label them along with their  number of standard stops
      text(inlapPredSecRange[1], (inlapPredSecRange + addedRaceDF$inlapDelta[yi])[1],
                with(addedRaceDF[yi,], paste(year, numStandardPitStop, sep = ' / ')),
                cex = 0.7)
      text(outlapPredSecRange[1], (outlapPredSecRange + addedRaceDF$outlapDelta[yi])[1],
                with(addedRaceDF[yi,], paste(year, numStandardPitStop, sep = ' / ')),
                cex = 0.7, adj = c(0, 0))
    }
  }

  lowerXLim = with(myPitStopDF, min(c(inlapPredSec, outlapPredSec), na.rm = TRUE))
  legend(lowerXLim, upperYLim, c('inlap', 'outlap'), lty = 1, col = c('red', 'green'),
          xjust = -1, yjust = 1)

  return(list(rawInlapDelta = rawInlapDelta, rawOutlapDelta = rawOutlapDelta))
}

MakeInlapOutlapDelta = function(myRace) {
  # so we try to do it automatically (allowing user to check it) but if it's a really messy race we might have to use data from other season to suggest the deltas
  myPitStopDF = .BolsterCurrentRacePitStopDF(myRace)
  satis = FALSE
  overwrittenUpperYLim = NULL
  addedRaceDF = raceDF %>% select(race, year, inlapDelta, outlapDelta, numStandardPitStop) %>% slice(0)
  while(!satis) {
    suggestedDelta = .DisplayInlapOutlapDelta(myPitStopDF,
                                      overwrittenUpperYLim = overwrittenUpperYLim,
                                      addedRaceDF = addedRaceDF)

    message('If you are happy with the suggestion, press \'h\'\nIf you want to adjust the upper y limit, press \'y\'\nIf you want to add a suggestion from a previous year, press \'a\'\nIf you want to click on the graph to force the inlapDelta and outlapDelta, press \'io\'')
    userInput = askcond(FALSE, FALSE)
    if (userInput == 'y') {
      message('What do you wnat the new upper y limit to be?')
      overwrittenUpperYLim = scan(quiet = TRUE, nmax = 1)
    }
    if (userInput == 'h') {
      inlapDelta = suggestedDelta$rawInlapDelta
      outlapDelta = suggestedDelta$rawOutlapDelta
      satis = TRUE
    }
    if (userInput == 'a') {
      addedRaceDF = .RetrieveRelevantOtherDelta(addedRaceDF, myRace)
    }
    if (userInput == 'io') {
      message('Click on a place that you think represents a sensible location for the inlap (RED) line')
      clickLocation = locator(n = 1)
      inlapDelta = with(clickLocation, y - x)
      message('Click on a place that you think represents a sensible location for the outlap (GREEN) line')
      clickLocation = locator(n = 1)
      outlapDelta = with(clickLocation, y - x)
      satis = TRUE
    }
  }

  return(list(inlapDelta = inlapDelta, outlapDelta = outlapDelta))
}

.RetrieveRelevantOtherDelta = function(addedRaceDF, myRace) {
  myCircuit = with(raceDF, circuit[race == myRace])
  relevantRace = with(raceDF, race[circuit == myCircuit & race != myRace & numStandardPitStop > 0])
  # but they're not relevant if we've already got them
  relevantRace = setdiff(relevantRace, addedRaceDF$race)
  # so we'll add in the previous race to the list
  if (length(relevantRace) == 0) {
    message('No more races to add!')
  }
  if (length(relevantRace) > 0) {
    addedRaceDF = bind_rows(addedRaceDF,
                          raceDF %>%
                            filter(race == tail(relevantRace, 1)) %>%
                            select(race, year, inlapDelta, outlapDelta, numStandardPitStop))
  }
  return(addedRaceDF)
}

# right, then surely it's quite simple to work out the implied lap times.

myPitStopDF$inlapDelta = inlapDelta
myPitStopDF$outlapDelta = outlapDelta

myPitStopDF$inlapImpSec = myPitStopDF$inlapSec - inlapDelta
myPitStopDF$outlapImpSec = myPitStopDF$outlapSec - outlapDelta

# yes, that all agrees with what we've already got, just about
# but of course we need something interactive and we need to be robust to the safety cars etc


lbl$newImpSec = NA

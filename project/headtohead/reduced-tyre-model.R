### initial look was interesting, now let's do the x-valid thing

## got to set up the intermediate models though, maybe we can steal it from elsewhere
# alas no, let's just make from scratch

# then what we actually do
# lbl = f1laptimelm::CalculateFuelTyreEffect(lbl, 30)
# no don't do that, because it uses all data so has unfair advantage

CalculateNonDriverEffect = function(myLbl, mod) {
  # no can't use this i don't think, see comment at end
  uglyDriverCoef = coef(mod)[grep('factor\\(driver\\)', names(coef(mod)))]
  neatDriverCoefDF = list_to_tibble(uglyDriverCoef,
                                    name = 'driver', value = 'driverCoef') %>%
                      mutate(driver = gsub('.+\\)', '', driver))
  myLbl = left_join(myLbl, neatDriverCoefDF, 'driver')
  # then get overall prediction
  # no can't do that, due to excluded drivers
  myLbl$predSec = predict(mod, myLbl)
  return(myLbl)
}

SingleRaceFitFuelOnlyLm = function(myLbl, myDriv1, myDriv2, myYear) {
  mod = lm(sec ~ factor(driver) + fuel - 1,
           data = myLbl %>% filter(!(driver %in% c(myDriv1, myDriv2) & year == myYear)))
  myLbl$fuelOnlyEffect = coef(mod)[['fuel']] * myLbl$fuel
  return(myLbl %>%
           select(race, driver, lap, fuelOnlyEffect))
}
#### then fuel plus tyre age, ignore compound
SingleRaceFitFuelTyreLapOnlyLm = function(myLbl, myDriv1, myDriv2, myYear) {
  mod = lm(sec ~ factor(driver) + fuel + tyreLap,
           data = myLbl %>% filter(!(driver %in% c(myDriv1, myDriv2) & year == myYear)))
  myLbl$fuelTyreLapOnlyEffect = coef(mod)[['fuel']] * myLbl$fuel + coef(mod)[['tyreLap']] * myLbl$tyreLap
  return(myLbl %>%
           select(race, driver, lap, fuelTyreLapOnlyEffect))
}
## now fuel and tyre compound but not tyre age
SingleRaceFitFuelTyreTypeOnlyLm = function(myLbl, myDriv1, myDriv2, myYear) {
  mod = lm(sec ~ factor(driver) + fuel + factor(tyre),
           data = myLbl %>% filter(!(driver %in% c(myDriv1, myDriv2) & year == myYear)))
  # what a hassle..
  uglyTyreCoef = coef(mod)[grep('factor\\(tyre\\)', names(coef(mod)))]
  neatTyreCoefDF = list_to_tibble(uglyTyreCoef, 'coef', 'tyre') %>%
    mutate(tyre = gsub('factor\\(tyre\\)', '', tyre))
  missingTyre = setdiff(unique(myLbl$tyre), neatTyreCoefDF$tyre)
  neatTyreCoefDF = add_row(neatTyreCoefDF, tyre = missingTyre, coef = 0)
  myLbl$tyreTypeEffect = neatTyreCoefDF$coef[match(myLbl$tyre, neatTyreCoefDF$tyre)]
  myLbl$fuelTyreTypeOnlyEffect = coef(mod)[['fuel']] * myLbl$fuel + myLbl$tyreTypeEffect
  return(myLbl %>%
           select(race, driver, lap, fuelTyreTypeOnlyEffect))
}
### then, fuel and tyre age and type, but common slope for them
SingleRaceFitFuelTyreCommonSlopeLm = function(myLbl, myDriv1, myDriv2, myYear) {
  mod = lm(sec ~ factor(driver) + fuel + factor(tyre) + tyreLap,
           data = myLbl %>% filter(!(driver %in% c(myDriv1, myDriv2) & year == myYear)))
  # what a hassle..
  uglyTyreCoef = coef(mod)[grep('factor\\(tyre\\)', names(coef(mod)))]
  neatTyreCoefDF = list_to_tibble(uglyTyreCoef, 'coef', 'tyre') %>%
    mutate(tyre = gsub('factor\\(tyre\\)', '', tyre))
  missingTyre = setdiff(unique(myLbl$tyre), neatTyreCoefDF$tyre)
  neatTyreCoefDF = add_row(neatTyreCoefDF, tyre = missingTyre, coef = 0)
  myLbl$tyreTypeEffect = neatTyreCoefDF$coef[match(myLbl$tyre, neatTyreCoefDF$tyre)]
  myLbl$fuelTyreCommonSlopeEffect = coef(mod)[['fuel']] * myLbl$fuel + myLbl$tyreTypeEffect + coef(mod)[['tyreLap']] * myLbl$tyreLap
  return(myLbl %>%
           select(race, driver, lap, fuelTyreCommonSlopeEffect))
}

FitAllLm = function(lbl, myDriv1, myDriv2, myYear) {
  myList = lbl %>%
    filter(isGood30) %>%
    group_by(race) %>%
    do(SingleRaceFitFuelOnlyLm(., myDriv1 = myDriv1, myDriv2 = myDriv2, myYear = myYear))
  fuelOnlyLmDF = bind_rows(myList)
  
  myList = lbl %>%
    filter(isGood30) %>%
    group_by(race) %>%
    do(SingleRaceFitFuelTyreLapOnlyLm(., myDriv1 = myDriv1, myDriv2 = myDriv2, myYear = myYear))
  fuelTyreLapOnlyLmDF = bind_rows(myList)
  
  myList = lbl %>%
    filter(isGood30) %>%
    group_by(race) %>%
    do(SingleRaceFitFuelTyreTypeOnlyLm(., myDriv1 = myDriv1, myDriv2 = myDriv2, myYear = myYear))
  fuelTyreTypeOnlyLmDF = bind_rows(myList)
  
  myList = lbl %>%
    filter(isGood30) %>%
    group_by(race) %>%
    do(SingleRaceFitFuelTyreCommonSlopeLm(., myDriv1 = myDriv1, myDriv2 = myDriv2, myYear = myYear))
  fuelTyreCommonSlopeLmDF = bind_rows(myList)
  
  lbl = left_join(lbl, fuelOnlyLmDF, c('race', 'driver', 'lap'))
  lbl = left_join(lbl, fuelTyreLapOnlyLmDF, c('race', 'driver', 'lap'))
  lbl = left_join(lbl, fuelTyreTypeOnlyLmDF, c('race', 'driver', 'lap'))
  lbl = left_join(lbl, fuelTyreCommonSlopeLmDF, c('race', 'driver', 'lap'))
  
  return(lbl)
}


# these names are a little confusing to be fair
# but they're not fitted out of sample so isn't the most complicated bound to win? not sure

# so, first model assume all team mates are equally good, predict lap time difference.
# we're only predicting a subset of the data, so maybe it's not guaranteed to be better for a small sample?
# no i think it's still guaranteed to be better once we've aggregated over all pairings.
# but we could loop over all team-seasons, eliminate that team's data, then refit the models above. that is a great idea in fact

CheckTeamYear = function(myDriv1, myDriv2, myYear) {
  
  lbl = FitAllLm(lbl, myDriv1, myDriv2, myYear)
 
  # now we make the data frame with relevant comparisons
  myLbl = lbl %>%
    filter(driver %in% c(myDriv1, myDriv2) & year == myYear & !isRogue)
 
  colToSpread = c('sec', 'inTraffic', 'isCarProblem', 'modalFinPosProb',
                  'fuelOnlyEffect', 'fuelTyreLapOnlyEffect',
                  'fuelTyreTypeOnlyEffect', 'fuelTyreCommonSlopeEffect')
  horizLbl = myLbl %>%
    select(race, lap, driver, colToSpread) %>%
    spread_multiple(keyCol = driver, sec, inTraffic, isCarProblem, modalFinPosProb,
                    fuelOnlyEffect, fuelTyreLapOnlyEffect,
                    fuelTyreTypeOnlyEffect, fuelTyreCommonSlopeEffect)
  
  ## horrible names of course, so rename
  badName = paste0('driver', apply(expand.grid(c(myDriv1, myDriv2), colToSpread), 1, paste, collapse = '_'))
  goodName = apply(expand.grid(c(1, 2), colToSpread), 1, function(x) paste(rev(x), collapse = ''))
  
  for (j in 1:length(badName)) {
    names(horizLbl)[names(horizLbl) == badName[j]] = goodName[j]
  }
  
   
  return(lbl)
}


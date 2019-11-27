QuickCheckModelRatings = function() {
  LoadAllData()
  dum = rddf %>%
    filter(race == raceDF$race[nrace]) %>%
    select(driver, mod4RawDCoef, mod4PredNValid) %>%
    arrange(mod4RawDCoef)
  print(dum)
}

# try out factor analysis on data, see if anything useful comes up
SetUpModel()

myYear = 2018

rddf = lazy_left_join(rddf, raceDF, 'race', 'circuit')
subRddf = rddf %>% filter(year == myYear) %>% select(driver, circuit, mod30DCoef)

resmat = spread(subRddf, key = 'circuit', value = 'mod30DCoef')
drivervec = resmat %>% pull(driver)
resmat = resmat %>% select(-driver)
resmat = as.matrix(resmat)

dum = suppressWarnings(psych::fa(resmat, nfactors = 2, missing = TRUE, rotate = 'varimax'))
 # so monaco in like monza, fantastic
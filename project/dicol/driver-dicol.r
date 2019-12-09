SetUpModel()

lbl = lazy_left_join(lbl, rddf, c('race', 'driver'), 'team')
lbl$teamYear = with(lbl, paste(team, year))

# separate the two kubicas
lbl$driver2 = lbl$driver
lbl = lbl %>%
  mutate_cond(driver == 'rkubica' & year == 2019,
              driver2 = 'rkubica2')

lbl$isGood = with(lbl, isGood30 & !isCarProblem & year != 2010 &
                    !driver %in% c('jbianchi', 'mchilton', 'rkubica2', 'grussell', 'jalguersuari', 'sbuemi'))

modelMatrix = with(lbl %>% filter(isGood),
                   model.matrix(~ factor(teamYear) + factor(driver2) + factor(race) - 1))
mod = speedglm::speedglm.wfit(y = with(lbl, sec[isGood]), X = modelMatrix, intercept = FALSE, sparse = TRUE)
# mod = lm(sec ~ factor(teamYear) + factor(driver) + factor(race) - 1, data = lbl %>% filter(isGood))

uglyDriverCoef = coef(mod)[grep('factor\\(driver2\\)', names(coef(mod)))]
driverCoefDF = tibble::enframe(uglyDriverCoef, name = 'driver', value = 'coef')
driverCoefDF$driver = gsub('factor\\(driver2\\)', '', driverCoefDF$driver)
missingDriver = setdiff(with(lbl, unique(driver2[isGood])), driverCoefDF$driver)
driverCoefDF = add_row(driverCoefDF, driver = missingDriver, coef = 0)

driverCoefDF %>% arrange(coef) %>% head(20)

# where is kubica 2 ffs
## just get rid of bianchi/chilton/russell/kubica2
# webber ahead of vettel, how...?
# maybe get rid of 2010 data too

### arrgh, i think kovalainen and dambrosios terrible one offs are swaying things too much
### just ditch every driver who only makes cameos for teams is surely sensible


driverCoefDF %>% arrange(coef) %>% head(20)

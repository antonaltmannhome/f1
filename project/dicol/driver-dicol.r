SetUpModel()

# let's get driver connections
suppressWarnings(library(GGally))
suppressWarnings(library(network))
suppressWarnings(library(sna))
suppressWarnings(library(ggplot2))

lbl = lazy_left_join(lbl, rddf, c('race', 'driver'), 'team')
lbl$teamYear = with(lbl, paste(team, year))

# but we have disconnected drivers

lbl$isGoodPreFilter = with(lbl, isGood30 & !isCarProblem & year != 2010)

# ditch drivers who didn't do more than 2 races for a team
driverTeamYearCount = lbl %>%
  filter(isGoodPreFilter) %>%
  group_by(driver, team, year) %>%
  summarise(numRace = length(unique(race))) %>%
  ungroup

lbl = indicate_overlapping_combination(lbl,
                                       driverTeamYearCount %>%
                                         filter(numRace <=2),
                                       c('driver', 'team', 'year'),
                                       'isCameo')


# and those who joined a team late in the year
raceDF = raceDF %>%
  group_by(year) %>%
  mutate(roundNumber = rank(daynum))
lateJoiner = rddf %>%
  lazy_left_join(raceDF, 'race', c('year', 'roundNumber')) %>%
  group_by(team, year, driver) %>%
  summarise(joinRoundNumber = min(roundNumber)) %>% 
  filter(joinRoundNumber > 8)
lbl = indicate_overlapping_combination(lbl, lateJoiner, c('team', 'year', 'driver'), 'isLateJoiner')

#webPageDF$seriesSeason = with(webPageDF, paste(series, season))
#unseriesSeason = unique(webPageDF$seriesSeason)

unTeamDriverYear = lbl %>%
  filter(isGoodPreFilter & !isCameo & !isLateJoiner) %>%
  distinct(team, driver, year)
unTeamDriverYear$teamYear = with(unTeamDriverYear, paste(team, year))

unDriver = unique(unTeamDriverYear$driver)
tmmatrix = matrix(0, nrow = length(unDriver), ncol = length(unDriver))
rownames(tmmatrix) = unDriver
colnames(tmmatrix) = unDriver

for (j in 1:length(unDriver)) {
  for (k in 1:length(unDriver)) {
    if (j != k) {
      overlap = with(unTeamDriverYear,
                     intersect(teamYear[driver == unDriver[j]],
                               teamYear[driver == unDriver[k]]))
      if (length(overlap) > 0) tmmatrix[j,k] = 1
    }
  }
}

net = network(tmmatrix, directed = FALSE)
network.vertex.names(net) = unDriver
ggnet2(net, size = 12, label = TRUE, label.size = 5)

disconnectedDriver = c('jbianchi', 'mchilton', 'rmehri', 'wstevens', 'arossi', 'jalguersuari', 'sbuemi', 'grussell', 'rkubica', 'gvandergarde', 'cpic', 'tglock', 'jdambrosio', 'vliuzzi', 'nkarthikeyan', 'pdelarosa', 'nheidfeld', 'vpetrov', 'hkovalainen', 'jtrulli')

# and in fact the new teams 
lbl$isDisconnected = with(lbl, driver %in% disconnectedDriver)

lbl$isGood = with(lbl, isGoodPreFilter & !isCameo & !isDisconnected & !isLateJoiner)

modelMatrix = with(lbl %>% filter(isGood),
                   model.matrix(~ factor(teamYear) + factor(driver) + factor(race) * fuel + factor(race) * tyreLap- 1))
mod = speedglm::speedglm.wfit(y = with(lbl, sec[isGood]), X = modelMatrix, intercept = FALSE)
# mod = lm(sec ~ factor(teamYear) + factor(driver) + factor(race) - 1, data = lbl %>% filter(isGood))

uglyDriverCoef = coef(mod)[grep('factor\\(driver\\)', names(coef(mod)))]
driverCoefDF = tibble::enframe(uglyDriverCoef, name = 'driver', value = 'coef')
driverCoefDF$driver = gsub('factor\\(driver\\)', '', driverCoefDF$driver)
missingDriver = setdiff(with(lbl, unique(driver[isGood])), driverCoefDF$driver)
driverCoefDF = add_row(driverCoefDF, driver = missingDriver, coef = 0)

driverCoefDF %>% arrange(coef) %>% head(20)

# where is kubica 2 ffs
## just get rid of bianchi/chilton/russell/kubica2
# webber ahead of vettel, how...?
# maybe get rid of 2010 data too

### arrgh, i think kovalainen and dambrosios terrible one offs are swaying things too much
### just ditch every driver who only makes cameos for teams is surely sensible


driverCoefDF %>% arrange(coef) %>% head(20)

# what a load of shite
# so it's bruno senna that's at the heart of the problem i think
# thrashed by petrov at lotus, so all of petrov's connections get a boost
# eliminate anyone who joined a long way into a season?
# so bye bye albon@rb, ocon@virgin etc

# done all that, it's still shtie

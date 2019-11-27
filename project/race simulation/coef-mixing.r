### can we reproduce the mod34upd coef reasonably simply in the new world?

source('model code/model-startup.r')

# first step, get hold of best pre-race driver coef
dum = f1smoothing:::GetSmooth('qr', 'r', 'bw', useStretch = TRUE, modelChoice = 30)

rddf = lazy_left_join(rddf, dum$rddf, c('race', 'driver'), 'preStretchQualDCoef')
rddf = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'smoothDCoef')

# there's a hierarchy of things we use: mixture of smoothDCoef and preStretchQualDCoef if available
# then fall back on team-mate's pace
# then fall back on guess based on previous season

teamSmoothCoef = rddf %>%
                  group_by(year, race, rr, team) %>%
                  summarise(teamSmoothCoef = mean(smoothDCoef, na.rm = TRUE)) %>%
                  ungroup()
# the fiddly bit is, at the start of the season it's not defined, so we need to fudge that
# actually, it's not including pace shown at the very last race of previous season - feel like that should be a possible addition to the smoothing. just the useCurrentRace option could be added, but obvs you don't optimise over it
teamSmoothCoef = teamSmoothCoef %>%
                  group_by(team) %>%
                  arrange(rr) %>%
                  mutate(isFirstRaceOfSeason = year != lag(year),
                          teamSmoothCoef = ifelse(isFirstRaceOfSeason,
                                                        lag(teamSmoothCoef),
                                                        teamSmoothCoef))
teamSmoothCoef = teamSmoothCoef %>%
                  mutate_cond(year == 2010 &
                              team %in% c('caterham', 'marussia', 'hrt') &
                              race == '2010bahrain',
                              teamSmoothCoef = 2)
teamSmoothCoef = teamSmoothCoef %>%
                  mutate_cond(year == 2016 &
                              team == 'haas' &
                              race == '2016australia',
                              teamSmoothCoef = 0.2)
rddf = lazy_left_join(rddf, teamSmoothCoef, c('rr','team'), 'teamSmoothCoef')

# then we ned to add the mix of the team-mate's qual and race smooth to this.
# although if you don't have qual, do you use smoothqual? why not, it's better than nothing
# no, that's already in smoothDCoef, because yougave it qr in qrToFit
# and do we need team mate coef? yes, for first race of season it will be better than teamsmoothcoef

CollectTeamMateCoef = function(myRddf) {
  myRddf = myRddf %>%
            group_by(team) %>%
            mutate(alloc = 1:n())
  tmMatchIndex = with(myRddf, match(paste(team, alloc), paste(team, 3 - alloc)))
  myRddf$teamMateQualDCoef = myRddf$preStretchQualDCoef[tmMatchIndex]
  myRddf = within(myRddf, rm(alloc))
  return(myRddf)
}
teamMateCoef = rddf %>%
                group_by(race) %>%
                select(race, team, driver, preStretchQualDCoef) %>%
                do(CollectTeamMateCoef(.))

rddf = lazy_left_join(rddf, teamMateCoef, c('race', 'driver'), 'teamMateQualDCoef')

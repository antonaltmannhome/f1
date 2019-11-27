InitialisePreRaceCoef = function(rddf, modelChoice, runMode) {

  # NB two runModes. 'postLMFit' assumes you have already done everything for the rae3 e.g for the simulations. 'preLMFit' means that you don't have the model estimates yet, so won't have done things like normalising the qualifying coefs, because you don't do that til after fitting the lm model
  # first step, get hold of best pre-race driver coef
  # although of course we can't do this with the edge case that we haven't yet fit any estimates
  # NB scratch 'runMode', decided it wasn't needed after all. in fact this function is only used for the simulations at the moment
  
  dum = f1smoothing:::GetSmooth('qr', 'r', 'bw', useStretch = TRUE,
                                  modelChoice = modelChoice)
  rddf = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'smoothDCoef')
  rddf = lazy_left_join(rddf, dum$rddf, c('race', 'driver'), 'preStretchQualDCoef')

  if (FALSE) {
    # the problem we have is, we don't have preStretchQualDCoef because we haven't been able to produce a normalised qualifying coef for the final race, so let's sort that now
      finalRaceIndex = with(rddf, which(rr == nrace & !is.na(smoothDCoef) & !is.na(modQualRawDCoef)))
      meanSmoothDCoef = mean(rddf$smoothDCoef[finalRaceIndex])
      meanRawQualDCoef = mean(rddf$modQualRawDCoef[finalRaceIndex])
      rddf$preStretchQualDCoef[finalRaceIndex] =
        rddf$modQualRawDCoef[finalRaceIndex] - meanRawQualDCoef + meanSmoothDCoef
  }

  rddf = f1smoothing:::GetTeamSmoothCoef(rddf)
  rddf = f1smoothing:::GetTeamMateDCoef(rddf)

  rddf$teamPriorDCoef = with(rddf, ifelse(!is.na(teamMateQualDCoef), teamMateQualDCoef, teamSmoothCoef))

  rddf = rddf %>%
    rename(qualDCoef = preStretchQualDCoef,
           smoothRaceDCoef = smoothDCoef)

  # this is fiddly because we won't have everything we need so need to put in -99s and 0s
  rddf = rddf %>%
    mutate(numQualDCoef = as.numeric(!is.na(qualDCoef)),
           numSmoothRaceDCoef = as.numeric(!is.na(smoothRaceDCoef)))
  rddf$qualDCoef = with(rddf, ifelse(!is.na(qualDCoef), qualDCoef, -999))
  rddf$smoothRaceDCoef = with(rddf, ifelse(!is.na(smoothRaceDCoef), smoothRaceDCoef, -999))

  rddf$preRaceDCoef = with(rddf, case_when(
                            qualDCoef > -998 & smoothRaceDCoef > -998 ~ 0.5 * qualDCoef + 0.5 * smoothRaceDCoef,
                            qualDCoef > -998 & smoothRaceDCoef < (-998) ~ qualDCoef,
                            qualDCoef < (-998) & smoothRaceDCoef > -998 ~ smoothRaceDCoef,
                            qualDCoef < (-998) & smoothRaceDCoef < (-998) ~ teamPriorDCoef))

  return(rddf)
}


GetTeamSmoothCoef = function(rddf) {

  # there's a hierarchy of things we use: mixture of smoothDCoef and preStretchQualDCoef if available
  # then fall back on team-mate's pace
  # then fall back on guess based on previous season

  teamSmoothCoef = rddf %>%
                    group_by(year, race, rr, aaTeam) %>%
                    summarise(teamSmoothCoef = mean(smoothDCoef, na.rm = TRUE)) %>%
                    ungroup()
  # the fiddly bit is, at the start of the season it's not defined, so we need to fudge that
  # actually, it's not including pace shown at the very last race of previous season - feel like that should be a possible addition to the smoothing. just the useCurrentRace option could be added, but obvs you don't optimise over it
  teamSmoothCoef = teamSmoothCoef %>%
                    group_by(aaTeam) %>%
                    arrange(rr) %>%
                    mutate(isFirstRaceOfSeason = year != lag(year),
                           teamSmoothCoef = ifelse(isFirstRaceOfSeason,
                                                   lag(teamSmoothCoef),
                                                   teamSmoothCoef))
  teamSmoothCoef = teamSmoothCoef %>%
                    mutate_cond(year == 2010 &
                                  aaTeam %in% c('caterham', 'manor', 'hrt') &
                                  race == '2010bahrain',
                                teamSmoothCoef = 2)
  teamSmoothCoef = teamSmoothCoef %>%
                    mutate_cond(year == 2016 &
                                  aaTeam == 'haas' &
                                  race == '2016australia',
                                teamSmoothCoef = 0.2)
  rddf = lazy_left_join(rddf, teamSmoothCoef, c('rr','aaTeam'), 'teamSmoothCoef')

  return(rddf)
}

.CollectTeamMateCoef = function(myRddf) {
  myRddf = myRddf %>%
    group_by(team) %>%
    mutate(alloc = 1:n())
  tmMatchIndex = with(myRddf, match(paste(team, alloc), paste(team, 3 - alloc)))
  myRddf$teamMateQualDCoef = myRddf$preStretchQualDCoef[tmMatchIndex]
  myRddf = within(myRddf, rm(alloc))
  return(myRddf)
}

GetTeamMateDCoef = function(rddf) {

  teamMateCoef = rddf %>%
    group_by(race) %>%
    select(race, team, driver, preStretchQualDCoef) %>%
    do(.CollectTeamMateCoef(.))

  rddf = lazy_left_join(rddf, teamMateCoef, c('race', 'driver'), 'teamMateQualDCoef')

  return(rddf)
}

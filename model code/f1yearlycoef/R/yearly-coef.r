### the research for this was done in driver-plus-team-oos.r
### the upshot of it is these two values:

lineOFBestFitCoef = c(0.08582368, 0.85915112)

# we now use that to attach a weight to every driver-race combo


GetConnectedDriver = function(raceDriverCoefDF) {
	# this might havea a problem when both drivers get replaced with no overlap, but not biggest worry in the world right now
	connectedDriver = raceDriverCoefDF %>%
						group_by(team, race) %>%
						filter(n() > 1) %>%
						ungroup() %>%
						distinct(team, driver) %>%
						arrange(team)
	return(connectedDriver)
}

FilterOutOneOffAppearance = function(driverCoefDF) {
	oneOffAppearance = driverCoefDF %>%
							group_by(year, team, driver) %>%
							summarise(numberOfAppearance = n()) %>%
							filter(numberOfAppearance == 1)
	driverCoefDF = indicate_overlapping_combination(
							driverCoefDF,
							oneOffAppearance,
							c('yr', 'team', 'driver'),
							'isOneOffAppearance')
	driverCoefDF = driverCoefDF %>%
							filter(!isOneOffAppearance)
							
	return(driverCoefDF)
}

GetTeamMateCoefGivenTeamDF = function(myTeamDF) {
	modelMatrix = model.matrix(~driver + race - 1, myTeamDF)

	mod = lm.wfit(x = modelMatrix, y = myTeamDF$mod30RawDCoef, w = myTeamDF$numObWeight)
	driverCoef = mod$coef[grep('driver', names(mod$coef))]
	names(driverCoef) = gsub('^driver', '', names(driverCoef))
	
	return(driverCoef)
}

GetTeamMateCoef = function(myTeamDF) {
	driverCoef = f1yearlycoef::GetTeamMateCoefGivenTeamDF(myTeamDF)
	driverCoefDF = data.frame(team = myTeamDF$team[1],
							driver = names(driverCoef),
							driverCoef = as.numeric(driverCoef))
	# want to normalise. taking off overall mean isn't quite right, there might be a one off appearance by a driver who's much slower than others so distorts overall mean, so take into account number of participations
	driverCoefDF = left_join(driverCoefDF,
								myTeamDF %>%
									group_by(driver) %>%
									summarise(numAppearance = n()),
								'driver')
	driverCoefDF$driverCoef = with(driverCoefDF, driverCoef - weighted.mean(driverCoef, numAppearance))

	return(driverCoefDF)
}

GetTeamCoef = function(myRaceDriverCoefDF) {
	mod = lm(mod30RawDCoef ~ factor(race) + factor(team) - 1,
						weight = numObWeight,
						offset = driverCoef,
						data = myRaceDriverCoefDF)
	myTeamCoefDF = ExtractLMFactorCoef(mod)$teamCoefDF

	return(myTeamCoefDF)
}

GetYearlyCoef = function(myYear) {

	# ditch badly fitted races
	terriblyFittedRacename = scan(paste0(OUTPUTPATH,'valid-but-terribly-fitted-race.dat'), what = '', quiet = TRUE)
	raceDF$isValidRace30[which(raceDF$race %in% terriblyFittedRacename)] = FALSE

	raceDriverCoefDF = rddf %>%
						filter(year == myYear & mod30PredNValid > 0) %>%
						select(race, driver, team, mod30RawDCoef, mod30PredNValid)

	raceDriverCoefDF$numObWeight = 1 - exp(-lineOFBestFitCoef[1] * raceDriverCoefDF$mod30PredNValid ^ lineOFBestFitCoef[2])
	# however, races with only 1 drive rin them don't help with the driver coef estimation so can trim down those
	raceTeamDriverCount = raceDriverCoefDF %>%
							group_by(race, team) %>%
							summarise(raceTeamDriverCount = n())
	raceDriverCoefDF = left_join(raceDriverCoefDF,
								raceTeamDriverCount,
								c('race', 'team'))

	validRaceDriverCoefDF = raceDriverCoefDF %>%
							filter(raceTeamDriverCount == 2)

	connectedDriver = f1yearlycoef::GetConnectedDriver(validRaceDriverCoefDF)

	validRaceDriverCoefDF = inner_join(validRaceDriverCoefDF,
								connectedDriver,
								by = c('team', 'driver'))

	splitByTeamDF = split(validRaceDriverCoefDF,
								validRaceDriverCoefDF$team)

	driverCoefDF = purrr::map_df(splitByTeamDF,
									f1yearlycoef::GetTeamMateCoef)

	raceDriverCoefDF = left_join(raceDriverCoefDF,
							driverCoefDF,
							c('team', 'driver'))

	# then just a case of getting the team coefs with drier coefs as offset
	# NB have to split by year though, otherwise get NA because of no overlap between races/yrTeams from different seasons

	teamCoefDF = f1yearlycoef::GetTeamCoef(raceDriverCoefDF)

	# ok, we've got what we need, now it's just a case of creating the combinations in question

	teamDriverCoefDF = inner_join(driverCoefDF,
									teamCoefDF,
									'team') %>%
							mutate(driverTeamCoef = teamCoef + driverCoef) %>%
							mutate(driverTeamCoef = driverTeamCoef - min(driverTeamCoef))

	### but we want the info about number of races entered and completed
	enteredIncludedDF = raceDriverCoefDF %>%
						lazy_left_join(raceDF, 'race', 'nlap') %>%
						mutate(propLap = mod30PredNValid / nlap) %>%
						group_by(driver) %>%
						summarise(numRaceEntered = n(),
									numRaceEffectiveIncluded = sum(propLap))

	teamDriverCoefDF = left_join(teamDriverCoefDF, enteredIncludedDF, 'driver')
							
	return(teamDriverCoefDF)
}
### aarghh, still got to get the columns desired by pace-rank, not that big a job i don't think though

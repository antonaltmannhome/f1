
CalculateTyreValidity = function(myRace, lbl) {

	numIsGoodPreValidRace = with(lbl, sum(isGoodPreValidRace[race == myRace]))
	if (numIsGoodPreValidRace == 0) {
		numStintByTyre = tibble(tyre = with(raceTyreDF, tyre[race == myRace]),
								numStint = 0,
								totalOverlap = 0,
								sumInternalContrast = 0,
								isValidTyre = FALSE)
	}

	if (numIsGoodPreValidRace > 0) {
		currentRaceDriverTyre = lbl %>%
									filter(race == myRace & isGoodPreValidRace) %>%
									group_by(driver, tyre, stint) %>%
									summarise(numGood = n(),
												numGoodProp = 1 - exp(-0.2 * numGood))

		numStintByDriverTyre = currentRaceDriverTyre %>%
								group_by(driver, tyre) %>%
								summarise(numGoodPropTyre = sum(numGoodProp))
		# but were there some good internal constrasts? very convoluted to detect this
		# firstly, restrict to drivers who did >=2 stints on same tyre, obviously
		# then if they've done >2, take longest two
		# then multiply them togehter, that's how good the comparison is
		# there might not be any situations where drivers have done more 1 stint though
		moreThanOneStintDriverTyre = currentRaceDriverTyre %>%
											count(driver, tyre) %>%
											filter(n >= 2)
		if (nrow(moreThanOneStintDriverTyre) == 0) {
			internalContrast = tibble(tyre = unique(currentRaceDriverTyre$tyre),
										sumInternalContrast = 0)
		}
		if (nrow(moreThanOneStintDriverTyre) > 0) {
			internalContrastByDriverTyre = inner_join(currentRaceDriverTyre,
											moreThanOneStintDriverTyre %>%
												select(driver, tyre),
												c('driver','tyre')) %>%
											group_by(driver, tyre) %>%
											arrange(-numGoodProp) %>%
											mutate(numGoodPropRank = 1:n()) %>%
											filter(numGoodPropRank <= 2) %>%
											summarise(internalContrast = prod(numGoodProp)) %>%
											ungroup()
			# but mising levels are a pain, let's stick em in
			internalContrastByDriverTyre = complete(internalContrastByDriverTyre,
													expand(currentRaceDriverTyre,
															nesting(driver, tyre)),
													fill = list(internalContrast = 0))

			internalContrast = internalContrastByDriverTyre %>%
								group_by(tyre) %>%
								summarise(sumInternalContrast = sum(internalContrast))
		}

		numStintByTyre = currentRaceDriverTyre %>%
							group_by(tyre) %>%
							summarise(numStint = sum(numGoodProp))

		if (nrow(numStintByTyre) == 1) {
			numStintByTyre$totalOverlap = 0
		}
		# now cycle through the pair to get each tyre's overlap quality
		if (nrow(numStintByTyre) > 1) {
			allTyreCombo = as_tibble(t(combn(numStintByTyre$tyre, 2)))
			allTyreCombo$numOverlap = NA
			for (ci in 1:nrow(allTyreCombo)) {
				currentTyre = unlist(allTyreCombo[ci,c('V1','V2')])
				currentTyreNumStint = currentRaceDriverTyre %>%
								filter(tyre %in% currentTyre) %>%
								group_by(driver, tyre) %>%
								summarise(numStint = n()) %>%
								ungroup()
				horizCurrentTyreNumStint = spread_multiple(currentTyreNumStint %>%
									select(driver, tyre, numStint),
									key = 'tyre', value = numStint)
				tyrecolname = names(horizCurrentTyreNumStint)[grep('^tyre', names(horizCurrentTyreNumStint))]
				horizCurrentTyreNumStint[,tyrecolname][is.na(horizCurrentTyreNumStint[,tyrecolname])]=0

				numOverlap = horizCurrentTyreNumStint %>%
									summarise(numOverlap = sum(get(tyrecolname[1])>0 &
																get(tyrecolname[2]) > 0))
				allTyreCombo$numOverlap[ci] = numOverlap$numOverlap
			}

			# right, let's compress our little rule to return whether any tyres and hence the race, are good
			numStintByTyre$totalOverlap = NA
			for (ti in 1:nrow(numStintByTyre)) {
				numStintByTyre$totalOverlap[ti] =
					with(allTyreCombo, sum(numOverlap[V1 == numStintByTyre$tyre[ti] |
														V2 == numStintByTyre$tyre[ti]]))
			}
		}

		numStintByTyre = left_join(numStintByTyre,
									internalContrast,
									'tyre') %>%
							mutate(isValidTyre = (numStint > 5 & totalOverlap > 5) |
													sumInternalContrast > 5)
	}

	return(numStintByTyre)
}

# this was my new version that seems to have gone horribly wrong:
CalculateTyreValidity = function(myRace, lbl) {

	numIsGoodPreValidRace = with(lbl, sum(isGoodPreValidRace[race == myRace]))
	if (numIsGoodPreValidRace == 0) {
		numStintByTyre = tibble(tyre = with(raceTyreDF, tyre[race == myRace]),
															numStint = 0,
															isValidTyre = FALSE)
	}

	if (numIsGoodPreValidRace > 0) {
		currentRaceDriverTyre = lbl %>%
									filter(race == myRace & isGoodPreValidRace) %>%
									group_by(driver, tyre, stint) %>%
									summarise(numGood = n(),
														numGoodProp = 1 - exp(-0.2 * numGood))

		numStintByDriverTyre = currentRaceDriverTyre %>%
								group_by(driver, tyre) %>%
								summarise(numGood = sum(numGood)) %>%
								ungroup()
		# want to pick up when there's a tyre that's been used only by 1 driver who didn't use anything else
		numStintByDriverTyre = complete(numStintByDriverTyre,
		                                  expand(numStintByDriverTyre, driver, tyre),
		                                  fill = list(numGood = 0))
		horizNumStintByDriverTyre = spread(numStintByDriverTyre, key = tyre, value = numGood)
		# it's the ones that have only one non-zero value in the column that will crash the lm
		currentRaceTyre = unique(currentRaceDriverTyre$tyre)
		myIsValidTyre = names(which(colSums(horizNumStintByDriverTyre[,currentRaceTyre] > 0) > 1))

		numStintByTyre = currentRaceDriverTyre %>%
							group_by(tyre) %>%
							summarise(numStint = sum(numGoodProp)) %>%
							mutate(isValidTyre = tyre %in% myIsValidTyre)
	}

	return(numStintByTyre)
}

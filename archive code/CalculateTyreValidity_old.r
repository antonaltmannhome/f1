
CalculateTyreValidity = function(modelchoice) {

	LoadAllData()

	if (modelchoice == 4) {
		allUpToDate = with(raceDF, all(doneTyreValidity4))
		dum = MakeIsGood('tyrevalidity4', lbl, raceDF)
		lbl = dum$lbl
		raceDF = dum$raceDF
		raceToUpdate = raceDF %>%
						filter(!doneTyreValidity4) %>%
						pull(race)
	}
	
	if (modelchoice == 30) {
		allUpToDate = with(raceDF, all(doneTyreValidity30))
		dum = MakeIsGood(4, lbl, raceDF)
		raceDF = dum$raceDF %>%
					rename(isValidRace4 = isValidRace)
		dum = MakeIsGood('tyrevalidity30', lbl, raceDF)
		lbl = dum$lbl
		raceDF = dum$raceDF
		raceToUpdate = raceDF %>%
						filter(!doneTyreValidity30) %>%
						pull(race)
	}
	
	for (myRace in raceToUpdate) {
	
		if (with(raceDF, !isValidRace[race == myRace])) {
			numStintByTyre = tibble(tyre = with(raceTyreDF, tyre[race == myRace]),
									numStint = 0,
									totalOverlap = 0,
									sumInternalContrast = 0,
									isValidTyre = FALSE)
		}

		if (with(raceDF, isValidRace[race == myRace])) {
			currentRaceDriverTyre = lbl %>%
										filter(race == myRace & isGood) %>%
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
				allTyreCombo = as_data_frame(t(combn(numStintByTyre$tyre, 2)))
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
		
		numStintByTyre$race = myRace
		### now store that
		if (modelchoice == 4) {
			numStintByTyre = numStintByTyre %>%
								dplyr::rename(numStint4 = numStint,
											totalOverlap4 = totalOverlap,
											sumInternalContrast4 = sumInternalContrast,
											isValidTyre4 = isValidTyre)

			raceDF[raceDF$race == myRace, 'doneTyreValidity4'] = TRUE
			sqlLazyUpdate(raceDF[raceDF$race == myRace,], 'race', 'race', 'doneTyreValidity4')
			sqlLazyUpdate(numStintByTyre,
						'racetyre',
						c('race', 'tyre'),
						c('numStint4', 'totalOverlap4', 'sumInternalContrast4', 'isValidTyre4'))
		}
		if (modelchoice == 30) {
			numStintByTyre = numStintByTyre %>%
								dplyr::rename(numStint30 = numStint,
											totalOverlap30 = totalOverlap,
											sumInternalContrast30 = sumInternalContrast,
											isValidTyre30 = isValidTyre)

			raceDF[raceDF$race == myRace, 'doneTyreValidity30'] = TRUE
			sqlLazyUpdate(raceDF[raceDF$race == myRace,], 'race', 'race', 'doneTyreValidity30')
			sqlLazyUpdate(numStintByTyre,
						'racetyre',
						c('race', 'tyre'),
						c('numStint30', 'totalOverlap30', 'sumInternalContrast30', 'isValidTyre30'))
		}
						
		message('Have processed tyre validity for ', myRace)
	}
	
}

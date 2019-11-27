WriteRaceToDatabase = function(myRace) {
	myYear = substr(myRace, 1, 4)
	raceDF = ReadF1Data(MakeYearFile(myYear, 'raceDF.csv'), 'raceDF')

	raceDF = raceDF %>%
				filter(race == myRace) %>%
				select(race, date, circuit, nlap, perim)
	sqlInsert_usedf('race', raceDF)
}

WriteDriverToDatabase = function() {
	driverAbbreviationFile = paste0(USERPATH, 'f1admin/driver-abbreviation.csv')
	driverAbbreviation = ReadF1Data(driverAbbreviationFile, 'driverAbbreviation')

	# any new drivers?
	existingDriverDF = sqlQuery2('select * from driver')
	driverAbbreviation$isNew = !driverAbbreviation$driver %in% existingDriverDF$driver
	if (any(driverAbbreviation$isNew)) {
		sqlInsert_usedf('driver', driverAbbreviation %>%
									filter(isNew) %>%
									select(forixId, driver, surname, adjustedSurname, longDriver))
	}
}

WriteRaceDriverToDatabase = function(myRace) {
	
	entryFile = MakeRaceFile(myRace, 'entryetc.csv')
	entryDF = ReadF1Data(entryFile, 'entryDF')

	entryDF$race = myRace
							  
	sqlInsert_usedf('racedriver', entryDF %>%
									select(race, driver, car, classification, officialFinPos, startingGrid) %>%
									dplyr::rename(classification = classification,
													officialfinishingposition = officialFinPos,
													startinggrid = startingGrid))
}

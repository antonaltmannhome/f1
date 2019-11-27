
ReshapeOvertakingDF = function(myOvertakingDF) {
	# think we can ditch this function, wasn't used in the end
	myOvertakingDF$tempOvertakingId = 1:nrow(myOvertakingDF)
	myVertOvertakingDF = myOvertakingDF %>%
							select(tempOvertakingId, racename, didotdriver, gototdriver) %>%
							dplyr::rename(did = didotdriver, got = gototdriver) %>%
							gather(didgot, driver, -c(tempOvertakingId, racename)) %>%
							arrange(tempOvertakingId) %>%
							mutate(isOvertaker = case_when(didgot == 'did' ~ TRUE,
															didgot == 'got' ~ FALSE)) %>%
							select(-didgot)
	## now join in the other info
	myVertOvertakingDF = subset_join(myVertOvertakingDF,
										myOvertakingDF %>%
										dplyr::rename(driver = didotdriver,
														lap = didotlap) %>%
										select(tempOvertakingId, driver, lap),
										c('tempOvertakingId', 'driver'),
										isOvertaker)
	myVertOvertakingDF = subset_join(myVertOvertakingDF,
										myOvertakingDF %>%
										dplyr::rename(driver = gototdriver,
														lap = gototlap) %>%
										select(tempOvertakingId, driver, lap),
										c('tempOvertakingId', 'driver'),
										!isOvertaker)
	myVertOvertakingDF = myVertOvertakingDF %>%
							select(racename, driver, lap, isOvertaker)
	
	return(myVertOvertakingDF)
}

	
GetActualSurname = function(myDriver) {
	driverAbbreviationFile = paste0(USERPATH, 'data/driver-abbreviation.csv')
	driverAbbreviation = ReadF1Data(driverAbbreviationFile, 'driverAbbreviation')

	myAdjustedSurname = with(driverAbbreviation, adjustedsurname[match(myDriver, driver)])
	mySurname = gsub('(^.+ )*', '', myAdjustedSurname)
	
	return(mySurname)
}


# this was from initialise-table.r
### then initialise the tables in sql...

myYear = 2018
myRaceName = '2018australia'

myRaceDF = read.csv(paste0('c:/research/lapbylap/data/', myYear,'/covsh_clean.csv'), as.is = TRUE)
myRaceDF$yr = substr(myRaceDF$rdate, 1, 4)
myRaceDF$racename = with(myRaceDF, paste0(yr, country))
myRaceDF$date = lubridate::ymd(myRaceDF$rdate)

entryDF = read.csv(paste0('c:/research/lapbylap/data/', myYear, '/', myRaceName,'/entryetc.csv'), as.is = TRUE)
entryDF$racename = myRaceName
entryDF = entryDF %>%
			dplyr::rename(startinggrid = startpos)

# we will need to chnage this soon, entryetc doesn't give us the exact info we want, but let's just test tables concept for now

allDriverDB = sqlQuery2('select * from f1.driver')

myDriverDF = inner_join(allDriverDB,
						entryDF %>%
							select(driver),by = 'driver') %>%
						select(forixid, driver, longname, adjustedsurname)

sqlInsert('race',
			myRaceDF %>%
			select(racename, date, nlap, perim))

sqlInsert('driver',
			myDriverDF)

sqlInsert('racedriver',
			entryDF %>%
			select(racename, driver, startinggrid))

TransferArchivedLapTimeCsv = function(myYear) {
	dum = list.files(paste0('c:/research/lapbylap/data/', myYear))
	allYearRaceDir = dum[grep('^[0-9]{4}[a-z]+$', dum)]
	fromfile = paste0('c:/research/lapbylap/data/', myYear, '/', allYearRaceDir, '/laptime.csv')
	tofile = gsub('c:/research/lapbylap/', 'c:/research/f1/', fromfile)
	tofile = gsub('laptime.csv', 'laptime-archived.csv', tofile)
	for (fi in 1:length(fromfile)) file.copy(fromfile[fi], tofile[fi])
}
	
TransferPitstop = function(myRaceName) {
	tofile = MakeRaceFile(myRaceName, 'pitstop-corrected.csv')
	fromfile = gsub('pitstop-corrected','pitstop_corrected',tofile)
	fromfile = gsub('f1','lapbylap',fromfile)
	b=read_csv(fromfile)
	b$penalty = b$penalty == 1
	b$replace = b$replace == 1
	b$isredflagstop = b$isredflagstop == 1
	b$isredflagstop[b$lap==0] = NA
	b = b%>%
			dplyr::rename(replaceTyre = replace,
							race = racename,
							isRedFlagStop = isredflagstop)
	# but be careful, you can have situation where a driver has officially stopped twice due to a red flag
	if (myRaceName == '2010korea') {
		b = b %>%
			filter(!(driver == 'ldigrassi' & lap == 4 & is.na(time)))
	}
	if (myRaceName == '2014greatbritain') {
		b = b %>%
			filter(!(driver == 'mchilton' & lap == 2 & is.na(time)))
	}
	# check there are on others like that
	anyDouble = b %>%
				group_by(driver, lap) %>%
				count() %>%
				ungroup() %>%
				summarise(anyDouble = any(n > 1)) %>%
				pull(anyDouble)
	if (anyDouble) {
		stop('Have got duplicated pit stop for',myRaceName,'\n')
	}
	
	write_csv(x = b, path = tofile)
}

TransferScInfo = function(myRaceName) {
	tofile = MakeRaceFile(myRaceName, 'safety-car.csv')
	fromfile = gsub('safety-car.csv', 'scinfo.dat', gsub('f1','lapbylap',tofile))
	scinfo = dget(fromfile)
	if (scinfo$scoutcount == 0) {
		safetycarDF = tibble(inout = character(0), timeElapsed = numeric(0))
	}
	if (scinfo$scoutcount > 0) {
		safetycarDF = with(scinfo, tibble(inout = c(rep('out', scoutcount), rep('in', scincount)),
						timeElapsed = c(scoutvec[1:scoutcount], scinvec[1:scincount])))
		safetycarDF = safetycarDF %>% arrange(timeElapsed)
	}
	write_csv(safetycarDF, path = tofile)
}

TransferWetInfo = function(myRaceName) {
	tofile = MakeRaceFile(myRaceName, 'wet-period.csv')
	fromfile = gsub('wet-period.csv', 'wetinfo.dat', gsub('f1','lapbylap',tofile))
	wetinfo = dget(fromfile)
	if (wetinfo$wetstartcount == 0) {
		wetPeriodDF = tibble(startEnd = character(0), timeElapsed = numeric(0))
	}
	if (wetinfo$wetstartcount > 0) {
		wetPeriodDF = with(wetinfo, tibble(startEnd = c(rep('start', wetstartcount), rep('end', wetendcount)),
						timeElapsed = c(wetstartvec[1:wetstartcount], wetendvec[1:wetendcount])))
		wetPeriodDF = wetPeriodDF %>% arrange(timeElapsed)
	}
	write_csv(wetPeriodDF, path = tofile)
}

TransferRedFlag = function(myRaceName) {
	tofile = MakeRaceFile(myRaceName,'red-flag.csv')
	fromfile = gsub('/f1/', '/lapbylap/', tofile)
	fromfile = gsub('red-flag', 'redflag', fromfile)

	if (!file.exists(fromfile)) {
		myRedFlagDF = tibble(race = character(0), lap = integer(0), driver = character(0))
	}
	if (file.exists(fromfile)) {
		myRedFlagDF = read.csv(fromfile, as.is = TRUE) %>%
						rename(race = racename)
	}
	readr::write_csv(x = myRedFlagDF, path = tofile)
}

TransferWetQualifying = function(myRaceName) {
	fromfile = 'c:/research/lapbylap/data/wetquali.csv'
	wetQualifyingDF = read.csv(fromfile, sep = '~', head = FALSE)
	names(wetQualifyingDF) = c('yr', 'country', 'session')
	wetQualifyingDF$country = gsub(' ','',wetQualifyingDF$country)
	myyr = substr(myRaceName, 1, 4)
	myCountry = substr(myRaceName, 5, nchar(myRaceName))
	wetQualyIndex = with(wetQualifyingDF, which(yr == myyr & country == myCountry))
	if (length(wetQualyIndex) == 0) {
		myWetQualyDF = tibble(race = character(0), session = integer(0))
	}
	if (length(wetQualyIndex) > 0) {
		myWetQualyDF = wetQualifyingDF[wetQualyIndex,] %>%
						mutate(race = paste0(yr, country)) %>%
						select(-c(yr, country)) %>%
						select(race, session)
	}
	fileout = MakeRaceFile(myRaceName, 'wet-qualifying.csv')
	write_csv(myWetQualyDF, path = fileout)
}

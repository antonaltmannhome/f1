### write the code to get hold of the qualifying data and the entry list

source(paste0(UPDATECODEPATH, 'fetch-data-funct.r'))

### get names from the coversheet

FetchSeasonInfo = function(myYear) {
	# have we already got this info but maybe not up to date? let's check

	YRDIRTOUSE = paste0(OUTPUTPATH, myYear)
	dircheckmake(YRDIRTOUSE)
	HTMLYRDIRTOUSE = paste0(HTMLPATH, myYear)
	dircheckmake(HTMLYRDIRTOUSE)

	raceDF = FetchSeasonInfoFromForix(myYear, YRDIRTOUSE)

	if (FALSE) {
	if (!newVersionIsLive) {
		fromfile = paste0('c:/research/f1/temporary transfer data/covsh_clean_',myYear,'.csv')
		tofile = MakeYearFile(myYear, 'raceDF.csv')
		b = read_csv(fromfile) %>%
				dplyr::rename(totalDistance = totdist,
								htmlName = htmlname,
								race = racename)
		write_csv(x = b, path = tofile)
		raceDF = ReadF1Data(MakeYearFile(myYear, 'raceDF.csv'), 'raceDF')
			
		fromfile = paste0('c:/research/f1/temporary transfer data/drivertla-override_', myYear, '.csv')
		b=read.csv(fromfile) %>%
			dplyr::rename(race = racename)
		tofile = paste0(YRDIRTOUSE, '/drivertla-override.csv')
		write_csv(x = b, path = tofile)
	}
	}
	
	InitialiseYearlyDirectory(YRDIRTOUSE, HTMLYRDIRTOUSE, raceDF)

	rawDataStatus = GetRawDataStatus(myYear)
	
	# of course, not all races will have happened yet, but will deal with that later
	raceToGet = FindRaceToGet(raceDF, rawDataStatus)

	dum = CompileWebAndFileAddress(raceToGet, raceDF, myYear)
	pagedf = dum$pagedf
	filedf = dum$filedf
	
	# now get the web pages
	for (ri in 1:length(raceToGet)) {
		autohotkeys:::visitsave(webpagelist = unlist(pagedf[ri, ]),
					                  filenamelist = MakeHtmlRaceFile(raceToGet[ri], filedf$file),
					                  browserchoice = 'Chrome')
	}
	
	if (FALSE) {
	if (!newVersionIsLive) {
		haveGapsChart = f1admin::CheckYearAttribute(myYear, 'haveGapsChart')
		haveArchivedLapTimeCsv = f1admin::CheckYearAttribute(myYear, 'haveArchivedLapTimeCsv')
		if (!haveGapsChart & haveArchivedLapTimeCsv) {
			TransferArchivedLapTimeCsv(myYear)
		}
	}
	
	### NB just copy them from where we've already got them obviously
	if (FALSE) {
	if (!newVersionIsLive) {
	print('Disable this section of code once new version is live!')
	for (ri in 1:length(raceToGet)) {
		tofile = MakeHtmlRaceFile(raceToGet[ri], filedf$file)
		fromfile = gsub('/f1/', '/lapbylap/', tofile)
		for (j in 1:length(tofile)) file.copy(fromfile[j], tofile[j], overwrite = TRUE)
	}
	}
	}
	}
	#### then first we need to strip the entry page in order to facilitate the fetching of the drivers pages
	
	for (ri in 1:length(raceToGet)) {
		entryDF = StripEntryEtc(raceToGet[ri])
	}
	
	for (ri in 1:length(raceToGet)) {
		entryDF = ReadF1Data(MakeRaceFile(raceToGet[ri], 'entryetc.csv'), 'entryDF')
		myHtmlName = with(raceDF, htmlName[race == raceToGet[ri]])
		FetchDriverRacePage(raceToGet[ri], entryDF, myHtmlName)
	}
	
	for (ri in 1:length(raceToGet)) {
		entryDF = ReadF1Data(MakeRaceFile(raceToGet[ri], 'entryetc.csv'), 'entryDF')
		StripPitStop(raceToGet[ri], entryDF)
		StripLapTime(raceToGet[ri])
		StripTyreSpeedtrap(raceToGet[ri])
		rawDataStatus$fetched[rawDataStatus$race == raceToGet[ri]] = TRUE
	}
	
	UpdateRawDataStatus(myYear, rawDataStatus)
}

### i think this should be split into two files when i'm less sick of looking at it and/or have an actual race to download. There should be 'fetch-data' and 'strip data' separately

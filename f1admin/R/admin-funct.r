#' @export
MakeRaceFile=function(myRace ,myFile) {
	### don't want to have to put the ugly pasting line in every time we write some data relating to a race to disk, this will do all of that
	if (any(!grepl('\\..+$',myFile))) {
		stop('You don\'t seem to have included a .dat or similar at the end of the file, exiting...\n')
	}
	myYear = substr(myRace, 1, 4)
	fileout=paste0(OUTPUTPATH, myYear, '/', myRace, '/', myFile)
	return(fileout)
}

MakeHtmlRaceFile=function(myRace ,myFile) {
	### don't want to have to put the ugly pasting line in every time we write some data relating to a race to disk, this will do all of that
	if (any(!grepl('\\..+$',myFile))) stop('You don\'t seem to have included a .dat or similar at the end of the file, exiting...\n')
	myYear = substr(myRace, 1, 4)
	fileout=paste0(HTMLPATH, myYear, '/', myRace, '/', myFile)
	return(fileout)
}

#' @export
MakeYearFile=function(myYear, myFile) {
	### don't want to have to put the ugly pasting line in every time we write some data relating to a race to disk, this will do all of that
	if (any(!grepl('\\..+$',myFile))) {
		stop('You don\'t seem to have included a .dat or similar at the end of the file, exiting...\n')
	}
	fileout=paste0(OUTPUTPATH, myYear, '/', myFile)
	return(fileout)
}

#' @export
ReadF1Data = function(myFile, myRName) {
	columnGuideFileName = paste0(USERPATH, 'f1admin/file-column-guide.csv')
	columnGuideDF = read_csv(columnGuideFileName,
						col_types = list(
									rName = col_character(),
									colName = col_character(),
									colType = col_character())) %>%
						filter(rName == myRName)

	rCommand1 = paste0('read_csv("', myFile, '", col_types = list(')
	rCommand2 = paste(with(columnGuideDF, paste(colName, colType, sep = ' = ')), collapse = ', ')
	rCommand3 = '))'
	rCommand = paste0(rCommand1, rCommand2, rCommand3)

	myDF = eval(parse(text = rCommand))
	return(myDF)
}

UpdateDatabase = function() {
	write(file=paste(SQLPATH,'temp.bat',sep=''),'mysqldump --login-path=local cleanf1 > "d:\\\\f1output\\\\cleanf1.sql"')
	Sys.sleep(1)
	system(paste(SQLPATH,'temp.bat',sep=''))
	print('Have just updated the database backup')
	}

CheckDatabaseUpToDate = function() {

	### do check to see that database is up to date, easy to forget to update it
	databaseRaceDate = lubridate::ymd(sqlQuery2('select date from race')$date)
	databaseMaxDate = max(databaseRaceDate)
	dum=list.files(OUTPUTPATH)
	currentMaxYear = max(as.numeric(dum[grep('^2[0-9]{3}$',dum)]))
	currentMaxYearRaceFile = MakeYearFile(currentMaxYear, 'raceDF.csv')
	currentYearRaceDF = ReadF1Data(currentMaxYearRaceFile, 'raceDF')
	dum = list.files(paste0(OUTPUTPATH, currentMaxYear), rec = TRUE)
	raceDir = gsub('/.+$', '', dum[grep('entryetc', dum)])
	fileMaxDate = max(currentYearRaceDF$date[match(raceDir, currentYearRaceDF$race)])

	if (fileMaxDate > databaseMaxDate) {
		cat('Warning:\nlatest file date is',as.character(fileMaxDate),'
			latest database date is', as.character(databaseMaxDate),'\n')
	}
}

#' @export
GetRawDataStatus = function(myYear) {
	raceDFFile = MakeYearFile(myYear, 'raceDF.csv')
	raceDF = ReadF1Data(raceDFFile, 'raceDF')
	rawDataStatusFile = MakeYearFile(myYear, 'raw-data-status.csv')
	if (!file.exists(rawDataStatusFile)) {
		rawDataStatus = tibble(race = raceDF$race,
								fetched = FALSE,
								augmented = FALSE,
								writtenToDatabase = FALSE)
		write_csv(rawDataStatus, file = rawDataStatusFile)
	}

	rawDataStatus = ReadF1Data(rawDataStatusFile, 'rawDataStatus')

	return(rawDataStatus)
}

UpdateRawDataStatus = function(myYear, rawDataStatus) {
	rawDataStatusFile = MakeYearFile(myYear, 'raw-data-status.csv')
	write_csv(rawDataStatus, file = rawDataStatusFile)
}

CreateModelColumn = function() {
	# likely use case is that you want to add new ones and leave the existing ones as they are
	# so let's just see which ones need to be added
	modelColumnDetail = ReadF1Data(paste0(USERPATH, 'f1admin/model-column-guide.csv'), 'modelColumnDetail')
	modelColumnDetail$toAdd = FALSE
	uniqueTable = unique(modelColumnDetail$table)
	for (myTable in uniqueTable) {
		currentColumn = sqlQuery2(paste('desc', myTable)) %>% pull(Field)
		columnToAdd = setdiff(modelColumnDetail %>% filter(table == myTable) %>% pull(column),
								currentColumn)
		modelColumnDetail$toAdd[with(modelColumnDetail, table == myTable & column %in% columnToAdd)] = TRUE
	}
	if (sum(modelColumnDetail$toAdd) == 0) {
		message('No new columns to add')
	}
	if (sum(modelColumnDetail$toAdd) > 0) {
		sqlComm = with(modelColumnDetail %>% filter(toAdd),
					paste('alter table', table,'add column', column,type,initialStatus, ';'))
		message('Here is the sql code to paste into mySql:')
		cat(sqlComm, sep = '\n')
	}
}

DeleteModelColumn = function() {
	modelColumnDetail = ReadF1Data(paste0(USERPATH, 'f1admin/model-column-guide.csv'), 'modelColumnDetail')
	sqlComm = with(modelColumnDetail,
					paste('alter table', table,'drop column', column, ';'))
	message('Here is the SQL code that deletes the columns, select the ones you want carefully!')
	cat(sqlComm, sep = '\n')
}

ResetModelForRace = function(myModelStage, myRaceList) {
	### this is for when you want to undo a race update without having to reset everything from the beginning
	modelColumnDetail = ReadF1Data(paste0(USERPATH, 'f1admin/model-column-guide.csv'), 'modelColumnDetail')
	if (!myModelStage %in% modelColumnDetail$modelStage) {
		stop('No modelstage is called that, exiting\n')
	}
	myColumnToReset = modelColumnDetail %>%
						filter(modelStage == myModelStage)
	charRaceList = paste0('(\'',paste(myRaceList, collapse = '\',\''), '\')')
	sqlComm = with(myColumnToReset, paste(
						'update',table,'set',column,'=',gsub('default','',initialStatus),
						'where race in', charRaceList, ';'))

	for (si in 1:length(sqlComm)) {
		sqlQuery2(sqlComm[si], retval = FALSE)
	}

	# but we need to make a special case for these next ones, because we need to delete rather than overwrite all the data with defaults, as is the case for all other models
	if (myModelStage == 'postDeltaOvertaking') {
		sqlComm = paste('delete from overtaking where race in', charRaceList,';')
		sqlQuery2(sqlComm, retval = FALSE)
	}
	if (myModelStage == 'possibleOvertaking') {
		sqlComm = paste('delete from possibleovertaking where race in', charRaceList,';')
		sqlQuery2(sqlComm, retval = FALSE)
	}
	if (myModelStage == 'simulation') {
		sqlComm = paste('delete from finishingpositionprobability where race in', charRaceList,';')
		sqlQuery2(sqlComm, retval = FALSE)
	}
}

CheckYearAttribute = function(myYear, myAttribute) {
	yearGuideDF = ReadF1Data(paste0(USERPATH, 'f1admin/year-guide.csv'), 'yearGuide')
	myValue = yearGuideDF %>%
				filter(year == myYear) %>%
				select(myAttribute) %>%
				pull(myAttribute)
	return(myValue)
}

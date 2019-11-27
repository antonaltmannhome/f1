rm(list=ls())

DATAPATH = 'c:/research/f1/data/'
source('c:/research/utils/sql/sqlfunct.r')
SQLPATH='c:/temp/sqlscript/'
library(RODBC)
library(dplyr)
mySqlDbName='cleanf1'
odbcConnection=odbcConnect('cleanf1')

DeleteAllTable = function() {
	tableList=sqlQuery2('show tables', retval = TRUE)
	tableToInitialise = c('qualifying', 'qualifyingsession', 'overtaking', 'stint', 'pitstop', 'racedriverlap', 'racetyre', 'racedriver', 'race', 'driver')
	for (myTable in tableToInitialise) {
		if (myTable %in% tableList$Tables_in_cleanf1) {
			sqlQuery2(paste('drop table', myTable), retval = FALSE)
		}
	}
}

### might well need to correct rawdatastatus in light of this
source('c:/research/f1/f1-startup.r')
allYear = list.files('c:/research/f1/data/')
allYear=allYear[grep('^[0-9]{4}$', allYear)]

for (yi in 1:length(allYear)) {
	rawDataStatusFile = MakeYearFile(allYear[yi], 'raw-data-status.csv')
	rawDataStatus = ReadF1Data(rawDataStatusFile, 'rawDataStatus')
	# rawDataStatus$fetched = FALSE
	# rawDataStatus$augmented = FALSE
	rawDataStatus$writtenToDatabase = FALSE
	write_csv(x = rawDataStatus, path = rawDataStatusFile)
}

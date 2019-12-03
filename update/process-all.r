source('c:/git/f1/f1-startup.r')

newVersionIsLive = TRUE
source(paste0(UPDATECODEPATH, 'fetch-data.r'))
source(paste0(UPDATECODEPATH, 'augment-raw-data.r'))
source(paste0(UPDATECODEPATH, 'transfer-data-to-database.r'))

myYear = 2019

FetchSeasonInfo(myYear)
AugmentRawData(myYear)
TransferDataToDatabase(myYear)

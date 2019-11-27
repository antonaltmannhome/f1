source(paste0(UPDATECODEPATH, 'augment-raw-data-funct.r'))
source(paste0(UPDATECODEPATH, 'reshape-raw-data-funct.r'))
if (!newVersionIsLive) source('c:/research/f1/temporary resetting code/transfer-raw-data-funct.r')

AugmentRawData = function(myYear) {
	
	rawDataStatus = GetRawDataStatus(myYear)

	raceToAugment = with(rawDataStatus, race[fetched & !augmented])
	for (myRace in raceToAugment) {
		if (newVersionIsLive) ProcessRedFlag(myRace)
		if (!newVersionIsLive) TransferRedFlag(myRace)
		if (newVersionIsLive) ProcessPitstop(myRace, myYear)
		if (!newVersionIsLive) TransferPitstop(myRace)
		if (newVersionIsLive) ProcessSafetyCar(myRace)
		if (!newVersionIsLive) TransferScInfo(myRace)
		if (newVersionIsLive) ProcessWetQualifying(myRace)
		if (!newVersionIsLive) TransferWetQualifying(myRace)
		if (newVersionIsLive) ProcessWetRace(myRace)
		if (!newVersionIsLive) TransferWetInfo(myRace)
		rawDataStatus$augmented[rawDataStatus$race == myRace] = TRUE
	}
	
	UpdateRawDataStatus(myYear, rawDataStatus)	
}

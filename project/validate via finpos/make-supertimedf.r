## let's get hold of supertimes, see how they do

LoadAllData()

sessionTimeDF = rddf[,c('race', 'driver')]
allSessionLabel = c("1stFree", "2ndFree", "3rdFree", "1stQualif", "2ndQualif", "3rdQualif")
sessionTimeDF[,allSessionLabel] = NA

dataIsOkIndex = with(rddf, which(!is.na(startingGrid)))
for (rdi in dataIsOkIndex) {
	myRaceDriverHtmFile = MakeHtmlRaceFile(rddf$race[rdi], paste0('driverrace/', rddf$driver[rdi], '.htm'))
	myHtm = scan(myRaceDriverHtmFile, what = '', quiet = TRUE, sep = '\n')

	practiceQualTimeLineIndex = grep('Time.+km/h.+(1st Free|2nd Free|3rd Free|1st Qualif|2nd Qualif|3rd Qualif)', myHtm)
	
	practiceQualTimeMessy = strsplit(myHtm[practiceQualTimeLineIndex], split = '>')[[1]]
	
	sessionLineIndex = grep('1st Free|2nd Free|3rd Free|1st Qualif|2nd Qualif|3rd Qualif', practiceQualTimeMessy)
	
	mySessionLabel = gsub('[^0-9a-zA-Z]', '', gsub('<.+$', '', practiceQualTimeMessy[sessionLineIndex]))
	mySessionTime = gsub('<.+$', '', practiceQualTimeMessy[sessionLineIndex + 9])
	mySessionSec = rep(NA, length(mySessionTime))
	
	for (j in 1:length(mySessionTime)) {
		mySessionSec[j] = ConvertStringTimeToSec(mySessionTime[j])
	}
	
	sessionTimeDF[rdi,match(mySessionLabel, names(sessionTimeDF))] = mySessionSec
	
	if ( (rdi %% 20) == 0) {
		message('Have processed ', paste(rddf[rdi, c('race', 'driver')], collapse= ', '))
	}
}

write.csv(file = paste0(USERPATH, 'project/validate via finpos/session-time.csv'),
			sessionTimeDF,
			row.names = FALSE)

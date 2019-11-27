for (ri in 1:nrace) {
	fromFile = paste0('c:/research/lapbylap/data/',raceDF$year[ri],'/',raceDF$race[ri],'/pitstop_interrupt.csv')
	toFile = paste0('c:/research/f1/data/',raceDF$year[ri],'/',raceDF$race[ri],'/pitstop-interrupt.csv')
	b = read.csv(fromFile, as.is = TRUE)
	b$stint = stintDF$stint[match(paste(b$racename, b$driver, b$lap), with(stintDF, paste(race, driver, endLap)))]
	b$interrupt = b$interrupt == 1
	b = b %>% select(driver, stint, interrupt)
	write_csv(x = b, path = toFile)
}

	
if (FALSE) {
	fromFile = paste0('c:/research/lapbylap/data/',raceDF$year[ri],'/',raceDF$race[ri],'/guessed_pitstop.csv')
	toFile = paste0('c:/research/f1/data/',raceDF$year[ri],'/',raceDF$race[ri],'/guessed-pitstop.csv')
	if (file.exists(fromFile)) {
		b = read.csv(fromFile, as.is = TRUE)
		b = b %>%
			rename(vantageStint = vantagestint,
					newTyre = newtyre)
		# get rid of silly half laps
		b$lap = round(b$lap)
		write_csv(x = b, path = toFile)
	}

	# don't copy the alternative strategy one, we want to remake those to account for some previous errors
	message('Have copied over data for ', raceDF$race[ri])
}


DisplayStrategy=function(messyDriverStintDF) {
	### gets a subset of stintdb and displays it prettily
	
	### add labels for thigns that are either unusual or not pit stops
	messyDriverStintDF = messyDriverStintDF %>%
							mutate(endLapPlus = 
									case_when(!is.na(interrupt) & interrupt & !(isRed | isSafetyCar | stintRetired | stintFinishRace) ~ paste(endLap, 'IRPT'),
												isRed ~ paste(endLap, 'RED'),
												isSafetyCar ~ paste(endLap, 'SC'),
												stintRetired ~ paste(endLap, 'DNF'),
												stintFinishRace ~ paste(endLap, 'FIN'),
												TRUE ~ as.character(endLap)))
	
	### now compress into one row for each driver
	myStratDF = messyDriverStintDF %>%
					mutate(stintLabel = paste0(tyre, '(', endLapPlus, ')')) %>%
					select(race, driver, stint, stintLabel) %>%
					spread(key = stint, value = stintLabel, sep = '') %>%
					lazy_left_join(rddf, c('race', 'driver'), c('team','startingGrid')) %>%
					arrange(startingGrid) %>%
					select(driver, team, contains('stint'))
	return(myStratDF)
}

ViewStrategy = function(myRace) {
	### get table giving guide to what strategies were actually used
	myStintDF=stintDF[stintDF$race == myRace,]
	myStintDF = lazy_left_join(myStintDF, rddf, c('race', 'driver'), c('classification', 'startingGrid'))
	### EDIT, no do display them but after the first bunch of drivers
	ismessystintsax=NULL
	iscleanstintsax=NULL
	commonstrategydf=NULL
	classifiedDriver = myStintDF %>% filter(!is.na(classification)) %>% distinct(driver) %>% pull(driver)
	myStintDF = left_join(myStintDF,
							myStintDF %>%
							group_by(driver) %>%
							summarise(notInterrupted = all(!interrupt | is.na(interrupt))),
							'driver')
	if (sum(myStintDF$notInterrupted) == 0) {
		cat('No drivers had uninterrupted stints!\n')
	}
	if (sum(myStintDF$notInterrupted) > 0) {
		uninterruptedDriver = myStintDF %>% filter(notInterrupted) %>% distinct(driver) %>% pull(driver)
		cleanDriver = intersect(classifiedDriver, uninterruptedDriver)
		messyDriver = setdiff(myStintDF$driver, cleanDriver)
	}
	
	messyStratDF=NULL
	if (length(messyDriver) > 0) messyStratDF=DisplayStrategy(myStintDF %>% filter(driver %in% messyDriver))
		
	cleanStratDF=NULL
	### no point trying to guess at most likley strategy unleast at least a few drivers had clean races
	cleanstrategydriv=unique(myStintDF[iscleanstintsax,'driver'])
	if (length(cleanDriver) > 3) {
		
		cleanStratDF=DisplayStrategy(myStintDF %>% filter(driver %in% cleanDriver))
		
		driverStratDF=myStintDF %>% filter(driver %in% cleanDriver) %>% group_by(driver) %>% summarise(stratLabel=paste(tyre,collapse=','))
		stratCount=driverStratDF %>% group_by(stratLabel) %>% summarise(stratCount=length(driver), nstint=length(strsplit(stratLabel[1],split=',')[[1]]))
		
		### but would be handy to have the median lap stopped too for each strategy - only d for one with at least two drivers doing it
		stratCount = stratCount %>% filter(stratCount>=2)
		
		### that might leave us with no strategies, no problem, just return null
		if (nrow(stratCount)>0) {
			### but we want estimated lap for tyre stops too
			commonstrategydf=with(stratCount,data.frame(stratnumber=rep(1:length(stratLabel),nstint),numob=rep(stratCount,nstint)))
			commonstrategydf=commonstrategydf %>% group_by(stratnumber) %>% mutate(stopnumber=1:length(stratnumber))
			commonstrategydf$tyre=unlist(strsplit(stratCount$stratLabel,split=','))
			commonstrategydf$endLap=NA
			for (si in 1:nrow(stratCount)) {
				sax2=which(driverStratDF$stratLabel==stratCount$stratLabel[si])
				### now have to find the lap stopped at for each driver
				for (k in 1:stratCount$nstint[si]) {
					dum=myStintDF[match(paste(driverStratDF$driver[sax2],k),paste(myStintDF$driver,myStintDF$stint)),'endLap']
					#horizcommonstrategydf[si,k]=median(dum)
					sax=with(commonstrategydf,which(stratnumber==si & stopnumber==k))
					commonstrategydf$endLap[sax]=median(dum)
				}
			}
		}
	}
	return(list(messystrategydf=messystrategydf, cleanStratDF=cleanStratDF, commonstrategydf=commonstrategydf))
}

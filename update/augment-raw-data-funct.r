ProcessPitstop = function(myRace, myYear) {
	
	pstab=read.csv(MakeRaceFile(myRace, 'pitstop_clean.csv'),sep=',',as.is=T)
	names(pstab)=c('lap','driver','time','penalty')
	# let's make penalty a logical, as it should be
	pstab$penalty = (pstab$penalty == 1)
	
	### occasional forix data cockups, so override those here
	pstab = .ProcessPitstop.InsertMissingStop(myRace, pstab)
	
	if (any(is.na(pstab[,'time']))) stop('Some NAs for pit stops times, correct this\n')
	
	# for old seasons, forix didn't always add the red flag tyre changes by default, need to do it ourselves
	forixAddsRedFlagPitStops = f1admin::CheckYearAttribute(myYear, 'forixAddsRedFlagPitStops')
	# annoyingly that's not consistent for 2014 though
	if (myYear == 2014 & myRace == '2014japan') {
		forixAddsRedFlagPitStops = TRUE
	}
	if (!forixAddsRedFlagPitStops) {
		pstab = .ProcessPitstop.InsertRedFlagTyreChange(myRace, pstab)
	}
	if (forixAddsRedFlagPitStops) {
		pstab = .ProcessPitstop.IndicateRedFlagTyreChange(myRace, pstab)
	}
		
	pstab=pstab[order(pstab$driver,as.numeric(pstab$lap)),]
	
	### but we want to include all info about drivers, so get hold of full field
	filein=MakeRaceFile(myRace, 'entryetc.csv')
	rtab=read.csv(filein,as.is=T)
	### NB no point having drivers who didn't qualify or start the race
	keep=which(!is.na(rtab$class) & !rtab$class%in%c('Withdrawn','Not started'))
	rtab=rtab[keep,]
	
	## insert lap 1 for all drivers
	lap1pstab = data.frame(driver = rtab$driver,
								lap = 0,
								time = NA,
								isRedFlagStop = FALSE,
								penalty = FALSE,
								tyre = NA,
								UN = NA,
								replaceTyre = NA)
	augmentedpstab = bind_rows(lap1pstab, pstab) %>%
							arrange(driver, lap)


	### but are there any suspiciously low pit stops times? if so, assume they are penalties
	medianpitstoptime = with(augmentedpstab, median(time[!is.na(time) & !isRedFlagStop & !penalty]))
	### but are there any suspiciously low pit stops times? if so, assume they are penalties
	yellowCircleForPitStopPenalty = f1admin::CheckYearAttribute(myYear, 'yellowCircleForPitStopPenalty')
	if (yellowCircleForPitStopPenalty) {
		sax=which(augmentedpstab$time<medianpitstoptime-4 & !augmentedpstab$penalty)
	}
	if (!yellowCircleForPitStopPenalty) {
		# set all non weird looking ones to zero
		augmentedpstab$penalty = 0
		sax=which(augmentedpstab$time<medianpitstoptime-4)
	}

	if (length(sax)>0) {
		print(augmentedpstab[sax,c('racename','driver','lap','time')])
		cat('Median pit stop time is',medianpitstoptime,'\n')
		print('Are you happy to assume these are all penalty laps?')
		dum=askcond(F,F)
		if (dum %in% c('y','Y')) {
			augmentedpstab[sax,"penalty"]=TRUE
		}
		if (!dum %in% c('y','Y')) {
			stop('ok, look into it\n')
		}
	}
	
	## let us now get hold of the tyre types
	filein = MakeRaceFile(myRace, 'tyre-stint.csv')
	tyreStint=read.csv(filein,as.is=T)
	myundriv=unique(augmentedpstab$driver)
	dum=unique(tyreStint$tyre)
	if (all(isgrep('/[UN]$',dum))) {
		myuntyre=unique(gsub(' ','',tolower(gsub('/[UN]$','',dum))))
	}
	if (!all(isgrep('/[UN]$',dum))) {
		myuntyre=gsub(' ','',tolower(dum))
	}
	
	for (k in 1:length(myundriv)) {
		sax=which(augmentedpstab$driver==myundriv[k])
		dum=with(tyreStint, tyre[driver == myundriv[k]])
		### do we have new or used info? if so, let's have it
		if (all(isgrep('/[UN]$',dum))) {
			ttype=gsub(' ','',tolower(gsub('/[UN]$','',dum)))
			unstat=gsub('.+/','',dum)
		}
		if (!all(isgrep('/[UN]$',dum))) {
			ttype=gsub(' ','',tolower(dum))
			unstat=rep(NA,length(ttype))
		}
		### now fill in the gaps, a bit fiddly when there's a penalty
		### but you can take penalties at the same time as pitstops from 2014, so need to tweak the code
		couldChangeTyresAndServePenalty = f1admin::CheckYearAttribute(myYear, 'couldChangeTyresAndServePenalty')
		if (!couldChangeTyresAndServePenalty) {
			pdum=augmentedpstab$penalty[sax]
			stopix=which(!pdum | is.na(pdum))
		}
		if (couldChangeTyresAndServePenalty) {
			pdum=1:length(sax)
			stopix=pdum
		}
		dum=rep(1:length(stopix),diff(c(stopix,length(pdum)+1)))
		if(length(stopix)==length(ttype)) {
			augmentedpstab$tyre[sax]=ttype[dum]
			augmentedpstab$UN[sax]=unstat[dum]
			if (length(sax)>1) augmentedpstab$replaceTyre[sax[2:length(sax)]]=1
		}
		if (length(stopix)!=length(ttype)) {
			cat('Can\'t quite match up the tyre types for',toupper(myundriv[k]),'in',toupper(myRace),'\n')
			### we can fill in the first one ourselves
			augmentedpstab$tyre[sax[1]]=ttype[1]
			augmentedpstab$UN[sax[1]]=unstat[1]
			### display all pitstop info, makes it a little easier
			cat('Here is all the pitstop info we currently have:\n')
			print(augmentedpstab[sax,])
			cat('And forix lists the driver\'s tyre sequence as:\n')
			print(paste(ttype,unstat,sep='~'))
			for (j in sax[2:length(sax)]) {
				### if it's a penalty pitstop, no need to ask user anything - provided it's not 2014 onwards...
				if (!couldChangeTyresAndServePenalty) {
					check=T
					if (augmentedpstab$penalty[j]) check=F
				}
				if (couldChangeTyresAndServePenalty) {
					check=T
				}
				
				if (check) {
					augmentedpstab$tyre[j]=augmentedpstab$tyre[j-1]
					augmentedpstab$UN[j]=augmentedpstab$UN[j-1]
					augmentedpstab$replaceTyre[j]=0
					replacecheck=T
				}
				if (check) {
					cat('Please enter the tyre that was put on the car during this stop:\n')
					print(augmentedpstab[j,])
					print(paste(1:length(myuntyre),myuntyre,sep=':'))
					satis=F
					while(!satis) {
						tdum=askcond(T,F)
						if (tdum<1 | tdum>length(myuntyre)) {
							print('Invalid entry, please enter again')
						}
						if (tdum>=1 & tdum<=length(myuntyre)) {
							satis=T
							next
						}
					}
					augmentedpstab$tyre[j]=myuntyre[tdum]
					
					replacecheck=F
					if ( ( (!couldChangeTyresAndServePenalty & !augmentedpstab$penalty[j]) |
								couldChangeTyresAndServePenalty) &
							augmentedpstab$tyre[j]==augmentedpstab$tyre[j-1]) {
						cat('And did this stop involve replacing the tyres?\n')
						dum=askcond(F,F)
						augmentedpstab$replaceTyre[j]=ifelse(dum %in% c('y','Y'),1,0)
						replacecheck=T
					}
					if (replacecheck==F & augmentedpstab$tyre[j]!=augmentedpstab$tyre[j-1]) {
						augmentedpstab$replaceTyre[j]=1
						replacecheck=F
					}
					if (!all(is.na(unstat))) {
						cat('Also, please enter whether the tyre was U or N\n')
						augmentedpstab$UN[j]=askcond(F,F)
					}
					print(augmentedpstab[sax,])
					print(paste(ttype,unstat,sep='~'))
				}
			}
		}
	}
	
	### penalty pitstops don't involve tyre changes of course
	### no, that's true for up to 2013, not from 2014 onwards necessarily - will have to develop that as the penalities build up
	if (!couldChangeTyresAndServePenalty) augmentedpstab$replaceTyre[which(augmentedpstab$penalty)]=0
	
	pstab4 = augmentedpstab %>%
				mutate(replaceTyre =  (replaceTyre == 1))
	
	### write that to a summary file because it's a total pain in the arse to have to redo
	write_csv(pstab4, path=MakeRaceFile(myRace, 'pitstop-corrected.csv'))
}

.ProcessPitstop.InsertMissingStop = function(myRace, pstab) {
	pitStopCorrectionDF = ReadF1Data(paste0(OUTPUTPATH, 'pit-stop-correction.csv'), 'pitStopCorrection')
	if (any(pitStopCorrectionDF$race == myRace)) {
		myPitStopCorrectionDF = pitStopCorrectionDF %>%
								filter(race == myRace)
		for (i in 1:nrow(myPitStopCorrectionDF)) {
			if (myPitStopCorrectionDF$insertOrReplace[i] == 'insert') {
				#stop('Not tested this yet, check it is ok\n')
				pstab = bind_rows(pstab,
									myPitStopCorrectionDF[i,] %>%
										select(lap, driver, time, penalty))
			}
			if (myPitStopCorrectionDF$insertOrReplace[i] == 'replace') {
				toOverWrite = with(pstab, which(driver == myPitStopCorrectionDF$driver[i] &
												lap == myPitStopCorrectionDF$lap[i]))
				pstab[toOverWrite,c('time', 'penalty')] = with(myPitStopCorrectionDF[i,], c(time, penalty))
			}
		}
	}
	return(pstab)
}

.ProcessPitstop.IndicateRedFlagTyreChange = function(myRace, pstab) {
	redFlagFile = MakeRaceFile(myRace, 'red-flag.csv')
	redFlagDF = ReadF1Data(redFlagFile, 'redFlagDF')
	pstab$templap = pstab$lap + 1
	sax0 = which(with(pstab, paste(driver, lap)) %in% with(redFlagDF, paste(driver, lap)))
	sax1 = which(with(pstab, paste(driver, templap)) %in% with(redFlagDF, paste(driver, lap)))
	
	pstab[,'isRedFlagStop'] = FALSE
	if (length(sax0) > 0) {
		pstab[sax0,'isRedFlagStop'] = TRUE
	}
	if (length(sax1) > 0) {
		pstab[sax1,'isRedFlagStop'] = TRUE
		pstab[sax1,'lap'] = pstab[sax1,'templap']
	}
	pstab = pstab %>% select(-templap)
	print(pstab)
	message('Does it look like it has identified the red flag stops correctly?')
	allcorrect = askcond(F,F)
	if (allcorrect != 'y') {
		redflagcheckfile = 'c:/temp/redflag-pitstop-check.csv'
		if (file.exists(redflagcheckfile)) file.remove(redflagcheckfile)

		message('ok, i am writing the pitstops to c:/temp/redflag-pitstop-check.csv, apply any corrections then press enter')
		write.csv(file = redflagcheckfile, pstab, row.names = FALSE)
		dum = askcond(F,T)
		pstab = read.csv(redflagcheckfile, as.is = TRUE)
	}
	return(pstab)
}

.ProcessPitstop.InsertRedFlagTyreChange = function(myRace, pstab) {
	
	redFlagFile = MakeRaceFile(myRace, 'red-flag.csv')
	redFlagDF = ReadF1Data(redFlagFile, 'redFlagDF')

	pstab$isRedFlagStop = FALSE
	
	if (nrow(redFlagDF) > 0) {
		redFlagTyreChange = redFlagDF %>%
								mutate(time = NA, penalty = NA) %>%
								select(lap, driver, time, penalty)
		# but hold on, sometimes a driver will have stopped for tyres on that lap anyway, so we remove their extra pit stop
		redFlagTyreChange = indicate_overlapping_combination(
									redFlagTyreChange,
									pstab,
									c('driver', 'lap'),
									'driverAlreadyStopped')
		redFlagTyreChange = redFlagTyreChange %>%
								filter(!driverAlreadyStopped) %>%
								select(-driverAlreadyStopped) %>%
								mutate(isRedFlagStop = TRUE)
		pstab = bind_rows(pstab,
							redFlagTyreChange)
	}
	return(pstab)
}

ProcessRedFlag = function(myRace) {
	
	mylapbylap=read.csv(MakeRaceFile(myRace, 'laptime.csv'),as.is=T)
		#laptimefile = makeracefile(myracename = myracename, myfilename = 'laptime.csv')
	
	### need to allocate the lap times into leader laps
	mylapbylap$isred = FALSE
	mylapbylap = mylapbylap %>%
					group_by(driver) %>%
					arrange(lap) %>%
					mutate(endtelapse = cumsum(sec),
							starttelapse = lag(endtelapse)) %>%
					ungroup() %>%
					group_by(lap) %>%
					mutate(isleader = starttelapse == min(starttelapse)) %>%
					ungroup()
	mylapbylap = mylapbylap %>%
					mutate_cond(lap == 1,
								starttelapse = 0)
					
	sortedendtelapse = sort(mylapbylap$endtelapse)
	telapsedelta = diff(sortedendtelapse)
	bigdelta = which(telapsedelta > 300)

	if (length(bigdelta) > 0) {
		# let's suggest there was a red flag and ask user to confirm
		for (si in 1:length(bigdelta)) {
			rftime = with(mylapbylap, min(endtelapse[endtelapse > sortedendtelapse[bigdelta[si]]])) - 0.1
			### so, what is first lap by each driver still in race after this point?
			leaderrestarttime = 
			firstlapafterrftimebydriver = mylapbylap %>%
											filter(endtelapse > rftime) %>%
											group_by(driver) %>%
											summarise(lap = min(lap))
			firstlapafterrftimebydriver = lazy_left_join(firstlapafterrftimebydriver,
															mylapbylap,
															c('driver', 'lap'),
															c('starttelapse', 'isleader', 'endtelapse'))
			leaderrestarttime = semi_join(mylapbylap,
											firstlapafterrftimebydriver %>%
											filter(isleader) %>%
											mutate(lap = lap+1),
											c('driver','lap')) %>%
											pull(endtelapse) - 1E-03
			# then display something nice for the user to confirm
			
			mylapbylap = mylapbylap %>%
							mutate_cond(between(endtelapse, rftime, leaderrestarttime),
										isred = TRUE)
			
			currentrflapbylapdf = mylapbylap %>%
									filter(between(endtelapse, rftime, leaderrestarttime)) %>%
									select(driver, lap)
			
			lapbeforerf = inner_join(mylapbylap,
										currentrflapbylapdf %>%
											group_by(driver) %>%
											summarise(lap = min(lap)) %>%
											mutate(lap = lap-1),
										c('driver','lap'))
			lapofrf = inner_join(mylapbylap,
										currentrflapbylapdf,
										c('driver','lap'))
			lapafterrf = inner_join(mylapbylap,
										currentrflapbylapdf %>%
											group_by(driver) %>%
											summarise(lap = max(lap)) %>%
										mutate(lap = lap+1),
										c('driver','lap'))
			laparoundrf = bind_rows(lapbeforerf, lapofrf, lapafterrf) %>%
							arrange(starttelapse) %>%
							select(starttelapse, lap, driver, isleader, sec, endtelapse, isred)
			
			print(laparoundrf)
			message('A red flag has been detected, does this look correct (y/n)?')
			dum = askcond(F, F)
			if (dum != 'y') {
				stop('Ok, you need to look into that\n')
			}
		}
	}
	
	mylapbylap$race = myRace
	rfinfo = mylapbylap %>%
				filter(isred) %>%
				select(race, lap, driver)

	### any more than 5 minutes long? must be a red flag
	
	fileout = MakeRaceFile(myRace, 'red-flag.csv')
	write_csv(rfinfo, path = fileout)
}

ProcessSafetyCar.plotsec=function(sax, lbl, myRace, xlim=NULL, ovylim, safetycarDF) {
	plot(lbl$endTimeElapsed[sax],lbl$sec[sax],xlim=xlim,ylim=c(ovylim[1],ovylim[2]),xlab='tempTimeElapsed',ylab='lap time',main=myRace)
	### highlight in and out laps
	points(lbl$endTimeElapsed[sax][lbl$inlap[sax]],lbl$sec[sax][lbl$inlap[sax]],col='green')
	points(lbl$endTimeElapsed[sax][lbl$outlap[sax]],lbl$sec[sax][lbl$outlap[sax]],col='red')
	### highlight leader at each stage
	with(lbl, points(endTimeElapsed[isLeader],sec[isLeader],col='cyan',pch='+',cex=0.75))
	### and draw in lines that we already have for safety cars
	if (nrow(safetycarDF)>0) {
		for (j in 1:nrow(safetycarDF)) {
			if (safetycarDF$inout[j] == 'in') abline(v=safetycarDF$timeElapsed[j],col='cyan',lty=3)
			if (safetycarDF$inout[j] == 'out') abline(v=safetycarDF$timeElapsed[j],col='cyan')
		}
	}
}

ProcessSafetyCar.focusfunct=function(lbl, xfocus,xwid, ovylim) {
	xlim=c(xfocus-xwid/2,xfocus+xwid/2)
	sax=which(lbl$endTimeElapsed>xlim[1] & lbl$endTimeElapsed<xlim[2])
	defylim=c(min(lbl$sec[sax],na.rm=T),pmin(ovylim[2],max(lbl$sec[sax],na.rm=T)))
	return(list(sax=sax,xlim=xlim,defylim=defylim))
}

ProcessSafetyCar = function(myRace) {
	
	lbl = InitialiseLbl(myRace)
	lbl = MakeLeaderTimeElapsed(lbl)
	pitStopDF = MakePitStopDF(myRace)
	lbl = MakeInlapOutlap(lbl, pitStopDF)
	### check to see if there is already safety car info for this race
	alldone=F
	ovylim=range(lbl$sec,na.rm=T)
	safetycarDF = tibble(inout = character(0), timeElapsed = numeric(0))
	safetycarfile=MakeRaceFile(myRace, 'safety-car.csv')
	while(!alldone) {
		
		ProcessSafetyCar.plotsec(1:nrow(lbl), lbl, myRace, ovylim=ovylim,safetycarDF = safetycarDF)
		### now offer user choice of actions
		cat("Type 'o' to identify a safety car going out\n")
		cat("Type 'i' to identify a safety car coming in\n")
		cat("Type 'y' to override the y limits\n")
		cat("Type 'r' to reset any choices you have made so far\n")
		cat("Type 'x' when there is nothing more to do\n")
		
		### click on a area that seems to correspond to safety car coming out
		
		uresp=scan(what='',nmax=1,quiet=T)
		
		if (!uresp %in% c('o','i','y','r','x')) {
			print('invalid response, try again')
			next
		}
		
		if (uresp=='y') {
			cat('Please enter new upper y limit:\n')
			ovylim[2]=as.numeric(scan(what='',nmax=1,quiet=T))
			next
		}
		
		if (uresp %in% c('i','o')) {
			
			inout=c('in','out')[match(uresp,c('i','o'))]
			onedone=F
			ovxwid=1500
			print('OK, click on an area you would like to zoom in on')
			xfocus=locator(n=1)
			
			focusquant=ProcessSafetyCar.focusfunct(lbl, xfocus$x,ovxwid, ovylim)
			ylim.tm=focusquant$defylim
			
			while(!onedone) {
				
				### let's get a close up of points around then
				
				ProcessSafetyCar.plotsec(focusquant$sax,lbl, myRace, xlim=focusquant$xlim,ovylim=ylim.tm,safetycarDF = safetycarDF)
				
				cat("Type 'x' to adjust the width of the window\n")
				cat("Type 'a' to abandon this search and seek a different place\n")
				cat("Type 'f' to confirm the point where the safety car comes",inout,"\n")
				cat("Type 'y' to override the y limits\n")
				
				uresp2=scan(what='',nmax=1,quiet=T)
				
				if (!uresp2 %in% c('x','a','y','f')) {
					print('invalid response, try again')
					next
				}
				
				if (uresp2=='y') {
					cat('Please enter new upper y limit:\n')
					ylim.tm[2]=as.numeric(scan(what='',nmax=1,quiet=T))
					next
				}
				if (uresp2=='x') {
					cat('Please enter new window width:\n')
					ovxwid=as.numeric(scan(what='',nmax=1,quiet=T))
					focusquant=ProcessSafetyCar.focusfunct(lbl, xfocus$x,ovxwid, ovylim)
					ylim.tm=focusquant$defylim
					next
				}
				if (uresp2=='a') {
					print('Ok, going back to looking at all times')
					onedone=T
					next
				}
				if (uresp2=='f') {
					cat('OK, click on your best guess for when the safety car comes',inout,'\n')
					dum=locator(n=1)
					myTimeElapsed=dum$x
					onedone=T
					next
				}
			}
			safetycarDF = add_row(safetycarDF, inout = inout, timeElapsed = myTimeElapsed)
		}
		
		if (uresp=='r') {
			safetycarDF = tibble(inout = character(0), timeElapsed = numeric(0))
			next
		}
		if (uresp=='x') {
			alldone=T
			next
		}
	}
	
	### and save to disk in case you need to rewrite the database at any point
	write_csv(safetycarDF, path=safetycarfile)
}

ProcessWetQualifying = function(myRace) {
	message('Which qualifying sessions in ',myRace,' were wet (ENTER for none)?')
	myWetSession = scan(quiet = TRUE, nmax = 1)
	if (length(myWetSession) == 0) {
		myWetQualyDF = tibble(race = character(0), session = integer(0))
	}
	if (length(myWetSession) > 0) {
		satis = FALSE
		while(!satis) {
			message('Which other qualifying sessions were wet (ENTER when finished)?')
			myNextWetSession = scan(quiet = TRUE, nmax = 1)
			if (length(myNextWetSession) == 0) {
				satis = TRUE
				break
			}
			myWetSession = c(myWetSession, myNextWetSession)
		}
		
		myWetQualyDF = tibble(race = myRace, session = myWetSession)
	}
	fileout = MakeRaceFile(myRace, 'wet-qualifying.csv')
	write_csv(myWetQualyDF, path = fileout)
}

ProcessWetRace.plotsec=function(sax, lbl, myRace, ovylim, wetPeriodDF, miscinfo) {
	plot(lbl$endTimeElapsed[sax],lbl$sec[sax],ylim=c(ovylim[1],ovylim[2]),xlab='temptimeelapsed',ylab='lap time',main=myRace)
	### highlight in and out laps
	points(lbl$endTimeElapsed[sax][lbl$inlap[sax]],lbl$sec[sax][lbl$inlap[sax]],col='green')
	points(lbl$endTimeElapsed[sax][lbl$outlap[sax]],lbl$sec[sax][lbl$outlap[sax]],col='yellow')
	
	### highlight any wet or intermediate tyre usage
	points(lbl$endTimeElapsed[sax][lbl$tyre[sax] == 'wet'],lbl$sec[sax][lbl$tyre[sax] == 'wet'],col='purple')
	points(lbl$endTimeElapsed[sax][lbl$tyre[sax] == 'intermediate'],lbl$sec[sax][lbl$tyre[sax] == 'intermediate'],col='magenta')
	### highlight leader at each stage
	points(lbl$endTimeElapsed[miscinfo$ixlead],lbl$sec[miscinfo$ixlead],col='cyan',pch='+',cex=0.75)
	### indicate where the qualifying intercept was - add 3 seconds to qualifying seems to be a good guide
	if (!is.na(miscinfo$fastestQualifyingSec)) abline(h=miscinfo$fastestQualifyingSec+3,lty=3,col='blue')
	### overlay line of fitted intercept for each lap
	for (j in 1:miscinfo$nlap) {
		lines(with(lbl,c(min(endTimeElapsed[leadLap==j]),max(endTimeElapsed[leadLap==j]))),c(miscinfo$lapcoef[j],miscinfo$lapcoef[j]),col='red',lwd=2)
	}
	### and draw in lines that we already have for wetness
	if (nrow(wetPeriodDF) > 0) {
		for (j in 1:nrow(wetPeriodDF)) {
			if (wetPeriodDF$startEnd[j] == 'start') abline(v=wetPeriodDF$timeElapsed[j],col='cyan',lty=3)
			if (wetPeriodDF$startEnd[j] == 'end') abline(v=wetPeriodDF$timeElapsed[j],col='cyan')
		}
	}
}

ProcessWetRace = function(myRace) {
	
	myYear = substr(myRace, 1, 4)
	lbl = InitialiseLbl(myRace)
	lbl = MakeLeaderTimeElapsed(lbl)
	pitStopDF = MakePitStopDF(myRace)
	stintDF = MakeStintDF(lbl, pitStopDF)
	lbl = MakeInlapOutlap(lbl, pitStopDF)
	lbl = AlignStintTyre(lbl, stintDF, pitStopDF)
	### get rid of any massive outlier times, they're so annoying
	dum=with(lbl,tapply(sec[inlap+outlap==0],lap[inlap+outlap==0],median))
	dum2=as.numeric(dum)[match(lbl$lap,names(dum))]
	sax=with(lbl,which((sec>dum2*1.1 & inlap+outlap==0) | (inlap+outlap>0 & sec>dum2*1.5)))
	lbl[sax,'sec']=NA
	
	### then supply a lm curve fitting intercept for each lap - eliminate the slowest three drivers to get rid of outliers though
	lbl$isvalid=rep(0,dim(lbl)[1])
	myNLap = max(lbl$lap)
	for (j in 1:myNLap) {
		sax=with(lbl,which(lap==j & inlap+outlap==0 & !is.na(sec)))
		rdum=rank(lbl$sec[sax])
		lbl$isvalid[sax]=as.numeric(rdum<=length(rdum)-3)
	}
	
	miscinfo = NULL

	miscinfo$nlap = myNLap
	miscinfo$fastestQualifyingSec = FindFastestDryQualifyingTime(myRace)

	mod=with(lbl[lbl$isvalid==1,],lm(sec~factor(driver)+factor(lap)-1))
	dum=coef(mod)[grep('factor\\(lap\\)',names(coef(mod)))]
	miscinfo$lapcoef=rep(NA,myNLap)
	miscinfo$lapcoef[match(gsub('(^.+\\))([0-9]+$)','\\2',names(dum)),1:myNLap)]=as.numeric(dum)
	### except we want it to represent the average for driers so it overlays nicely on the plot, so let's add that on
	miscinfo$lapcoef=miscinfo$lapcoef + mean(coef(mod)[grep('factor\\(driver\\)',names(coef(mod)))])
	
	miscinfo$ixlead=which(lbl$isLeader==1)
	
	### check to see if there is already safety car info for this race
	alldone=F
	ovylim=range(c(miscinfo$fastestQualifyingSec+3,lbl$sec),na.rm=T)
	wetPeriodDF = tibble(startEnd = character(0), timeElapsed = numeric(0))
	wetperiodfile=MakeRaceFile(myRace,'wet-period.csv')
	while(!alldone) {
		
		ProcessWetRace.plotsec(1:nrow(lbl),lbl,myRace, ovylim=ovylim,wetPeriodDF = wetPeriodDF,miscinfo=miscinfo)
		### now offer user choice of actions
		cat("Type 's' to identify start of rain\n")
		cat("Type 'e' to identify end of rain\n")
		cat("Type 'y' to override the y limits\n")
		cat("Type 'r' to reset any choices you have made so far\n")
		cat("Type 'x' when there is nothing more to do\n")
		
		### click on a area that seems to correspond to safety car coming out
		
		uresp=scan(what='',nmax=1,quiet=T)
		
		if (!uresp %in% c('s','e','y','r','x')) {
			print('invalid response, try again')
			next
		}
		
		if (uresp=='y') {
			cat('Please enter new upper y limit:\n')
			ovylim[2]=as.numeric(scan(what='',nmax=1,quiet=T))
			next
		}
		
		if (uresp %in% c('s','e')) {
			
			startEnd=c('start','end')[match(uresp,c('s','e'))]
			message('OK, click on where you think the rain',startEnd,'s')

			dum=locator(n=1)
			myTimeElapsed=dum$x
			wetPeriodDF = add_row(wetPeriodDF, startEnd = startEnd, timeElapsed = myTimeElapsed)
			next
		}
		
		if (uresp=='r') {
			wetPeriodDF = tibble(startEnd = character(0), timeElapsed = numeric(0))
			next
		}
		if (uresp=='x') {
			numStartPeriod = sum(wetPeriodDF$startEnd == 'start')
			numEndPeriod = sum(wetPeriodDF$startEnd == 'end')
			if (numStartPeriod != numEndPeriod) {
				print('hold on there, you need to have the same number of start and end periods')
				alldone=F
				next
			}
			if (numStartPeriod == numEndPeriod) {
				print('ok, all done for this race')
				alldone=T
				next
			}
		}
	}
	
	### and save to disk in case you need to rewrite the database at any point
	write_csv(wetPeriodDF, path=wetperiodfile)
}

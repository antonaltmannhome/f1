
FindAllOvertakingMove = function(myLbl, myRace) {
	otlist=NULL
	otcount=0
	### mark out the cutoff points for TimeElapsed for each lap
	myundriv=unique(myLbl$driver)
	### and we wan that in order of elapsed time
	myLbl=myLbl[order(myLbl$startTimeElapsed),]
	for (di in 1:length(myundriv)) {
		for (li in myLbl$lap[myLbl$driver==myundriv[di]]) {
			### so, who was both behind when they started the lap, AND aheda when they finished it?
			t1=with(myLbl,startTimeElapsed[driver==myundriv[di] & lap==li])
			t2=with(myLbl,endTimeElapsed[driver==myundriv[di] & lap==li])
			dum1=with(myLbl,paste(driver,lap)[startTimeElapsed>t1])
			dum2=with(myLbl,paste(driver,lap)[endTimeElapsed<t2])
			yesot=gsub(' [0-9]+$','',intersect(dum1,dum2))
			oterslap=as.integer(gsub('(^.+ )([0-9]+$)','\\2',intersect(dum1,dum2)))
			if (length(yesot)>0) {
				otcount=otcount+1
				otlist[[otcount]]=tibble(didOtDriver = yesot,
											didOtLap = oterslap,
											gotOtDriver = myundriv[di],
											gotOtLap = li)
			}
		}
		#mylapbylap[predoneix,]
		#mylapbylap[postdoneix,]
		cat('Have got overtaking info for',myundriv[di],'\n')
		}
	overtakingDF=bind_rows(otlist)
	
	# but tack on whether it was an in or outlap by gotOt (for non-corrected version)
	overtakingDF = lazy_left_join(overtakingDF,
									myLbl %>%
									dplyr::rename(gotOtDriver = driver,
													gotOtLap = lap),
									c('gotOtDriver', 'gotOtLap'),
									c('inlap', 'outlap', 'isSafetyCar', 'isSCRestart'))
	overtakingDF = overtakingDF %>%
					arrange(didOtLap) %>%
					mutate(race = myRace)
	
	return(overtakingDF)
}

AlignOvertaking = function(myLbl, overtakingDF, isPreDelta) {
	CheckRequiredColumnsInDF(myLbl, c('inlap', 'outlap'))

	myLbl = myLbl %>%
			mutate(didOt = 0L, gotOt = 0L, didLap = 0L, gotLap = 0L)
	
	if (isPreDelta) {
		overtakingDF$isValid = with(overtakingDF, !inlap & !outlap & !isSafetyCar & !isSCRestart)
	}
	if (!isPreDelta) {
		overtakingDF$isValid = TRUE
	}
	
	for (overtakeLapStatus in c('overtake', 'lap')) {
		if (overtakeLapStatus == 'overtake') {
			relevantDF = overtakingDF %>%
						filter(didOtLap == gotOtLap & isValid)
			columnSecondWord = 'Ot'
		}
		if (overtakeLapStatus == 'lap') {
			relevantDF = overtakingDF %>%
						filter(didOtLap > gotOtLap & isValid)
			columnSecondWord = 'Lap'
		}
		for (didGotStatus in c('did', 'got')) {
			if (didGotStatus == 'did') {
				otSummaryDF = relevantDF %>%
								group_by(didOtDriver, didOtLap) %>%
								summarise(total = n()) %>%
								ungroup()
				columnFirstWord = 'did'
			}
			if (didGotStatus == 'got') {
				otSummaryDF = relevantDF %>%
								group_by(gotOtDriver, gotOtLap) %>%
								summarise(total = n()) %>%
								ungroup()
				columnFirstWord = 'got'
			}
			driverColumnName = paste0(columnFirstWord, 'OtDriver')
			lapColumnName = paste0(columnFirstWord, 'OtLap')
			totalColumnName = paste0(columnFirstWord, columnSecondWord)
			otSummaryDF = otSummaryDF %>%
							mutate(driver = get(driverColumnName),
											lap = get(lapColumnName),
											!!totalColumnName := total) %>%
							select(driver, lap, !!totalColumnName)
		myLbl  = indicate_overlapping_combination(
					myLbl,
					otSummaryDF,
					c('driver', 'lap'),
					'anyhappened')
		myLbl  = subset_join(myLbl,
						otSummaryDF,
						c('driver', 'lap'),
						anyhappened)
		}
	}
	
	# but wait, was this done before we got hold of all the pit stop delta info
	# if so, it should be stored as the 'predelta' columns
	if (isPreDelta) {
		myLbl = myLbl %>%
				dplyr::rename(preDeltaDidOt = didOt,
								preDeltaGotOt = gotOt,
								preDeltaDidLap = didLap,
								preDeltaGotLap = gotLap)
	}
	
	myLbl = within(myLbl, rm(anyhappened))
	
	return(myLbl)
}

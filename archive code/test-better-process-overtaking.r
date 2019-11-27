
ProcessOvertaking = function(myRaceDF) {
	otlist=NULL
	otcount=0
	### mark out the cutoff points for telapse for each lap
	myundriv=unique(myRaceDF$driver)
	### and we wan that in order of elapsed time
	myRaceDF=myRaceDF[order(myRaceDF$starttelapse),]
	for (di in 1:length(myundriv)) {
		for (li in myRaceDF$lap[myRaceDF$driver==myundriv[di]]) {
			### so, who was both behind when they started the lap, AND aheda when they finished it?
			t1=with(myRaceDF,starttelapse[driver==myundriv[di] & lap==li])
			t2=with(myRaceDF,endtelapse[driver==myundriv[di] & lap==li])
			dum1=with(myRaceDF,paste(driver,lap)[starttelapse>t1])
			dum2=with(myRaceDF,paste(driver,lap)[endtelapse<t2])
			yesot=gsub(' [0-9]+$','',intersect(dum1,dum2))
			oterslap=as.numeric(gsub('(^.+ )([0-9]+$)','\\2',intersect(dum1,dum2)))
			if (length(yesot)>0) {
				otcount=otcount+1
				otlist[[otcount]]=tibble(overtaker = yesot,
				overtakerlap = oterslap,
				overtakee = myundriv[di],
				overtakeelap = li)
			}
		}
		#mylapbylap[predoneix,]
		#mylapbylap[postdoneix,]
		cat('Have got overtaking info for',myundriv[di],'\n')
	}
	overtakingDF=bind_rows(otlist)
	
	# but tack on whether it was an in or outlap by overtakee (for non-corrected version)
	overtakingDF = lazy_left_join(overtakingDF,
									myRaceDF %>%
									dplyr::rename(overtakee = driver,
													overtakeelap = lap),
									c('overtakee', 'overtakeelap'),
									c('inlap', 'outlap'))
	overtakingDF = overtakingDF %>%
					filter(!inlap & !outlap) %>%
					arrange(overtakerlap)
	
	return(overtakingDF)
}


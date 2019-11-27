# need to calculate overtakings twice, the second time to account for pit stop adjustments

GetPostDeltaOvertaking = function() {
	LoadAllData()
	
	lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE)

	rrToModel = with(raceDF, rr[!donePostDeltaOvertaking])

	for (ri in rrToModel) {
		if (raceDF$isValidRace4[ri]) {
			# note that the overtaking code assumes you're sending in lbl for a single race
			# so that's why we loop over all races
			myLbl = lbl %>%
					filter(race == raceDF$race[ri])
			myOvertakingDF = f1gaptrafficpitstop:::FindAllOvertakingMove(myLbl, raceDF$race[ri])
			myLbl = f1gaptrafficpitstop:::AlignOvertaking(myLbl, myOvertakingDF, isPreDelta = FALSE)
			
			### then write that to the database
			sqlLazyUpdate(myLbl, 'racedriverlap', c('race', 'driver', 'lap'), c('didOt', 'gotOt', 'didLap', 'gotLap'))
			if (FALSE) {
			sqlUpdate_multikey('racedriverlap', 
								c('didOt', 'gotOt', 'didLap', 'gotLap'),
								c('race', 'driver', 'lap'),
								myLbl[,c('didOt', 'gotOt', 'didLap', 'gotLap')],
								myLbl[,c('race', 'driver', 'lap')])
			}
			
			sqlInsert_usedf('overtaking',
						myOvertakingDF)
		}
		raceDF$donePostDeltaOvertaking[ri] = TRUE
	}

	sqlLazyUpdate(raceDF[rrToModel,], 'race', 'race', 'donePostDeltaOvertaking')
	if (FALSE) {
	sqlUpdate_multikey('race',
						'donePostDeltaOvertaking',
						'race',
						raceDF[rrToModel,'donePostDeltaOvertaking'],
						raceDF[rrToModel,'race'])
	}
}

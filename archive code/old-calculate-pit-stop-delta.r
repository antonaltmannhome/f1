
CalculatePitStopDelta = function(raceDF, lbl) {

	# there are good inlaps/and bad inlaps, make sure we elimate any suspicious ones
	stintDF$stintLength = with(stintDF, endLap - startLap)
	lbl = lazy_left_join(lbl, stintDF, c('race', 'driver', 'stint'), 'stintLength')

	# what is wrong with penalty pit stop times?
	lbl = f1gaptrafficpitstop::AlignPitStopData(lbl)

	lbl = lazy_left_join(lbl, rddf, c('race', 'driver'), 'mod4PredNValid')

	lbl = f1validity::MakeIsRogue(lbl, inlapOutlapAreRogue = FALSE)
	lbl = lazy_left_join(lbl, raceTyreDF, c('race', 'tyre'), 'isValidTyre4')
	lbl$isGoodInlap = with(lbl, !isRogue & !isOutlier0 & mod4PredNValid > 0 & isValidTyre4)

	lbl = f1laptimelm::MakePredSec(lbl, 4)

	# this goes horrifically wrong if there are an unexpected NAs so watch out for them
	unexpectedNA = with(lbl, sum(is.na(mod4PredSec[isGoodInlap])))
	if (unexpectedNA > 0) {
		stop('Unexpected NAs with mod4PredSec in CalculatePitStopDelta, please investigate\n')
	}

	inlapOutlapDelta = lbl %>%
						group_by(race) %>%
						filter(isGoodInlap) %>%
						summarise(inlapLoss = median( (sec - mod4PredSec)[inlap]),
									outlapLoss = median( (sec - mod4PredSec)[outlap]),
									numValidStop = sum(inlap),
									medianPitStopTime = median( pitStopTime[inlap & !penalty]),
									earlyPit = inlapLoss > outlapLoss,
									inlapNotPitTime = median( (mod4PredSec - (sec - pitStopTime))[inlap]),
									outlapNotPitTime = median( (mod4PredSec - (sec - prevLapPitStopTime))[outlap]),
									notPitTime = ifelse(earlyPit,
														inlapNotPitTime,
														outlapNotPitTime)) %>%
									select(-c(earlyPit, inlapNotPitTime, outlapNotPitTime))

	raceDF = lazy_left_join(raceDF, inlapOutlapDelta, 'race')

	### be careful, in a one stop race with a safety car, data will be very dodgy, better warn ourselves about this
	if (any(raceDF$numValidStop<15)) {
		missrace=raceDF %>%
					filter(numValidStop < 15) %>%
					pull(race)
		filein=paste(USERPATH,'data/pittimedf_override.csv',sep='')
		overwrittenInlapOutlapDelta = ReadF1Data(filein, 'pitTimeOverride')
		if (any(!missrace %in% overwrittenInlapOutlapDelta$race)) {
			stop('You need to add ',sum(!missrace %in% overwrittenInlapOutlapDelta$race),' lines to data/pittimedf_override, we don\'t have enough data to be confident about pit time differences for\n',paste(setdiff(missrace,overwrittenInlapOutlapDelta$race),collapse='\n'),'\n')
		}
		## if we've got this far, then we have all the data we need
		raceDF = indicate_overlapping_combination(
					raceDF,
					overwrittenInlapOutlapDelta,
					'race',
					'toOverwrite')
		raceDF = subset_join(raceDF,
								overwrittenInlapOutlapDelta,
								'race',
								toOverwrite)
		raceDF = raceDF %>% select(-toOverwrite)
	}

	# except while we're testing, let's override all of these with the stored ones just to check everything else is behaving
	message('NB, note still using the old pittimedf, switch over when ready')
	oldpittimedf = read.csv('c:/research/f1/temporary transfer data/pittimedf.csv') %>%
					rename(race = racename,
							inlapLoss = illoss,
							outlapLoss = olloss,
							medianPitStopTime = medpstime,
							notPitTime = notpittime,
							numValidStop = nvalidstop) %>%
					select(-c(rr))
	if (any(!raceDF$race %in% oldpittimedf$race)) {
		message('you have not updated the pittimdf.csv, you need to write this line in old world:')
		write.csv(file = 'c:/research/f1/temporary transfer data/pittimedf.csv',
					pittimedf, row.names = FALSE)
	}
	raceDF = raceDF %>% select(-c(inlapLoss, outlapLoss, medianPitStopTime, notPitTime, numValidStop))
	raceDF = left_join(raceDF, oldpittimedf, 'race')

	return(raceDF)
}

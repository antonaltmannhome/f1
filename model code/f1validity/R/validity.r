MakeIsRogue = function(lbl, inlapOutlapAreRogue = TRUE) {
	lbl = f1data:::DetectWetTyreOnDryTrack(lbl)
	stintDF$isShortStint = with(stintDF, endLap - startLap <=2)
	lbl = lazy_left_join(lbl, stintDF, c('race', 'driver', 'stint'), 'isShortStint')

	lbl$isRogue = with(lbl, isShortStint |
							isSafetyCar | isWet | isRed | lap == 1 | isSCRestart |
							isWetTyreOnDryTrack)
	if (inlapOutlapAreRogue) {
		lbl$isRogue[with(lbl, which(inlap | outlap))] = TRUE
	}

	return(lbl)
}

MakeInTraffic = function(modelchoice, lbl) {
	if (modelchoice == 4) {
		lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = TRUE)
		lbl$inTraffic = with(lbl, preDeltaDidOt > 0 | preDeltaGotOt >0 | preDeltaGotLap > 0 |
							(startRank > 1 & SOLGap < 1.5))
	}

	if (modelchoice == 30) {
		lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE)
		lbl$wasBlocked = with(lbl, (startRank > 1 & SOLGap < 1.5) |
								(endRank > 1 & EOLGap > 0 & EOLGap < 1.5))
		lbl$inTraffic = with(lbl, didOt > 0 | gotOt >0 | gotLap > 0 | wasBlocked)
	}
	return(lbl)
}

MakeIsOutlier = function(modelchoice, lbl) {

	if (modelchoice == 30) {

		if (!'inTraffic' %in% names(lbl)) {
			lbl = f1validity:::MakeInTraffic(30, lbl)
		}

		rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, 4, 'race')
		lbl$mod4PredSec = f1laptimelm::MakePredSecFromNormalisedDCoef(lbl, rddf, 4, 'mod4DCoef')
		dum = f1smoothing::GetSmooth(qrToFit = 'qr',
								qrToPredict = 'r',
								modelChoice = 4,
								useStretch = TRUE,
								fwbw = 'fwbw')
		rddf = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'smoothDCoef')
		lbl$smoothedMod4PredSec =
			f1laptimelm::MakePredSecFromNormalisedDCoef(lbl, rddf, 4, 'smoothDCoef')
		lbl$predSecToUse = with(lbl, ifelse(!is.na(mod4PredSec), mod4PredSec, smoothedMod4PredSec))
		lbl$isOutlier30 = FALSE
		lbl$isOutlier30[with(lbl, which(sec > predSecToUse + 2 & !inTraffic & !isCarProblem))] = TRUE
		lbl$isOutlier30[with(lbl, which(sec > predSecToUse + 6 & inTraffic & !isCarProblem))] = TRUE
	}

	return(lbl)
}

MakeIsGoodPreValidRace = function(modelchoice, lbl, raceDF) {

	if (modelchoice == 4) {

		lbl = f1validity::MakeIsRogue(lbl)
		lbl = f1validity:::MakeInTraffic(4, lbl)
		lbl$isGoodPreValidRace = with(lbl, !isRogue & !isOutlier0 & !inTraffic)
	}
	if (modelchoice %in% c(30)) {
		lbl = f1validity::MakeIsRogue(lbl, inlapOutlapAreRogue = TRUE)
		lbl = f1validity:::MakeInTraffic(30, lbl)
		lbl = lazy_left_join(lbl, raceDF, 'race', 'isValidRace4')

		lbl = f1validity:::MakeIsOutlier(30, lbl)
		lbl$isGoodPreValidRace = with(lbl, !isRogue &
											!inTraffic &
											isValidRace4 &
											!isOutlier30)
	}

	return(lbl)
}

MakeIsValidRace = function(myRace, lbl) {

	# do we have any clear good laps?
	# even if we do, it'll be pointless unless there are a decent number of laps by enough drivers, something like at least 10 drivers doing at least 20 laps is the bare minimum
	numValidLap = with(lbl, sum(isGoodPreValidRace[race == myRace]))
  # if the number of valid laps is > 500, surely pointless and irritating to ask if it's valid, it surely is
	if (numValidLap > 500) {
	  myIsValidRace = TRUE
	}
	
	if (numValidLap < 100) {
	  myIsValidRace = FALSE
	}
	if (between(numValidLap, 100, 500)) {
  	f1plot:::StintSummary(myRace, hatchwet = TRUE, hatchsafetycar = TRUE)
    message('The number of valid laps is only ', numValidLap)
    message('When looking at the stint summary, do you think we should treat this as a valid race (y/n')
    satis = FALSE
    while(!satis) {
      dum = askcond(FALSE, FALSE)
      if (dum %in% c('y', 'n')) {
        satis = TRUE
      }
    }
    if (dum == 'y') myIsValidRace = TRUE
    if (dum == 'n') myIsValidRace = FALSE
	}
	
	return(myIsValidRace)
}

UpdateValidity = function(modelchoice) {

	# NB we store stuff to disk because it does take a bit of time to calcualte plus they're used ubiquitously
	LoadAllData()

	modelLabel = f1data:::MakeModelToUseName(paste0('validity', modelchoice), modelchoice)

	rrToUpdate = with(raceDF, rr[!get(modelLabel$doneValidity)])
	
	if ( (length(rrToUpdate) > 0) ) {
		message('Need to update validity for model ', modelchoice,' for the following race(s):')
		print(raceDF$race[rrToUpdate])

		## step 1, get the pre-valid race valid laps

		if (modelchoice == 'outlier0') {
			lbl$isGood = with(lbl, !isRogue)
			raceDF$isValidRace = TRUE
		}

		if (modelchoice %in% c(4, 30)) {
			lbl = f1validity::MakeIsGoodPreValidRace(modelchoice, lbl, raceDF)
			raceDF$isValidRace = NA
			for (ri in rrToUpdate) {
			  raceDF$isValidRace[ri] = f1validity::MakeIsValidRace(raceDF$race[ri], lbl)
			  lbl$isCurrentRace = with(lbl, rr == ri)
			  lbl = subset_join(lbl,
			                    raceDF %>%
			                       select(race, isValidRace),
			                    'race',
			                    isCurrentRace)
			  if (modelchoice == 4) {
			    lbl$isGood = with(lbl, isGoodPreValidRace & isValidRace)
			  }
			  if (modelchoice == 30) {
			    lbl$isGood = with(lbl, isGoodPreValidRace &
			                        isValidRace &
			                        !isCarProblem)
			    lbl$isCarProblemButGood30 = with(lbl, isGoodPreValidRace &
			                                       isValidRace &
			                                       isCarProblem)
			  }
			  
			  raceRaceIndex = which(raceDF$rr == ri)
				lblRaceIndex = which(lbl$rr == ri)
				raceDF[raceRaceIndex,modelLabel$isValidRace] = raceDF$isValidRace[raceRaceIndex]
				raceDF[raceRaceIndex,modelLabel$doneValidity] = TRUE
				lbl[lblRaceIndex,modelLabel$isGood] = lbl$isGood[lblRaceIndex]
				sqlLazyUpdate(raceDF[raceRaceIndex,],
						'race',
						'race',
						c(modelLabel$isValidRace, modelLabel$doneValidity))
				sqlLazyUpdate(lbl[lblRaceIndex,],
						'racedriverlap',
						c('race', 'driver', 'lap'),
						modelLabel$isGood)
				if (modelchoice == 30) {
					sqlLazyUpdate(lbl[lblRaceIndex,],
						'racedriverlap',
						c('race', 'driver', 'lap'),
						'isCarProblemButGood30')
				}
				message('Have updated validity for model ', modelchoice,' for ', raceDF$race[ri])
			}
			# but what do we return? if you don't want the intermeidate columns, all you get is isGoodX and isValidRaceX, otherwise
			# no, that's the wrong way to think about this. this just updates the database, nothing more
			# current thinking: you get those things when you need them, we'll see how often that happens and modify accordingly then. it's not hard to make additional functions that just return isRogue, inTraffic etc, doesn't require major overhaul to do that
			# maybe there's a case for a 'RetrieveIsGood' function, because we might want several isGoods eg for carproblem model 30, overtaking etc. don't want all available by default
		}
	}

	return(NULL)
}

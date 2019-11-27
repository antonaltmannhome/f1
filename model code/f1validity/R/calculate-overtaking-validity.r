
MakeGotOtBlockedIsGood = function(lbl) {
	# NB this one is not run as part of UpdateValidity but is in the same family of functions so lives here

	lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE)
	lbl = f1validity:::MakeIsRogue(lbl)

	lbl = f1laptimelm:::MakePredSec(lbl, 30, adjustForCarProblem = TRUE)

	lbl = lbl %>%
			mutate(gotOvertakenOnceCutoff = mod30PredSec + miscStat$model30GotOvertakenOnceOutlierCutoff,
					gotOvertakenGTOnceCutoff = mod30PredSec + miscStat$model30GotOvertakenGTOnceOutlierCutoff) %>%
			mutate(outlierCutoff = case_when(
						gotOt == 1 & !isRogue ~ gotOvertakenOnceCutoff,
						gotOt > 1 & !isRogue ~ gotOvertakenGTOnceCutoff)) %>%
			mutate(isOutlierGotOt = (gotOt > 0) & (impsec > outlierCutoff) & !is.na(mod30PredSec),
					isGoodGotOt = (gotOt > 0) & !is.na(mod30PredSec) & !isOutlierGotOt & !isRogue)

	# we will need to know secLimit before deciding something is validly blocked. so let's define that
	# see project/overtaking/more-investigation for explanation
	# for most situations, secLimit is just EOLDASec - SOLGapToEOLDA. Which is equal to SOLDASec - SOLGapToSOLDA when the driver ahead doesn't have any problems or overtake. It's not the case whent he driver ahead does overtaking, but that's why the first equation is better
	# exceptions (1): the driver ahead gets overtaken by us but does an outlying lap. in that case, we ought to ignore that driver then recalculate based on other drivers. but that's complicated, and gives us virtually no benefit, because all we gain is a few extra (possibly dodgy) data points in our massive database of possible overtakings/blocked laps. might help in a situation where we're trying to estimate circuit OT parameters, but that's too much of an edge case to worry about I think
	# exceptions (2): the driver ahead pits. actually, this isn't a problem, impsec and endTimeElapsed should cover it. this is fine
	lbl = lbl %>%
				mutate(isDeadHeat = impsec - secLimit < 0.05,
							isBlocked = (overlap < 5) & ! isDeadHeat,
							isOutlierBlocked = (impsec > mod30PredSec + miscStat$model30ClearLapOutlierCutoff) &
																	(impsec > secLimit + miscStat$model30ClearLapOutlierCutoff),
							isGoodBlocked = !isRogue & didOt == 0 & gotOt == 0 &
															!is.na(mod30PredSec) & startRank != 1 &
															!isOutlierBlocked & isBlocked)

	lbl = lbl %>%
			select(-c(gotOvertakenOnceCutoff, gotOvertakenGTOnceCutoff,
						outlierCutoff, isDeadHeat, isBlocked, mod30PredSec))

	# if you did overtake, that's always legit i think except when the driver ahead was an outlier. or it's lap1, or SC, or SCRestart ie. isRogue
	lbl$isGoodDidOt = with(lbl, didOt > 0 & !isRogue)

	return(lbl)
}

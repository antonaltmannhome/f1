LoadAllData()
lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE, includeIntermediateColumn = TRUE)
lbl = f1validity:::MakeIsRogue(lbl)
lbl$secLimit = with(lbl, SOLDASec - SOLGap)

lbl = f1laptimelm:::MakePredSec(lbl, 30, adjustForCarProblem = TRUE)

lbl = lbl %>%
		mutate(isClearLapNotLeader = (didOt == 0) & (gotOt == 0) & !isRogue & (startRank > 1),
				isClearLapLeader = !isRogue & (startRank == 1),
				clearLapCutoff = pmax(mod30PredSec, secLimit) + miscStat$model30ClearLapOutlierCutoff,
				leaderCutoff = mod30PredSec + miscStat$model30ClearLapOutlierCutoff,
				didOvertakeCutoff = mod30PredSec + 300,
				gotOvertakenOnceCutoff = mod30PredSec + miscStat$model30GotOvertakenOnceOutlierCutoff,
				gotOvertakenGTOnceCutoff = mod30PredSec + miscStat$model30GotOvertakenGTOnceOutlierCutoff) %>%
		mutate(outlierCutoff = case_when(
					isClearLapNotLeader ~ clearLapCutoff,
					isClearLapLeader ~ leaderCutoff,
					didOt & !isRogue ~ didOvertakeCutoff,
					gotOt ==1 & !isRogue ~ gotOvertakenOnceCutoff,
					gotOt > 1 & !isRogue ~ gotOvertakenGTOnceCutoff)) %>%
		mutate(isOutlierOvertaking = impsec > outlierCutoff & !is.na(mod30PredSec),
				isGoodOvertaking = !is.na(mod30PredSec) & !isOutlierOvertaking & !isRogue)

# huge amount of junk there, get rid
lbl = lbl %>%
		select(-c(isClearLapLeader, isClearLapNotLeader,
					clearLapCutoff, leaderCutoff, didOvertakeCutoff, gotOvertakenOnceCutoff, gotOvertakenGTOnceCutoff,
					outlierCutoff))

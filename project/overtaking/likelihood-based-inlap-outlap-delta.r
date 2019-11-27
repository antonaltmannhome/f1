# can we do a likelihood based inlap and outlap delta estimation? so it would make a delta seem less likely if it involved a large number of cars overtaking other while on their inlap or getting overtaken on their outlap

# this is a bit of a bastard because we'd have to estimate overtaking probability based on model 4, ignoring all situations where either driver is on in or outlap

# let's just try it on a single race first, australia 2016 (where at the moment sainz does what looks like an unlikely overtake on massa on lap 8 while on an inlap)
### aargh this is nasty, have to do all of the isgood processing on model 4 data. this feels like a big job, need to think about how much it really matters.
# why do we actually need impsec for model 30? i think it's just to redefine the blocked or not status of laps. actually, we ought to have that

# no, i think this won't be much beter than before, the delta is different for different drivers. it could be better but would involve far too much work for a small gain. we just can't be sure if a driver is ahead or behind from the info we have

MakeOvertakingIsGood = function(modelChoice, lbl) {
  # NB this one is not run as part of UpdateValidity but is in the same family of functions so lives here
  if (modelChoice == 4) {
    lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = TRUE, includeIntermediateColumn = TRUE)
    lbl = f1laptimelm:::MakePredSec(lbl, modelChoice) %>%
      rename(predSec = mod4PredSec)
    lbl$secToUse = lbl$sec
  }
  if (modelChoice == 30) {
    lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE, includeIntermediateColumn = TRUE)
    lbl = f1laptimelm:::MakePredSec(lbl, modelChoice, adjustForCarProblem = TRUE) %>%
      rename(predSec = mod30PredSec)
    lbl$secToUse = lbl$impsec
  }
  lbl = f1validity:::MakeIsRogue(lbl)
  lbl$secLimit = with(lbl, SOLDASec - SOLGapToSOLDA)

  lbl = lbl %>%
    mutate(isClearLapNotLeader = (didOt == 0) & (gotOt == 0) & !isRogue & (startRank > 1),
           isClearLapLeader = !isRogue & (startRank == 1),
           clearLapCutoff = pmax(predSec, secLimit) + miscStat$model30ClearLapOutlierCutoff,
           leaderCutoff = predSec + miscStat$model30ClearLapOutlierCutoff,
           didOvertakeCutoff = predSec + 300,
           gotOvertakenOnceCutoff = predSec + miscStat$model30GotOvertakenOnceOutlierCutoff,
           gotOvertakenGTOnceCutoff = predSec + miscStat$model30GotOvertakenGTOnceOutlierCutoff) %>%
    mutate(outlierCutoff = case_when(
      isClearLapNotLeader ~ clearLapCutoff,
      isClearLapLeader ~ leaderCutoff,
      didOt & !isRogue ~ didOvertakeCutoff,
      gotOt ==1 & !isRogue ~ gotOvertakenOnceCutoff,
      gotOt > 1 & !isRogue ~ gotOvertakenGTOnceCutoff)) %>%
    mutate(isOutlierOvertaking = secToUse > outlierCutoff & !is.na(predSec),
           isGoodOvertaking = !is.na(predSec) & !isOutlierOvertaking & !isRogue)
  
  # huge amount of junk there, get rid
  lbl = lbl %>%
    select(-c(isClearLapLeader, isClearLapNotLeader,
              clearLapCutoff, leaderCutoff, didOvertakeCutoff, gotOvertakenOnceCutoff, gotOvertakenGTOnceCutoff,
              outlierCutoff))
  
  return(lbl)
}	

# so get overtaking prbability at melbourne in general first
lbl = lazy_left_join(lbl, raceDF, 'race', 'circuit')
lbl = f1laptimelm:::MakePredSec(lbl, 4)
lbl = f1validity::MakeIsRogue(lbl)
lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = TRUE)
lbl = MakeOvertakingIsGood(4, lbl)
lbl = lbl %>%
      mutate(overlap = )
melbourneLbl = lbl %>%
                filter(circuit == 'melbourne' & !isRogue)

melbourneLbl = melbourneLbl %>%
                mutate(overlap = )
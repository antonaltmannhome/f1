SetUpModel = function() {
  LoadAllData()
  lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE)
  message('Have processed gaps/overtakings/pit stop deltas etc')
  lbl = f1validity:::MakeIsRogue(lbl)
  lbl = f1validity:::MakeInTraffic(30, lbl)
  message('Have processed isRogue and inTraffic indicators')
  lbl = f1laptimelm:::MakePredSec(lbl, 30, adjustForCarProblem = TRUE)
  rddf = f1laptimelm:::MakeNormDriverCoef(rddf, raceDF, 30, 'race')
  message('Have processed predicted laps times and driver coefficients')
  lbl = lazy_left_join(lbl, raceDF, 'race', 'isValidRace30')

  assign('lbl', lbl, envir = globalenv())
  assign('rddf', rddf, envir = globalenv())
}

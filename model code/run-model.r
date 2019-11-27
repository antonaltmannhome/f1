source('c:/research/f1/f1-startup.r')

f1qualifying::RunQualifyingOutlier()
f1qualifying::RunQualifyingModel()

# also, do we need driverteam in the function, how about use driverTeamYr which has already been made
# worth checking the optimisation is ok, it looked a bit flat when run on very small sample of data
SmoothAndStretch('qual')
# ah but wait, we should really do makeisvalidrace, then put that into makeisgood, before doing this

f1outlier::GetOutlier0()

modelchoice = 4
f1validity::UpdateValidity(modelchoice)
f1laptimelm::ProcessLapTimeModel(modelchoice)

CheckSensibleModel(4)

f1laptimelm::GetLapTimeLMIntercept(modelchoice)

### all checks are agreeing to desired level up to here...
SmoothAndStretch(modelchoice)

f1gaptrafficpitstop::ProcessInlapOutlapDelta()
f1gaptrafficpitstop::GetPostDeltaOvertaking()

# is this actually needed?
# f1validity::UpdateValidity('carProblem')

f1carproblem::ProcessCarProblem()

modelchoice = 30
f1validity::UpdateValidity(modelchoice)
f1laptimelm::ProcessLapTimeModel(modelchoice)
f1laptimelm::GetLapTimeLMIntercept(modelchoice)
SmoothAndStretch(modelchoice)

f1messystint:::ProcessInterruptedStint()
f1messystint:::ProcessGuessedPitStop()
f1messystint:::ProcessAlternativeStrategy()
f1messystint:::ProcessIntendedStopLap()

f1blockedovertakingmodel:::ProcessPossibleOvertaking()
f1blockedovertakingmodel:::ProcessBlockedOvertakingModel()

f1simulation:::ProcessInRunningWeightModel()
f1simulation:::ProcessSimulation()

f1admin::UpdateDatabase()

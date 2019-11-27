### pain in the arse setting up the data table testing every time, here's a quick startup file

source('model code/model-startup.r')
source('project/race simulation/coef-mixing-funct.r')
source('project/race simulation/simulation-funct.r')
blockedOvertakingModelCoef = f1blockedovertakingmodel:::GetBlockedOvertakingModelCoef()
overtakingOverlapCoef =
with(blockedOvertakingModelCoef$overtakingCoefDF, mle[coefType == 'overlap'])
didOvertakeCost = blockedOvertakingModelCoef$didOvertakeCost
gotOvertakenCost = blockedOvertakingModelCoef$gotOvertakenCost
blockedMeanCoef = blockedOvertakingModelCoef$blockedMeanCoef
blockedVarCoef = blockedOvertakingModelCoef$blockedVarCoef
raceDF = f1gaptrafficpitstop::CalculateMedianPitStopTime(raceDF)
#dum = f1simulation:::SetUpUpdatingCoef(lbl)
dum = GetInRunningWeightCoef()
dCoefSourceWeightCoef = dum$dCoefSourceWeightCoef
varByTotalWeightCoef = dum$varByTotalWeightCoef
lbl = InitialisePreRaceCoef(rddf, lbl)
lbl = PrepareLblForCoefMixing(lbl)
myRace = '2019australia'; startOfLap = 1
dataForRace = MakeDataForRace(myRace)
numberOfLaps = dataForRace$numberOfLaps
myLbl = lbl %>%
filter(race == myRace) %>%
data.table::data.table()
myLbl$miscLapWeight = 1
myLbl[,c('meanFinPos', 'modalFinPosProb')] = NA


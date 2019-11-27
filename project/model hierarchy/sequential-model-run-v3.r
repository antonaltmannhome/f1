### like v2 but with weight for lap incentive added in.

source('project/model hierarchy/model-hierarchy-setup.r')

rdlFile = paste0(USERPATH, 'project/validate via finpos/race-driver-lap-simulation.csv')
raceDriverLapSimulation = read.csv(rdlFile, as.is = TRUE) %>%
							rename(race = racename,
									meanFinPos = mod34meanfinpos,
									modalFinPosProb = mod34modalfinposprob)

lbl = left_join(lbl, raceDriverLapSimulation, c('race', 'driver', 'lap'))

lbl$lapWeight = with(lbl, ifelse(modalFinPosProb < 0.95, 1, 1 - 20 * (modalFinPosProb - 0.95)))
lbl$lapWeight[which(lbl$lapWeight < 0.01)] = 0.01
lbl$lapWeight[which(lbl$lapWeight > 1)] = 1
lbl$lapWeight[is.na(lbl$lapWeight)] = 1

# lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE)

#########################################################################################
#################### first make all the validity combinations ###########################
#########################################################################################

lbl$isValidNonRogue = with(lbl, !isRogue & isValidRace30)
rddf$predNValidNonRogue = MakePredNValidForLblValid('isValidNonRogue', rddf, lbl)

lbl$isValidClearLap = with(lbl, !isRogue & !inTraffic & isValidRace30)
rddf$predNValidClearLap = MakePredNValidForLblValid('isValidClearLap', rddf, lbl)

lbl$isValidModel30 = with(lbl, !isRogue & !inTraffic & !isCarProblem & !isOutlier30 &
								isValidTyre30 & isValidRace30)
rddf$predNValidModel30 = MakePredNValidForLblValid('isValidModel30', rddf, lbl)

lbl$isValidBOC  = with(lbl, !isRogue & !isOvertaking & isValidRace30)
rddf$predNValidBOC = MakePredNValidForLblValid('isValidBOC', rddf, lbl)

lbl$isValidBOCNoCarProblem = with(lbl, !isRogue & !isOvertaking & isValidRace30 & !isCarProblem)
rddf$predNValidBOCNoCarProblem = MakePredNValidForLblValid('isValidBOCNoCarProblem', rddf, lbl)

#########################################################################################
############################## then run all the models ##################################
#########################################################################################


allModelName = c('mean', 'fuel', 'fuelTyreLap', 'fuelTyreTypeAndAge', 'fuelTyreTypeAndAgeDWLockedIn')
allValidName = c('nonRogue', 'clearLap', 'model30', 'BOC', 'BOCNoCarProblem')

allConfig = expand.grid(modelName = allModelName,
						validName = allValidName,
						stringsAsFactors = FALSE)

allConfig$sqDiffName = with(allConfig,
						paste0('sqDiff', ToCamel(modelName),
										ToCamel(validName)))
for (mi in 1:nrow(allConfig)) {
	rddf = RunModelAllRace(allConfig$modelName[mi],
							allConfig$validName[mi],
							rddf, lbl)
}

# let's add in model 30 out of interest
rddf = f1laptimelm:::MakeNormDriverCoef(rddf, raceDF, 30, 'race')
rddf = SmoothAndCalculateSqDiff(rddf, 'mod30DCoef', 'mod30PredNValid', 'efp30', 'sqDiff30')

allConfig = add_row(allConfig,
					modelName = 30, validName = 30, sqDiffName = 'sqDiff30')

rddf$isComparisonValid = complete.cases(rddf[,allConfig$sqDiffName])

allConfig = left_join(allConfig,
						tibble::enframe(colMeans(rddf[rddf$isComparisonValid, allConfig$sqDiffName]),
										name = 'sqDiffName',
										value = 'meanSqDiff'),
						'sqDiffName')
                      modelName       validName                                        sqDiffName meanSqDiff
1                          mean        nonRogue                                sqDiffMeanNonRogue   4.206861
2                          fuel        nonRogue                                sqDiffFuelNonRogue   4.137394
3                   fuelTyreLap        nonRogue                         sqDiffFuelTyreLapNonRogue   4.095074
4            fuelTyreTypeAndAge        nonRogue                  sqDiffFuelTyreTypeAndAgeNonRogue   4.062642
5  fuelTyreTypeAndAgeDWLockedIn        nonRogue        sqDiffFuelTyreTypeAndAgeDWLockedInNonRogue   4.059932
6                          mean        clearLap                                sqDiffMeanClearLap   4.540252
7                          fuel        clearLap                                sqDiffFuelClearLap   4.254839
8                   fuelTyreLap        clearLap                         sqDiffFuelTyreLapClearLap   4.157106
9            fuelTyreTypeAndAge        clearLap                  sqDiffFuelTyreTypeAndAgeClearLap   4.144081
10 fuelTyreTypeAndAgeDWLockedIn        clearLap        sqDiffFuelTyreTypeAndAgeDWLockedInClearLap   4.110757
11                         mean         model30                                 sqDiffMeanModel30   4.505639
12                         fuel         model30                                 sqDiffFuelModel30   4.217757
13                  fuelTyreLap         model30                          sqDiffFuelTyreLapModel30   4.123394
14           fuelTyreTypeAndAge         model30                   sqDiffFuelTyreTypeAndAgeModel30   4.102854
15 fuelTyreTypeAndAgeDWLockedIn         model30         sqDiffFuelTyreTypeAndAgeDWLockedInModel30   4.083692
16                         mean             BOC                                     sqDiffMeanBOC   4.167592
17                         fuel             BOC                                     sqDiffFuelBOC   4.091630
18                  fuelTyreLap             BOC                              sqDiffFuelTyreLapBOC   4.033025
19           fuelTyreTypeAndAge             BOC                       sqDiffFuelTyreTypeAndAgeBOC   4.014162
20 fuelTyreTypeAndAgeDWLockedIn             BOC             sqDiffFuelTyreTypeAndAgeDWLockedInBOC   4.023263
21                         mean BOCNoCarProblem                         sqDiffMeanBOCNoCarProblem   4.144774
22                         fuel BOCNoCarProblem                         sqDiffFuelBOCNoCarProblem   4.066210
23                  fuelTyreLap BOCNoCarProblem                  sqDiffFuelTyreLapBOCNoCarProblem   4.019297
24           fuelTyreTypeAndAge BOCNoCarProblem           sqDiffFuelTyreTypeAndAgeBOCNoCarProblem   4.028243
25 fuelTyreTypeAndAgeDWLockedIn BOCNoCarProblem sqDiffFuelTyreTypeAndAgeDWLockedInBOCNoCarProblem   4.036628
26                           30              30                                          sqDiff30   4.100047

# carproblem is a bit of a mixed bag, seems to help clearlap marginally but hinder BOC slightly.
# solution is probably to exclude the extremem silly ones but that will required
# but we still have the issue about BOC being better than clearLap, that's a bugger

### sequential model run is a bit of a nightmare to follow. let's try to compress things a bit, have more sensible variable names etc
source('project/model hierarchy/model-hierarchy-setup.r')

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


allModelName = c('mean', 'fuel', 'fuelTyreLap', 'fuelTyreTypeAndAge')
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
            modelName       validName                              sqDiffName meanSqDiff
1                mean        nonRogue                      sqDiffMeanNonRogue   4.206861
2                fuel        nonRogue                      sqDiffFuelNonRogue   4.137394
3         fuelTyreLap        nonRogue               sqDiffFuelTyreLapNonRogue   4.095074
4  fuelTyreTypeAndAge        nonRogue        sqDiffFuelTyreTypeAndAgeNonRogue   4.062642
5                mean        clearLap                      sqDiffMeanClearLap   4.540252
6                fuel        clearLap                      sqDiffFuelClearLap   4.254839
7         fuelTyreLap        clearLap               sqDiffFuelTyreLapClearLap   4.157106
8  fuelTyreTypeAndAge        clearLap        sqDiffFuelTyreTypeAndAgeClearLap   4.144081
9                mean         model30                       sqDiffMeanModel30   4.505639
10               fuel         model30                       sqDiffFuelModel30   4.217757
11        fuelTyreLap         model30                sqDiffFuelTyreLapModel30   4.123394
12 fuelTyreTypeAndAge         model30         sqDiffFuelTyreTypeAndAgeModel30   4.102854
13               mean             BOC                           sqDiffMeanBOC   4.167592
14               fuel             BOC                           sqDiffFuelBOC   4.091630
15        fuelTyreLap             BOC                    sqDiffFuelTyreLapBOC   4.033025
16 fuelTyreTypeAndAge             BOC             sqDiffFuelTyreTypeAndAgeBOC   4.014162
17               mean BOCNoCarProblem               sqDiffMeanBOCNoCarProblem   4.144774
18               fuel BOCNoCarProblem               sqDiffFuelBOCNoCarProblem   4.066210
19        fuelTyreLap BOCNoCarProblem        sqDiffFuelTyreLapBOCNoCarProblem   4.019297
20 fuelTyreTypeAndAge BOCNoCarProblem sqDiffFuelTyreTypeAndAgeBOCNoCarProblem   4.028243
21                 30              30                                sqDiff30   4.100047

# carproblem is a bit of a mixed bag, seems to help clearlap marginally but hinder BOC slightly.
# solution is probably to exclude the extremem silly ones but that will required
# but we still have the issue about BOC being better than clearLap, that's a bugger

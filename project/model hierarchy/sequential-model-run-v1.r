### sequential model run is a bit of a nightmare to follow. let's try to compress things a bit, have more sensible variable names etc
source('project/model hierarchy/model-hierarchy-setup.r')

#########################################################################################
#################### first make all the validity combinations ###########################
#########################################################################################

dum = MakePredNValidForLblValid(with(lbl, !isRogue & isValidRace30), 5,
								'nonRogue',	rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl

dum = MakePredNValidForLblValid(with(lbl, !isRogue & !inTraffic & isValidRace30), 5,
								'clearLap', rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl

dum = MakePredNValidForLblValid(with(lbl, !isRogue & !isOvertaking & isValidRace30), 5,
								'BOC', rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl

dum = MakePredNValidForLblValid(with(lbl, !isRogue & !isOvertaking & isValidRace30), 5,
								'BOC', rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl

dum = MakePredNValidForLblValid(with(lbl, !isRogue & !isOvertaking & isValidRace30 & !isCarProblem), 5,
								'BOCNoCarProblem', rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl

#########################################################################################
############################## then run all the models ##################################
#########################################################################################


allModelName = c('mean', 'fuel', 'fuelTyreLap', 'fuelTyreTypeAndAge')
allValidName = c('nonRogue', 'clearLap', 'BOC', 'BOCNoCarProblem')

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
1                mean        nonRogue                      sqDiffMeanNonRogue   4.195455
2                fuel        nonRogue                      sqDiffFuelNonRogue   4.126023
3         fuelTyreLap        nonRogue               sqDiffFuelTyreLapNonRogue   4.080480
4  fuelTyreTypeAndAge        nonRogue        sqDiffFuelTyreTypeAndAgeNonRogue   4.047914
5                mean        clearLap                      sqDiffMeanClearLap   4.601325
6                fuel        clearLap                      sqDiffFuelClearLap   4.257564
7         fuelTyreLap        clearLap               sqDiffFuelTyreLapClearLap   4.156379
8  fuelTyreTypeAndAge        clearLap        sqDiffFuelTyreTypeAndAgeClearLap   4.141741
9                mean             BOC                           sqDiffMeanBOC   4.154972
10               fuel             BOC                           sqDiffFuelBOC   4.082310
11        fuelTyreLap             BOC                    sqDiffFuelTyreLapBOC   4.027511
12 fuelTyreTypeAndAge             BOC             sqDiffFuelTyreTypeAndAgeBOC   4.009401
13               mean BOCNoCarProblem               sqDiffMeanBOCNoCarProblem   4.134390
14               fuel BOCNoCarProblem               sqDiffFuelBOCNoCarProblem   4.069516
15        fuelTyreLap BOCNoCarProblem        sqDiffFuelTyreLapBOCNoCarProblem   4.025696
16 fuelTyreTypeAndAge BOCNoCarProblem sqDiffFuelTyreTypeAndAgeBOCNoCarProblem   4.034748
17                 30              30                                sqDiff30   4.100047
# so if you compare to clearLap, model 30 is doing something right, if you compare to BOC it's doing something wrong. let's try to figure it out

## let's try to reproduce model 30

rddf = f1laptimelm:::MakeNormDriverCoef(rddf, raceDF, 30, 'race')
rddf = SmoothAndCalculateSqDiff(rddf, 'mod30DCoef', 'mod30PredNValid', 'efp30', 'sqDiff30')

dum = MakePredNValidForLblValid(with(lbl, !isRogue & !inTraffic &
											!isCarProblem & !isOutlier30 &
											isValidTyre30 & isValidRace30), 1,
								'model30Generated', rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl
rddf = RunModelAllRace('fuelTyreTypeAndAge',
							'model30Generated',
							rddf, lbl)
							
dum = c('sqDiff30', 'sqDiffFuelTyreTypeAndAgeModel30Generated')
rddf$isComparisonValid = complete.cases(rddf[,dum])

tibble::enframe(colMeans(rddf[rddf$isComparisonValid, dum]))
# A tibble: 2 x 2
  name                                       value
  <chr>                                      <dbl>
1 sqDiff30                                 4.10005
2 sqDiffFuelTyreTypeAndAgeModel30Generated 4.10285
## yes that looks good.

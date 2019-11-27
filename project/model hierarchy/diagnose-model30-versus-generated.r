### this will hopefulyl be a short file, want to work out what model 30 is doing right and wrong compared to the generated models

source('project/model hierarchy/model-hierarchy-setup.r')


dum = MakePredNValidForLblValid(
		with(lbl, !isRogue & !inTraffic & !isOutlier30 & !isCarProblem &
					isValidRace30 & isValidTyre30),
				1, 'clearLap', rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl

rddf = RunModelAllRace('fuelTyreTypeAndAge', 'clearLap', rddf, lbl)

rddf = f1laptimelm:::MakeNormDriverCoef(rddf, raceDF, 30, 'race')
rddf = SmoothAndCalculateSqDiff(rddf, 'mod30DCoef', 'mod30PredNValid', 'efp30', 'sqDiff30')

### turns out it was due to validtyre.
### VEERY close now. think the difference is due to model30 retaining car problem drivers when fitting the lm

# right, let's just tinker to see what makes it worse exactly

		with(lbl, !isRogue & !inTraffic & !isOutlier30 & !isCarProblem &
					isValidRace30 & isValidTyre30),
				1, 'clearLap', rddf, lbl)

dum = MakePredNValidForLblValid(
		with(lbl, !isRogue &
					isValidRace30),
				1, 'nonRogue1', rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl
rddf = RunModelAllRace('fuelTyreTypeAndAge', 'nonRogue1', rddf, lbl)

### exclude overtaking laps
dum = MakePredNValidForLblValid(
		with(lbl, !isRogue & !isOvertaking &
					isValidRace30),
				1, 'nonRogueOvertake1', rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl
rddf = RunModelAllRace('fuelTyreTypeAndAge', 'nonRogueOvertake1', rddf, lbl)

### exclude car problem laps
dum = MakePredNValidForLblValid(
		with(lbl, !isRogue & !isOvertaking & !isCarProblem &
					isValidRace30),
				1, 'nonRogueOvertakeCarProblem1', rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl
rddf = RunModelAllRace('fuelTyreTypeAndAge', 'nonRogueOvertakeCarProblem1', rddf, lbl)

### exclude invalid tyres
dum = MakePredNValidForLblValid(
		with(lbl, !isRogue & !isOvertaking &
					isValidTyre30 & isValidRace30),
				1, 'nonRogueOvertakeYesValidTyre1', rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl
rddf = RunModelAllRace('fuelTyreTypeAndAge', 'nonRogueOvertakeYesValidTyre1', rddf, lbl)

### exclude outlier laps
dum = MakePredNValidForLblValid(
		with(lbl, !isRogue & !isOvertaking & !isOutlier30 &
					isValidRace30),
				1, 'nonRogueOvertakeOutlier1', rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl
rddf = RunModelAllRace('fuelTyreTypeAndAge', 'nonRogueOvertakeOutlier1', rddf, lbl)

### summary

allSqDiffName = paste0('sqDiffFuelTyreTypeAndAgeNonRogue',
					c('1', 'Overtake1', 'OvertakeCarProblem1', 'OvertakeYesValidTyre1', 'OvertakeOutlier1'))

rddf$isComparisonValid = complete.cases(rddf[,allSqDiffName])
tibble::enframe(colMeans(rddf[rddf$isComparisonValid, allSqDiffName]))

name                                                    value
  <chr>                                                   <dbl>
1 sqDiffFuelTyreTypeAndAgeNonRogue1                     4.06264
2 sqDiffFuelTyreTypeAndAgeNonRogueOvertake1             4.01416
3 sqDiffFuelTyreTypeAndAgeNonRogueOvertakeCarProblem1   4.02824
4 sqDiffFuelTyreTypeAndAgeNonRogueOvertakeYesValidTyre1 4.01509
5 sqDiffFuelTyreTypeAndAgeNonRogueOvertakeOutlier1      4.00779

# the carproblem one can't be quite right, in extreme cases surely you need to exclude. but it looks like we're excluding a little too readily maybe? hardly significant though

# i say, let's retain everything except the validtyre thing which seems to make no difference

dum = MakePredNValidForLblValid(
		with(lbl, !isRogue & !isOvertaking & !isOutlier30 & !isCarProblem &
					isValidRace30),
				1, 'default1', rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl
rddf = RunModelAllRace('fuelTyreTypeAndAge', 'default1', rddf, lbl)

allSqDiffName = c(paste0('sqDiffFuelTyreTypeAndAgeNonRogue',
					c('1', 'Overtake1', 'OvertakeCarProblem1', 'OvertakeYesValidTyre1', 'OvertakeOutlier1')), 'sqDiffFuelTyreTypeAndAgeDefault1')

rddf$isComparisonValid = complete.cases(rddf[,allSqDiffName])
tibble::enframe(colMeans(rddf[rddf$isComparisonValid, allSqDiffName]))

# now let's tweak the minLapThreshold

dum = MakePredNValidForLblValid(
		with(lbl, !isRogue & !isOvertaking & !isOutlier30 & !isCarProblem &
					isValidRace30),
				2, 'default2', rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl
rddf = RunModelAllRace('fuelTyreTypeAndAge', 'default2', rddf, lbl)

dum = MakePredNValidForLblValid(
		with(lbl, !isRogue & !isOvertaking & !isOutlier30 & !isCarProblem &
					isValidRace30),
				3, 'default3', rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl
rddf = RunModelAllRace('fuelTyreTypeAndAge', 'default3', rddf, lbl)

dum = MakePredNValidForLblValid(
		with(lbl, !isRogue & !isOvertaking & !isOutlier30 & !isCarProblem &
					isValidRace30),
				4, 'default4', rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl
rddf = RunModelAllRace('fuelTyreTypeAndAge', 'default4', rddf, lbl)

dum = MakePredNValidForLblValid(
		with(lbl, !isRogue & !isOvertaking & !isOutlier30 & !isCarProblem &
					isValidRace30),
				5, 'default5', rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl
rddf = RunModelAllRace('fuelTyreTypeAndAge', 'default5', rddf, lbl)

allSqDiffName = paste0('sqDiffFuelTyreTypeAndAgeDefault', 1:5)

rddf$isComparisonValid = complete.cases(rddf[,allSqDiffName])
tibble::enframe(colMeans(rddf[rddf$isComparisonValid, allSqDiffName]))

# A tibble: 5 x 2
  name                               value
  <chr>                              <dbl>
1 sqDiffFuelTyreTypeAndAgeDefault1 4.01908
2 sqDiffFuelTyreTypeAndAgeDefault2 4.01848
3 sqDiffFuelTyreTypeAndAgeDefault3 4.02237
4 sqDiffFuelTyreTypeAndAgeDefault4 4.01733
5 sqDiffFuelTyreTypeAndAgeDefault5 4.02570

# ok, so that filtering is completely pointless then, that's helpful, we can get rid of some code

### i still think it'll be interesting to see how qualifying/supertimes/my model do

source('project/model hierarchy/model-hierarchy-setup.r')

####################################################
################ supertimes ########################
####################################################

## now let's get hold of supertimes
sessionTimeDF = read.csv(paste0(USERPATH, 'project/validate via finpos/session-time.csv'),
							as.is = TRUE) %>%
					rename(free1 = X1stFree, free2 = X2ndFree, free3 = X3rdFree,
							quali1 = X1stQualif, quali2 = X2ndQualif, quali3 = X3rdQualif)

# but we need th fastest race lap, then weed out outliers
fastestRaceLap = lbl %>%
					group_by(race, driver) %>%
					summarise(fastestLap = min(sec))

sessionTimeDF = left_join(sessionTimeDF, fastestRaceLap, c('race', 'driver'))

min2 = function(x) {
	if (all(is.na(x))) return(NA)
	return(min(x, na.rm = TRUE))
}
sessionTimeDF$rawDCoef = apply(sessionTimeDF[,c('free1', 'free2', 'free3',
												'quali1', 'quali2', 'quali3',
												'fastestLap')], 1, min2)

rddf$dCoefSuperTime = NormaliseRawDCoef(sessionTimeDF)

### 2015 usa is a bit of a problem, just ditch it
rddf$dCoefSuperTime[rddf$race == '2015usa'] = NA
rddf$predNValidSuperTime = with(rddf, ifelse(!is.na(dCoefSuperTime), 1, 0))


rddf = SmoothAndCalculateSqDiff(rddf, 'dCoefSuperTime',
										'predNValidSuperTime',
										'efpSuperTime', 'sqDiffSuperTime')

####################################################
################ qualifying ########################
####################################################

rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, 30, 'qual')

mySmooth = f1smoothing::GetSmooth(qrToFit = 'r', qrToPredict = 'rfinpos', useStretch = FALSE, fwbw = 'flat',
				customSmoothInfo = list(qualRace = 'race',
										dCoefName = 'mod30QualDCoef',
										predNValidName = 'modQualPredNValid'),
				expectedFinPosName = 'efpQual')
rddf = lazy_left_join(rddf, mySmooth$smoothDF, c('race', 'driver'), 'efpQual')
rddf = rddf %>%
		mutate(sqDiffQual := cleanliness * (officialFinishingPosition - efpQual)^2)

####################################################
####################### mod30 ######################
####################################################

rddf = f1laptimelm:::MakeNormDriverCoef(rddf, raceDF, 30, 'race')
rddf = SmoothAndCalculateSqDiff(rddf, 'mod30DCoef', 'mod30PredNValid', 'efp30', 'sqDiff30')
	
if (FALSE) {
	smooth30 = f1smoothing:::GetSmooth('r', 'rfinpos', useStretch = FALSE, fwbw = 'fwbw',
								modelChoice = 30, myRace = '2018abudhabi',
								expectedFinPosName = 'expectedFinPos30')
	rddf = lazy_left_join(rddf, smooth30$smoothDF, c('race', 'driver'), 'expectedFinPos30')
	rddf = rddf %>%
			mutate(sqDiff30 = cleanliness * (officialFinishingPosition - expectedFinPos30)^2)
}
		
allModelName = paste0('sqDiff', c('SuperTime', '30', 'Qual'))
rddf$isComparisonValid = complete.cases(rddf[,allModelName])

if (FALSE) {
	# this was with fwbw downweight
tibble::enframe(sort(colMeans(rddf[rddf$isComparisonValid, allModelName])))
# # A tibble: 3 x 2
  # name              value
  # <chr>             <dbl>
# 1 sqDiff30        3.99872
# 2 sqDiffSuperTime 4.07547
# 3 sqDiffQual      4.19461
}

tibble::enframe(sort(colMeans(rddf[rddf$isComparisonValid, allModelName])))
# A tibble: 3 x 2
  name              value
  <chr>             <dbl>
1 sqDiff30        4.10103
2 sqDiffSuperTime 4.17647
3 sqDiffQual      4.30886

# so supertimes aren't actually too bad. not bad at all really. although we know we can get better somehow, by including all laps. but i just don't like it
# putting it into persepctive however, just doing the fuel adjustment on clear laps is equal to supertimes. but who mnows how much better supertimes are if you get rid of outliers. don't know how many there are, you'd have to have problems in literally every session

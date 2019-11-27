### so average lap times is just as good as my model at predicting finpos. Let's look into that

smooth30 = GetSmooth('r', 'rfinpos', useStretch = FALSE, fwbw = 'fwbw',
							modelChoice = 30, myRace = '2018abudhabi',
							expectedFinPosName = 'expectedFinPos30')


dCoefName= "dCoef0"
predNValidName= "predNValid0"
expectedFinPosName= "expectedFinPos0"
sqDiffName= "sqDiff0"

smoothMean = GetSmooth(qrToFit = 'r', qrToPredict = 'rfinpos', useStretch = FALSE, fwbw = 'fwbw',
				customSmoothInfo = list(qualRace = 'race',
										dCoefName = dCoefName,
										predNValidName = predNValidName),
				expectedFinPosName = expectedFinPosName)

rddf = lazy_left_join(rddf, smooth30$smoothDF, c('race', 'driver'), 'expectedFinPos30')
rddf = lazy_left_join(rddf, smoothMean$smoothDF, c('race', 'driver'), 'expectedFinPos0')

print(rddf[which.max(abs(rddf$expectedFinPos30 - rddf$expectedFinPos0)),c('race', 'driver')])
# perez at spain 2011, what happened there




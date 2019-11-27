### want to allow custom smothing, let's set one up. We'll cheat, by making it identical to model 30

source('project/cleansmooth/smoothing-plus-finpos.r')
source('project/cleansmooth/smoothing-admin-funct.r')
suppressWarnings(library(data.table))

# firstly check I've not broken anything that already works
LoadAllData()
standardSmooth = GetSmooth('r', 'r', useStretch = TRUE, fwbw = 'fwbw',
							modelChoice = 30, myRace = '2018abudhabi')

#### now check a few things that should agree actually do
### although this is a bit silly, we should check they agree at predicting rfinpos

rddf = f1laptimelm:::MakeNormDriverCoef(rddf, raceDF, 30, 'race')
rddf[,'customDCoef'] = rddf[,'mod30DCoef']
rddf[,'customPredNValid'] = rddf[,'mod30PredNValid']

modelChoice = 30
customSmoothInfo = list(qualRace = 'race',
								dCoefName = 'customDCoef',
								predNValidName = 'customPredNValid')

standardSmooth = GetSmooth('r', 'r', useStretch = TRUE, fwbw = 'fwbw',
							modelChoice = 30, myRace = '2018abudhabi')

customSmooth = GetSmooth('r', 'r', useStretch = TRUE, fwbw = 'fwbw',
							modelChoice = 30, myRace = '2018abudhabi',
							customSmoothInfo = customSmoothInfo)

dum = full_join(standardSmooth$smoothDF, customSmooth$smoothDF, c('race', 'driver'))
CheckVectorsAreEqual(dum$smoothDCoef.x, dum$smoothDCoef.y,
							tol = 0.001, verbose = FALSE) &
						CheckVectorsAreEqual(dum$smoothWgt.x, dum$smoothWgt.y,
							tol = 0.001, verbose = FALSE)				

###################################

rddf = f1laptimelm:::MakeNormDriverCoef(rddf, raceDF, 30, 'qual')
rddf[,'customDCoef'] = rddf[,'mod30QualDCoef']
rddf[,'customPredNValid'] = rddf[,'modQualPredNValid']


modelChoice = 30
customSmoothInfo = list(qualRace = 'qual',
								dCoefName = 'customDCoef',
								predNValidName = 'customPredNValid')

standardSmooth = GetSmooth('q', 'q', useStretch = TRUE, fwbw = 'fwbw',
							modelChoice = 30, myRace = '2018abudhabi')

customSmooth = GetSmooth('q', 'q', useStretch = TRUE, fwbw = 'fwbw',
							modelChoice = 30, myRace = '2018abudhabi',
							customSmoothInfo = customSmoothInfo)

dum = full_join(standardSmooth$smoothDF, customSmooth$smoothDF, c('race', 'driver'))
CheckVectorsAreEqual(dum$smoothDCoef.x, dum$smoothDCoef.y,
							tol = 0.001, verbose = FALSE) &
						CheckVectorsAreEqual(dum$smoothWgt.x, dum$smoothWgt.y,
							tol = 0.001, verbose = FALSE)				
							### yes


rddf = f1laptimelm:::MakeNormDriverCoef(rddf, raceDF, 30, 'qual')
rddf[,'customDCoef'] = rddf[,'mod30QualDCoef']
rddf[,'customPredNValid'] = rddf[,'modQualPredNValid']
customSmoothInfo = list(qualRace = 'qual',
								dCoefName = 'customDCoef',
								predNValidName = 'customPredNValid')

standardSmooth = GetSmooth('q', 'q', useStretch = FALSE, fwbw = 'fwbw',
							modelChoice = 30, myRace = '2018abudhabi')

customSmooth = GetSmooth('q', 'q', useStretch = FALSE, fwbw = 'fwbw',
							myRace = '2018abudhabi',
							customSmoothInfo = customSmoothInfo)

dum = full_join(standardSmooth$smoothDF, customSmooth$smoothDF, c('race', 'driver'))
CheckVectorsAreEqual(dum$smoothDCoef.x, dum$smoothDCoef.y,
							tol = 0.001, verbose = FALSE) &
CheckVectorsAreEqual(dum$smoothWgt.x, dum$smoothWgt.y,
							tol = 0.001, verbose = FALSE)				

### yes


rddf = f1laptimelm:::MakeNormDriverCoef(rddf, raceDF, 30, 'race')
rddf[,'customDCoef'] = rddf[,'mod30DCoef']
rddf[,'customPredNValid'] = rddf[,'mod30PredNValid']
customSmoothInfo = list(qualRace = 'race',
								dCoefName = 'customDCoef',
								predNValidName = 'customPredNValid')

standardSmooth = GetSmooth('r', 'r', useStretch = FALSE, fwbw = 'fwbw',
							modelChoice = 30, myRace = '2018abudhabi')

customSmooth = GetSmooth('r', 'r', useStretch = FALSE, fwbw = 'fwbw',
							myRace = '2018abudhabi',
							customSmoothInfo = customSmoothInfo)

dum = full_join(standardSmooth$smoothDF, customSmooth$smoothDF, c('race', 'driver'))
CheckVectorsAreEqual(dum$smoothDCoef.x, dum$smoothDCoef.y,
							tol = 0.001, verbose = FALSE) &
CheckVectorsAreEqual(dum$smoothWgt.x, dum$smoothWgt.y,
							tol = 0.001, verbose = FALSE)				


### yes


rddf = f1laptimelm:::MakeNormDriverCoef(rddf, raceDF, 30, 'race')
rddf[,'customDCoef'] = rddf[,'mod30DCoef']
rddf[,'customPredNValid'] = rddf[,'mod30PredNValid']
customSmoothInfo = list(qualRace = 'race',
								dCoefName = 'customDCoef',
								predNValidName = 'customPredNValid')

standardSmooth = GetSmooth('r', 'r', useStretch = FALSE, fwbw = 'bw',
							modelChoice = 30, myRace = '2018abudhabi')

customSmooth = GetSmooth('r', 'r', useStretch = FALSE, fwbw = 'bw',
							myRace = '2018abudhabi',
							customSmoothInfo = customSmoothInfo)

dum = full_join(standardSmooth$smoothDF, customSmooth$smoothDF, c('race', 'driver'))
CheckVectorsAreEqual(dum$smoothDCoef.x, dum$smoothDCoef.y,
							tol = 0.001, verbose = FALSE) &
CheckVectorsAreEqual(dum$smoothWgt.x, dum$smoothWgt.y,
							tol = 0.001, verbose = FALSE)				

# i'm happy, this is working

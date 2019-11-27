### as we modify the smoothing code, want to make sure we're agreeing where relevant to the existing code

# need to get list of things to compare it on

LoadAllData()
suppressWarnings(library(data.table))
source('project/cleansmooth/smoothing-plus-finpos.r')
source('project/cleansmooth/smoothing-admin-funct.r')

allcombo=f1smoothing::GenerateCombo(filterlist=list(modelchoice = c(30, 'qual')))
newAllCombo=GenerateCombo(filterList=list(modelChoice = c(30, 'qual')))

remax = FALSE
if (!remax) agreement = rep(NA, nrow(allcombo))
if (remax) agreement = NULL
for (j in 1:nrow(allcombo)) {
	existingmethod = f1smoothing::GetSmooth(qrtofit=allcombo$qrtofit[j],
									qrtopredict=allcombo$qrtopredict[j],
									modelchoice=allcombo$modelchoice[j],
									usestretch=allcombo$usestretch[j],
									fwbw=allcombo$fwbw[j],
									myRace=raceDF$race[nrace],
									resetMaxTheta = remax)
	
	newmethod = GetSmooth(qrToFit=newAllCombo$qrToFit[j],
							qrToPredict=newAllCombo$qrToPredict[j],
							modelChoice=newAllCombo$modelChoice[j],
							useStretch=newAllCombo$useStretch[j],
							fwbw=newAllCombo$fwbw[j],
							myRace=raceDF$race[nrace],
									resetMaxTheta = remax)

	if (!remax) {
		dum = full_join(existingmethod$smoothDF, newmethod$smoothDF, c('race', 'driver'))
		agreement[j] = CheckVectorsAreEqual(dum$smoothDCoef.x, dum$smoothDCoef.y,
							tol = 0.001, verbose = FALSE) &
						CheckVectorsAreEqual(dum$smoothWgt.x, dum$smoothWgt.y,
							tol = 0.001, verbose = FALSE)
	}
	if (remax) {
		agreement[[j]] = cbind(existingmethod$optcoef, newmethod$optcoef)
	}
}

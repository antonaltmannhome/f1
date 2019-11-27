### need a testing framework for the changes we're going to make, let's make it as painless as possible

.F1Startup()
LoadAllData()
library(data.table)

source('project/cleansmooth/smoothing-funct2.r')

allcombo=f1smoothing::GenerateCombo(filterlist=list(method = 'downweight',
														modelchoice = c(30, 'qual')))

remax = FALSE
if (!remax) agreement = rep(NA, nrow(allcombo))
if (remax) agreement = NULL
for (j in 1:nrow(allcombo)) {
	existingmethod = f1smoothing::GetSmooth(method=allcombo$method[j],
									qrtofit=allcombo$qrtofit[j],
									qrtopredict=allcombo$qrtopredict[j],
									modelchoice=allcombo$modelchoice[j],
									usestretch=allcombo$usestretch[j],
									fwbw=allcombo$fwbw[j],
									myRace=raceDF$race[nrace],
									resetMaxTheta = remax)
	
	runmode = 'datatable'
	newmethod = GetSmooth(qrtofit=allcombo$qrtofit[j],
							qrtopredict=allcombo$qrtopredict[j],
							modelchoice=allcombo$modelchoice[j],
							usestretch=allcombo$usestretch[j],
							fwbw=allcombo$fwbw[j],
							myRace=raceDF$race[nrace],
									resetMaxTheta = remax)

	if (!remax) {
		agreement[j] = all.equal(existingmethod$smval, newmethod$smval) &
					all.equal(existingmethod$numobval, newmethod$numobval)
	}
	if (remax) {
		agreement[[j]] = cbind(existingmethod$optcoef, newmethod$optcoef)
	}
}

# we can delete MixQualifyingRaceDCoef! DONE
# SmoothForSingleRace is too slow: maybe replace the group_by with tapply, maybe change the tibble to a data frame DONE
# have tried to do that, by making OOSRddf. But it doesn't actually agree with existing method at all DONE
# actually, can't we just do the summing by add them all up, then taking away what's not valid? NO. DONE
# still need to check that it optimises to almost exactly the same thing, but we'll do that after speeding it up DONE
# get rid of isvalidpred, just write modQyalPRedNValid>0 etc in the sqdiff section. rename MakeValidPred something like DetectDriverHasOTherData DONE
# separate out the un-stretching into separate function - we will be wanting to add the yearlycoef style driver adjustment into this function DONE
# usecurrentrace seems to be being forgotten
# we're maybe not miles away from combining the yearly and smooths together. the big job is to get the driver estimate adjustments in, but it's a good idea. they have different functions for the amount of weight on rddf row, but thtat's ok, will be interesting to see which is better
# then, it might not be so hard to add in the pairwise finpos prediction
# MakeGeneralColumnName is disgusting, i think we can use the f1laptimelm modelname stuff to clean it. no it's not that simple, MakeModelToUseName for model30 doesn't return the normalised dcoefs which is what we need. althought htere's a case to be made for improving that, but not immediately
# acqual could be done more nicely like stretch, could there be a function that does (1) acqual) (2) usestretch (3) driver adjustments, and you specify which direction you want to go DONE
# want to get rid of method from the arguments, but it does make comparison with existing method a nightmare, so don't do until the end
# make it so that you can pass EITHER modelchoice OR the name of the column you want to smooth (though you would need to send quali column name, prednvalid name too etc). But it needs to be NA or not in the same places as model 30, otherwise there's no way you could compare difference models.

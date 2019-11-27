
RunQualifyingModel = function() {

	LoadAllData()

	### and after that, simulate qualifying probs so that we can eliminate low incentive laps

	### not so sure about those coefs any more - massive selection bias problem, if eg heidfeld makes Q2 in rae 1 but Q3 in race 2, it'll look like Q3 is faster than Q2 since we're comparing a lap from Heidfeld where he WAS in the top10 to one where he wasn't
	qdf = lazy_left_join(qdf, raceDF, 'race', 'circuit')
	qdf = lazy_left_join(qdf, rddf, c('race', 'driver'), 'team')
	qdf$driverteam = with(qdf, paste(driver, team))
	
	qualifyingSessionDF = f1qualifying::GetNumberOfQualifiers(qualifyingSessionDF, rddf)

	raceDF = MakeIsValidQualifying(raceDF, qualifyingSessionDF = qualifyingSessionDF)
	qdf = f1qualifying::MakeQualifyingIsValid(qdf)

	yearToUpdate = with(raceDF, unique(year[!doneQualifyingModel]))

	if (length(yearToUpdate)>0) {
		wgt=0.1
		### 0.1 seems to be about the best

		for (yi in 1:length(yearToUpdate)) {
			### fiddly to decide which driver get excluded - we want to have coefs for drivers where possible, preferably from the finqs set, if that causes NAs, then resort to the validbackup ones

			rrToModel = with(qdf, unique(rr[isvalidfit & year == yearToUpdate[yi]]))

			qdf = f1qualifying::GetPreliminaryPredSec(qdf, rrToModel, yearToUpdate[yi], wgt) # that fills out/updates predSec for myYear

			mysd=sd( (qdf$sec-qdf$predSec)[qdf$isvalidfit & qdf$year<=yearToUpdate[yi]])
			### right, now we simulate qualifying probabilities based on these numbers
			qdf = f1qualifying::SimulateQualProb(qdf, qualifyingSessionDF, rrToModel, mysd)

			### excellent, that gives us a much better guide than isvalidfit as to which times we can be confident using

			qdf = f1qualifying::CalculateQualWgt(qdf)

			currentYrRr = with(raceDF, which(year == yearToUpdate[yi]))
			rddf = f1qualifying::CalculateRawDCoefPredNValid(rddf, raceDF, qdf, currentYrRr)

			### we'd like to be able to normalise the raw coefs though and have a circuit intercept, that way we can compare driver coefs from different races
			### so for all races this year, do an lm of all drivers at all races

			if (length(rrToModel)==1) {
				sax=with(rddf,which(year==yearToUpdate[yi] & !is.na(modQualRawDCoef)))
				dum=list(qcoef=mean(rddf$modQualRawDCoef[sax]))
				names(dum$qcoef)=raceDF$circuit[rrToModel]
			}
			if (length(rrToModel)>1) {
				dum=f1qualifying::getqint(rrToModel, rddf)
			}
			raceDF$modQualQualIntercept[rrToModel]=dum$qcoef[match(raceDF$circuit[rrToModel],names(dum$qcoef))]

			### now store everything
			### all the qualifying session sepcific things first

			sax=which(qdf$year==yearToUpdate[yi] & qdf$isvalidpred)
			sqlLazyUpdate(qdf[sax,], 'qualifying', c('race', 'session', 'driver'), c('predSec','qualProb','qualWgt'))
			## then the race summary statistics
			sax=with(rddf,which(year==yearToUpdate[yi]))
			sqlLazyUpdate(rddf[sax,], 'racedriver', c('race', 'driver'), 'modQualPredNValid')
			sax=with(rddf,which(year==yearToUpdate[yi] & modQualPredNValid > 0))
			sqlLazyUpdate(rddf[sax,], 'racedriver', c('race', 'driver'), 'modQualRawDCoef')
			### then overall qualifying intercept
			sax=which(raceDF$year==yearToUpdate[yi] & raceDF$isValidQual)
			sqlLazyUpdate(raceDF[sax,], 'race', 'race', 'modQualQualIntercept')
			### updating database to know we're done works a little differently, we need to update for all races in this year
			rsax=with(raceDF, which(year==yearToUpdate[yi] & !doneQualifyingModel))
			for (ri in rsax) {
				raceDF$doneQualifyingModel[ri]=TRUE
				sqlLazyUpdate(raceDF[ri,], 'race', 'race', 'doneQualifyingModel')
			}
			cat('Have updated qualifying model for',yearToUpdate[yi],'\n')
		}
	}
}

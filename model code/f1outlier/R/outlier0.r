#### think the outlier exclusions are too aggressive, let's try another idea

### do we need to update our outliers vector? check to see if some isGood0==1 values are 0

GetOutlier0 = function() {

	LoadAllData()
	mostRecentRace = raceDF$race[nrace]

	outliercutoff=0.001
	extremeoutliercutoff=1E-08

	### we'll be needing this during the process
	dum=f1smoothing::GetSmooth(qrToFit = 'q',
							qrToPredict = 'q',
							modelChoice = 'qual',
							fwbw = 'fwbw',
							useStretch = TRUE,
							smoothDCoefName = 'qualSmoothDCoef')
	rddf = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'qualSmoothDCoef')


	allUpToDate = with(raceDF, all(doneRaceOutlier))

	if (!allUpToDate) {
		# potentially want to update choice of outliers for previous races this season, so loop through race so far this season
		lbl = f1validity::MakeIsRogue(lbl) %>%
				mutate(isGood = !isRogue)
		yearToUpdate=with(raceDF, unique(year[!doneRaceOutlier]))
		for (yi in 1:length(yearToUpdate)) {
			rrToModel = with(raceDF, rr[year == yearToUpdate[yi]])
			### NB once we're happy with the model agreeing, start using isValidRace properly here
			for (ri in rrToModel) {
				anyGoodLap = any(with(lbl, isGood[rr==ri]))
				if (!anyGoodLap) {
					lbl$isOutlier0[lbl$rr==ri] = FALSE
				}
				if (anyGoodLap) {
					cat('About to start check for outliers for',raceDF$race[ri],'\n')
					risax=which(lbl$rr==ri)
					lbl$updatedIsGood = lbl$isGood
					lbl$isOutlier0[risax]=0
					### that's our starting point, now let's iterate through the problem laps
					
					finished=F
					sweepcount=1
					while(!finished) {
						
						saxgood=risax[which(lbl$updatedIsGood[risax])]
						lbl$smoothsec=NA
						### smooth each stint using antsmooth, but excluding the current lap
						for (j in saxgood) {
							sax2=saxgood[which(lbl$driver[saxgood]==lbl$driver[j] & lbl$stint[saxgood]==lbl$stint[j])]
							if (length(sax2)>1) {
								sax2b=saxgood[which(lbl$driver[saxgood]==lbl$driver[j] & lbl$stint[saxgood]==lbl$stint[j] & saxgood!=j)]
								### fit the smooth
								dum=antsmooth(lbl$lap[sax2b],lbl$sec[sax2b],0.5)
								### extrapolate with splinefun
								sfunct=splinefun(lbl$lap[sax2b],dum)
								lbl$smoothsec[j]=sfunct(lbl$lap[j])
							}
						}
						
						###now if we take the e.g. 90% best predicted ones, what is their sd?
						qdum=quantile(lbl$sec-lbl$smoothsec,pr=c(0.05,0.95),na.rm=T)
						keep=which(lbl$sec-lbl$smoothsec>qdum[1] & lbl$sec-lbl$smoothsec<qdum[2])
						approxsd=1.5*sd( (lbl$sec-lbl$smoothsec)[keep])
						### right, we'll reject anything less than e.g. 0.01 likely
						### but we also want to compare to qualifying times
						### adjust smoothsec for fuel load
						mod=lm(lbl$smoothsec[keep]~lbl$fuel[keep] + factor(lbl$driver[keep]))
						fuelcoef=coef(mod)[grep('lbl\\$fuel',names(coef(mod)))]
						lbl$adjsmoothsec=lbl$smoothsec - fuelcoef*lbl$fuel
						### that includes the circuit intercept though, so need to correct for that
						tabdat=rddf[which(rddf$rr==ri),c('driver','qualSmoothDCoef')]
						## don't have a qualdcoef? fine, use smoothed number instead
						#tabdat$qdum=with(tabdat,ifelse(!is.na(qualdcoef),qualdcoef,qualSmoothDCoef))
						rdum=tapply(lbl$adjsmoothsec[keep],lbl$driver[keep],mean)
						tabdat$madjsmooth=as.numeric(rdum)[match(tabdat$driver,names(rdum))]
						sax=which(!is.na(tabdat$qualSmoothDCoef + tabdat$madjsmooth))
						dum=with(tabdat[sax,],mean(madjsmooth-qualSmoothDCoef))
						tabdat$qtouse=tabdat$qualSmoothDCoef + dum
						### from that, can hazard a guess at what time they should be setting according to their q times and the fuel load
						lbl$qpredsec=NA
						lbl$qpredsec[risax]=tabdat$qtouse[match(lbl$driver[risax],tabdat$driver)] + fuelcoef*lbl$fuel[risax]
						### would like an approximate sd for that too
						qdum=quantile( (lbl$sec-lbl$qpredsec)[lbl$isGood & lbl$rr==ri],pr=c(0.05,0.95),na.rm=T)
						keep=which(lbl$isGood & lbl$rr==ri & lbl$sec-lbl$qpredsec>qdum[1] & lbl$sec-lbl$qpredsec<qdum[2])
						qapproxsd=1.5*sd( (lbl$sec-lbl$qpredsec)[keep])
						
						### so, what is biggest outlier by each stint?
						
						## so here's the measure of unlikeliness of each lap time (one sided, we don't mind freakishly quick laps:
						lbl$smsecprob=1-pnorm(lbl$sec,lbl$smoothsec,approxsd)
						lbl$qsecprob=1-pnorm(lbl$sec,lbl$qpredsec,qapproxsd)
						lbl$secprob=pmin(lbl$smsecprob,lbl$qsecprob,na.rm=T)
												
						maxOutlier = lbl[saxgood,] %>%
										group_by(driver, stint) %>%
										filter(secprob == min(secprob)) %>%
										select(race, driver, stint, lap)
						lbl = indicate_overlapping_combination(
									lbl,
									maxOutlier,
									c('race', 'driver', 'stint', 'lap'),
									'ismaxoutlier')
						
						### right, now for those outliers that are less than cutoff, exclude and redo
						
						outliersax=saxgood[which( (lbl$ismaxoutlier[saxgood] & lbl$secprob[saxgood]<outliercutoff) | lbl$secprob[saxgood]<extremeoutliercutoff)]
						cat('Sweep number',sweepcount,' have got',length(outliersax),'outliers\n')
						if (length(outliersax)==0) {
							finished=T
							next
						}
						
						sweepcount=sweepcount+1
						lbl$isOutlier0[outliersax]=1
						lbl$updatedIsGood = lbl$isGood & !is.na(lbl$isOutlier0) & !lbl$isOutlier0
					}
				}
				## now add that to database
				sqlLazyUpdate(lbl[lbl$rr == ri,], 'racedriverlap', c('race','driver','lap'), 'isOutlier0')
				if (FALSE) {
				sqlUpdate_multikey('racedriverlap',
									'isOutlier0',
									c('race','driver','lap'),
									lbl[lbl$rr==ri,'isOutlier0'],
									lbl[lbl$rr==ri,c('race','driver','lap')])
				}

				### and let database know that we have done outliers for this racetodo
				raceDF[ri,'doneRaceOutlier']=TRUE
				sqlLazyUpdate(raceDF[ri,], 'race', 'race', 'doneRaceOutlier')
				if (FALSE) {
				sqlUpdate_multikey('race',
									'doneRaceOutlier',
									'race',
									raceDF[ri,'doneRaceOutlier'],
									raceDF[ri,'race'])
				}
				cat('Have updated outliers for',raceDF$race[ri],'\n')
			}
		}
	}
}

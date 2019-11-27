
### not convenient to add qualifying data into the data_to_db file yet, have to scan it all off disk for the time being. this file does that

### useful to know if entire qualifying was rained off for later stages of model

### just doing lm returns such messy coefs, with one driver set to zero etc, this functions should give nicer output

RunQualifyingOutlier = function() {

	LoadAllData()

	qdf = lazy_left_join(qdf, raceDF, 'race', 'circuit')
	qdf = lazy_left_join(qdf, rddf, c('race', 'driver'), 'team')
	qdf$driverteam = with(qdf, paste(driver, team))
	
	racetodo=raceDF %>% filter(!doneQualifyingOutlier) %>% pull(race)
	### set outliers to zero by default
	qdf$isOutlier[which(qdf$race %in% racetodo)]=FALSE

	if (length(racetodo)>0) {
		probcutoff=0.001
		raceDF$circcoef=rep(NA,nrace)
		qdf$temppredsec=rep(NA,dim(qdf)[1])
		### want to update the list for all races within year of each racetodo
		### have to take very different approach depending on whether we have one or >1 races
		eliminate=NULL
		yearToUpdate = raceDF %>% filter(race %in% racetodo) %>% distinct(year) %>% pull(year)
		for (yi in 1:length(yearToUpdate)) {
			validqtsax=which(qdf$year == yearToUpdate[yi] & !qdf$isWet)
			rrToModel=unique(qdf$rr[validqtsax])
			if (length(rrToModel)==1) {
				### this is a real pain in the arse, just have to ask the user to pick out any lap times that look out - it will only be a temporary situation, they'll all get retested after the second race
				myundriv=unique(qdf$driver[validqtsax])
				mysecarr=cbind(myundriv,array(NA,dim=c(length(myundriv),3)))
				for (k in 1:3) {
					currentSessionIsWet = with(qualifyingSessionDF, isWet[rr==rrToModel & session==k])
					if (!currentSessionIsWet) {
						sax2=which(qdf$rr==rrToModel & qdf$session==k)
						mysecarr[,k+1]=qdf$sec[sax2[match(myundriv,qdf$driver[sax2])]]
					}
				}
				satis=F
				while(!satis) {
					print(mysecarr)
					cat('Enter row names of any outlying times (-99 when finished)\n')
					userdum=askcond(T,F)
					if (userdum==-99) {
						satis=T
					}
					if (userdum!=-99) {
						cat('Which session did',myundriv[userdum],'set an outlying time?\n')
						userdum2=askcond(T,F)
						eliminate=c(eliminate,which(qdf$rr==rrToModel & qdf$session==userdum2 & qdf$driver==myundriv[userdum]))
					}
				}
			}
			if (length(rrToModel)>1) {
				qdf$lik=rep(0,dim(qdf)[1])
				overrule=NULL
				while(any(qdf$lik<probcutoff,na.rm=T)) {
					### firstly let's get hold of approximate race intercept coefs
					mod=with(qdf[validqtsax,],lm(sec~factor(circuit)-1+factor(driverteam)))
					dum=f1qualifying::getcircdrivcoef(mod)
					allcirccoef=dum$circcoef
					for (ri in rrToModel) {
						sax=which(qdf$year == yearToUpdate[yi] & !qdf$isWet & qdf$rr!=ri)
						### if only one other race, lm will give error when factoring circuits
						if (length(rrToModel)==2) {
							mod=with(qdf[sax,],lm(sec~factor(driverteam)-1))
							### now we want to normalise by the drivers who appear in the race we're predicting for
							dum=list(dcoef=coef(mod)-allcirccoef[names(allcirccoef)==raceDF$circuit[setdiff(rrToModel,ri)]])
							names(dum$dcoef)=gsub('.+\\)','',names(dum$dcoef))
						}
						if (length(rrToModel)>2) {
							mod=with(qdf[sax,],lm(sec~factor(circuit)-1+factor(driverteam)))
							dum=f1qualifying::getcircdrivcoef(mod)
							### but be careful, standard of field might have changed since the overall circuit coefs were fit - so realign driver coefs accordingly
							rdccoef=allcirccoef[match(names(dum$circcoef),names(allcirccoef))]
							adjval=mean(rdccoef-dum$circcoef)
							dum$dcoef=dum$dcoef-adjval
						}
						sax2=which(qdf$rr==ri  & !qdf$isWet)
						qdf$temppredsec[sax2]=allcirccoef[names(allcirccoef)==raceDF$circuit[ri]] + as.numeric(dum$dcoef)[match(qdf$driverteam[sax2],names(dum$dcoef))]
					}
					### now, we can estimate sigma by taking only the central 90% of the data
					qdum=quantile(qdf$sec-qdf$temppredsec,na.rm=T,pr=c(0.05,0.95))
					sax=which( (qdf$sec-qdf$temppredsec)>qdum[1] & (qdf$sec-qdf$temppredsec)<qdum[2] )
					mysd=sd(qdf$sec[sax]-qdf$temppredsec[sax])
					### can then get a probability estimate for each time based on that
					qdf$lik=dnorm(qdf$sec,qdf$temppredsec,mysd)
					qdf$lik[qdf$year!=yearToUpdate[yi]]=NA
					### don't want to keep picking up ones already oked, so temporarily delete them
					qdf$lik[overrule]=NA
					### right, now is the most unlikely below the threshold? ditch if so
					if (min(qdf$lik,na.rm=T)<probcutoff) {
						mdum=which.min(qdf$lik)
						tabdat = qdf %>%
									filter(race == qdf$race[mdum]) %>%
									select(race, driver, session, sec) %>%
									spread(key = session, value = sec, sep = '')
						rowOfBiggestOutlier=which(tabdat$driver==qdf$driver[mdum])
						sessionOfBiggestOutlier = paste0('session', qdf$session[mdum])
						tabdat[rowOfBiggestOutlier,sessionOfBiggestOutlier]=
							paste(tabdat[rowOfBiggestOutlier,sessionOfBiggestOutlier],' (',round(qdf$temppredsec[mdum],3),')',sep='')
						print(tabdat)
						cat('Probability:',min(qdf$lik,na.rm=T),'\n')
						print('Do you want to eliminate this time (y/n)?')
						satis=F
						while(!satis) {
							elimyn=askcond(F,F)
							if (elimyn %in% c('y','n')) {
								satis=T
								break
							}
							print('Invalid response, enter again')
						}
						if (elimyn=='y') {
							qdf$sec[mdum]=NA
							eliminate=c(eliminate,mdum)
						}
						if (elimyn=='n') overrule=c(overrule,mdum)
					}
				}
			}
			### now update the entire list of outlier indicators for this year, including potentially overruling previous declared outliers
			if (length(eliminate)>0) qdf$isOutlier[eliminate]=TRUE
			ysax=which(qdf$year==yearToUpdate[yi])
			sqlLazyUpdate(qdf[ysax,], 'qualifying',  c('race', 'session', 'driver'), 'isOutlier')
			rsax=with(raceDF, which(year==yearToUpdate[yi] & !doneQualifyingOutlier))
			for (ri in rsax) {
				raceDF$doneQualifyingOutlier[ri]=TRUE
				sqlLazyUpdate(raceDF[ri,], 'race', 'race', 'doneQualifyingOutlier')
			}
		}
	}
}

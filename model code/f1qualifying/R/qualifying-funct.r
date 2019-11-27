
getcircdrivcoef=function(mod, circvar='circuit', drivvar='driverteam') {

	circcoef=as.numeric(coef(mod))[grep(paste('factor\\(',circvar,'\\)',sep=''),names(coef(mod)))]
	names(circcoef)=gsub('^.+\\)','',names(coef(mod))[grep(paste('factor\\(',circvar,'\\)',sep=''),names(coef(mod)))])

	dum=grep(paste('factor\\(',drivvar,'\\)',sep=''),names(coef(mod)))
	dcoef=as.numeric(coef(mod)[dum])
	dcoefname=gsub('^.+\\)','',names(coef(mod)[dum]))
	# but have to add default driver into this
	dcoefname=c(mod$xlevels[[grep(drivvar,names(mod$xlevels))]][1],dcoefname)
	dcoef=c(0,dcoef)
	names(dcoef)=dcoefname
	
	### but we prefer mean cnetred driver coefs, so adjust them, and correct circuit coefs at same time
	meandcoef=mean(dcoef)
	
	dcoef=dcoef-meandcoef
	circcoef=circcoef+meandcoef
	
	return(list(circcoef=circcoef, dcoef=dcoef))
}

getqint=function(rrToModel, rddf) {
	
	rddf = lazy_left_join(rddf, raceDF, 'race', 'circuit')
	qsax=with(rddf,which(rr %in% rrToModel & !is.na(modQualRawDCoef) ))
	qdum=rddf[qsax,c('rr','driverTeamYear','circuit','modQualRawDCoef')]
	qdum$circuit=paste(qdum$circuit,'q')
	names(qdum)=c('rr','driverTeamYear','circuit','rawdcoef')
	mod=with(qdum,lm(rawdcoef~factor(circuit) + factor(driverTeamYear)-1))
	
	### good, now let's redo the driver coefs with the circuits put in as offsets
	qcirccoef=coef(mod)[grep('factor\\(circuit\\).+q$',names(coef(mod)))]
	names(qcirccoef)=gsub('(factor\\(circuit\\))','',names(qcirccoef))
	
	### however, first driver has been omitted and taken to be zero - so need to readjust
	dum=grep('factor\\(driverTeamYear\\)',names(coef(mod)))
	mydcoef=c(0,coef(mod)[dum])
	
	### but we want those to be zero centered, and in that case, circuit coefs have to adjust:
	
	adjqcirccoef=qcirccoef + mean(mydcoef)
	### good idea to remove the qs and rs before returning
	names(adjqcirccoef)=gsub(' q$','',names(adjqcirccoef))
	
	return(list(qcoef=adjqcirccoef))
}

MakeQualifyingIsValid = function(qdf) {
	### want to have two valid indicators, one that we use to model, which excludes possible low incentive laps, and another that predicts for those, but excludes wet sessions, outliers etc
	
	finalSessionByDriver = qdf %>%
					group_by(race, driver) %>%
					summarise(finalSessionByDriver = n()) %>%
					ungroup()
	qdf = lazy_left_join(qdf, finalSessionByDriver, c('race', 'driver'))
					
	qdf$isvalidfit=qdf$isvalidbackupfit=qdf$isvaliddesperatefit=qdf$isvalidpred=FALSE
	qdf$isvalidfit[with(qdf, which(!isWet & session == finalSessionByDriver & !is.na(sec) & !isOutlier))]=TRUE
	### except, if we're in the situation where the only lap times we've got for a driver this season are discounted due to not being finqs, then we should use non-outlier ones from their finqs-1 session as backup
	qdf$isvalidbackupfit[with(qdf, which(!isWet & session == finalSessionByDriver - 1 & !is.na(sec) & !isOutlier))]=TRUE
	### use q1 times if we're absolutely desperate
	qdf$isvaliddesperatefit[with(qdf, which(!isWet & session == finalSessionByDriver - 2 & !is.na(sec) & !isOutlier))]=TRUE
	qdf$isvalidpred[with(qdf, which(!isWet & !is.na(sec) & !isOutlier))]=TRUE

	return(qdf)
}

GetPreliminaryPredSec = function(qdf, rrToModel, myYear, wgt) {
	qdf$dcoef=qdf$circcoef=rep(NA,dim(qdf)[1]) # awfully vague names esp dcoef
	
	if (length(rrToModel)==1) {
		# our first navie guess is just the most important time that they set e.g q3 if they made it to that, q2 if they mde it to that but got eliminated, otherwise q1
		mostImportantSecByDriver = qdf %>%
									filter(rr==rrToModel & isvalidfit) %>%
									select(race, driver, sec) %>%
									rename(predSec = sec)
		qdf$isCurrentRace = (qdf$rr == rrToModel)
		qdf = subset_join(qdf, mostImportantSecByDriver, c('race', 'driver'), isCurrentRace)
		qdf = within(qdf, rm(isCurrentRace))
	}
	if (length(rrToModel)>1) {
		### get the circuit coefs firstly
		qdf$wgtval = 1
		mod=lm(sec~factor(circuit)-1+factor(driverteam),
								weights=wgtval,
								data = qdf %>% filter(isvalidfit & year == myYear))
		dum=f1qualifying::getcircdrivcoef(mod)
		allcirccoef=dum$circcoef
		for (ri in rrToModel) {
			qdf$useInPreliminaryQualiModel = f1qualifying:::SelectDriverSessionToUse(rrToModel, ri, qdf)
			validQdf = qdf %>% filter(useInPreliminaryQualiModel)
			validQdf$wgtval = exp(-wgt*abs(validQdf$rr-ri))
			mod=with(validQdf,lm(sec~factor(circuit)-1+factor(driverteam),weights=wgtval))
			dum=f1qualifying::getcircdrivcoef(mod)
			### again have to adjust the driver times for the changing field
			rdccoef=allcirccoef[match(names(dum$circcoef),names(allcirccoef))]
			adjval=mean(rdccoef-dum$circcoef)
			dum$dcoef=dum$dcoef-adjval
			sax2=which(qdf$rr==ri & qdf$isvalidpred)
			qdf$circcoef[sax2]=allcirccoef[names(allcirccoef)==raceDF$circuit[ri]]
			qdf$dcoef[sax2]=dum$dcoef[match(qdf$driverteam[sax2],names(dum$dcoef))]
			qdf$predSec[sax2]=qdf$circcoef[sax2] + qdf$dcoef[sax2]
		}
	}
	
	# predSec is only thing required from here on
	qdf = qdf %>% select(-c(dcoef, circcoef))
	return(qdf)
}
	
SimulateQualProb = function(qdf, qualifyingSessionDF, rrToModel, mysd) {

	nsim=1000
	for (ri in rrToModel) {
		qsax=which(qualifyingSessionDF$rr==ri & !qualifyingSessionDF$isWet)
		qsax12=qsax[qualifyingSessionDF$session[qsax] %in% c(1:2)]
		if (length(qsax12)>0) {
			for (qi in qsax12) {
				sax2=which(qdf$rr==qualifyingSessionDF$rr[qi] &
							qdf$session == qualifyingSessionDF$session[qi] &
							qdf$isvalidpred)
				qdf$qualProb[sax2]=apply(apply(matrix(rnorm(nsim*length(sax2),rep(qdf$predSec[sax2],nsim),mysd),ncol=nsim),2,rank)<=qualifyingSessionDF$numberOfQualifier[qi],1,mean)
			}
		}
		sax2=with(qdf, which(rr == ri & session == 3 & isvalidpred))
		qdf$qualProb[sax2]=-99
		cat('Have processed qualifying probabilities for',raceDF$race[ri],'\n')
	}
	return(qdf)
}

CalculateQualWgt = function(qdf) {
	### now let's make a vector putting less weight on qualy times that were dead certs
	qdf$qualWgt[which(qdf$qualProb>0.99)]=0
	qdf$qualWgt[which(qdf$qualProb<0.9)]=1
	sax=which(qdf$qualProb>=0.9 & qdf$qualProb<=0.99)
	mixval=(qdf$qualProb[sax]-0.9)/(0.99-0.9)
	qdf$qualWgt[sax]=(1-mixval)
	
	return(qdf)
}

CalculateRawDCoefPredNValid = function(rddf, raceDF, qdf, currentYearRr) {
	
	for (ri in currentYearRr) {
		rdsax=which(rddf$rr==ri)
		if (!raceDF$isValidQual[ri]) {
			rddf$modQualPredNValid[rdsax] = 0
		}
		if (raceDF$isValidQual[ri]) {
			sax=which(qdf$rr==ri & !qdf$isWet & qdf$qualWgt>0 & !is.na(qdf$qualWgt))
			if (length(unique(qdf$session[sax]))==1) {
				dum1=which(rddf$driver[rdsax] %in% qdf$driver[sax])
				dum2=which(!rddf$driver[rdsax] %in% qdf$driver[sax])
				mdum=sax[match(rddf$driver[rdsax[dum1]],qdf$driver[sax])]
				rddf$modQualRawDCoef[rdsax[dum1]]=qdf$sec[mdum]
				rddf$modQualPredNValid[rdsax[dum1]]=qdf$qualWgt[mdum]
				rddf$modQualPredNValid[rdsax[dum2]]=0
			}
			if (length(unique(qdf$session[sax]))>1) {
				mod=with(qdf[sax,],lm(sec~factor(driver)-1+factor(session,levels=c(3,1,2)),weight=qualWgt))
				dum=grep('factor\\(driver\\)',names(coef(mod)))
				dcoef=as.numeric(coef(mod)[dum])
				dcoefname=gsub('^.+\\)','',names(coef(mod))[dum])
				rddf$modQualRawDCoef[rdsax]=dcoef[match(rddf$driver[rdsax],dcoefname)]
				dum=with(qdf[sax,],tapply(qualWgt,driver,sum))
				dum1=which(rddf$driver[rdsax] %in% names(dum))
				dum2=which(!rddf$driver[rdsax] %in% names(dum))
				rddf$modQualPredNValid[rdsax[dum1]]=as.numeric(dum)[match(rddf$driver[rdsax[dum1]],names(dum))]
				rddf$modQualPredNValid[rdsax[dum2]]=0
			}
		}
	}
	
	return(rddf)
}
	
SelectDriverSessionToUse=function(rrToModel, ri, qdf) {
	### so the drivers we need numbers for are:
	needdriv=unique(qdf$driver[qdf$rr==ri & qdf$isvalidpred])
	### firstly try to get the ones we want from validfit
	sax0=which(qdf$rr %in% rrToModel & qdf$isvalidfit)
	### but we might find we've only e.g got one time by doing that, so in that case, grab the backup ones
	dum=table(qdf$driver[sax0])
	dum2=needdriv[!needdriv %in% names(dum)[dum>1]]
	sax1=NULL
	if (length(dum2)>0) {
		sax1=which(qdf$driver %in% dum2 & qdf$rr %in% rrToModel & qdf$isvalidbackupfit)
	}
	### repeat test - do we have everything we need? if not, take ones from q1
	dum=table(qdf$driver[c(sax0,sax1)])
	dum2=needdriv[!needdriv %in% names(dum)[dum>1]]
	sax2=NULL
	if (length(dum2)>0) {
		sax2=which(qdf$driver %in% dum2 & qdf$rr %in% rrToModel & qdf$isvaliddesperatefit)
	}
	sax=c(sax0,sax1,sax2)
	qdf$useInPreliminaryQualiModel = FALSE
	qdf$useInPreliminaryQualiModel[sax] = TRUE
	return(qdf$useInPreliminaryQualiModel)
}

GetNumberOfQualifiers = function(qualifyingSessionDF, rddf) {
	qualifyingSessionDF = left_join(qualifyingSessionDF,
									rddf %>%
									group_by(race) %>%
									summarise(numberOfStarter = n()),
									'race')
	if (!all(qualifyingSessionDF$numberOfStarter %in% c(17:24))) {
		stop('Have not catered for this number of starters in the race\n')
	}
	qualifyingSessionDF = qualifyingSessionDF %>%
							mutate(numberOfQualifier = case_when(
									session == 1 & numberOfStarter %in% c(23, 24) ~ 17,
									session == 1 & numberOfStarter %in% c(21, 22) ~ 16,
									session == 1 & numberOfStarter %in% c(19, 20) ~ 15,
									session == 1 & numberOfStarter %in% c(17, 18) ~ 14,
									session == 2 ~ 10))
	### but we also have the oddball 2016 australian and bahrain gp, which had different rules
	qualifyingSessionDF = qualifyingSessionDF %>%
							mutate_cond(race %in% c('2016australia','2016bahrain') & session == 1,
										numberOfQualifier = 15)
	qualifyingSessionDF = qualifyingSessionDF %>%
							mutate_cond(race %in% c('2016australia','2016bahrain') & session == 2,
										numberOfQualifier = 8)

	return(qualifyingSessionDF)
}
	
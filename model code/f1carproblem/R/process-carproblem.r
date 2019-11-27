### process_carproblem is a nightmare to follow/debug, let's see if we can improve it a tad

### now have to do a loopy thing to pick out worst cases
### don't want to check entire year every time, just need to update I'd say within 5 dry races

DiagPlot=function(myRace,myDriv,myupperylim=NULL,myPrevCarProblemDF=NULL,myCarProblemDF=NULL, lbl, rddf, raceDF, stintDF) {

	ri=which(raceDF$race==myRace)
	### NB can only view one driver at a time doing this
	myuntyre=unique(lbl$tyre[lbl$rr==ri & !lbl$isWet & lbl$driver==myDriv])
	myxlim=c(1,raceDF$nlap[ri])
	drsax=which(lbl$rr==ri & lbl$driver==myDriv)

	### we'll be wanting estimates of what lap times should be given qualifying and smoothed race pace, so let's create them on the fly - might be a case for having them permanently
	myylim=range(c(with(lbl,lbl$impsec[drsax][lbl$isGoodCarProblem[drsax]]),lbl$mod4PredSec[drsax],lbl$qualSmoothedRacePredSec[drsax]),na.rm=T)
	### unless we've requested to override it of course
	if (!is.null(myupperylim)) myylim[2]=myupperylim

	mynstint=length(unique(lbl$stint[drsax]))
	mystinttyre=with(lbl,tyre[drsax][match(1:mynstint,stint[drsax])])
	mylty=c(1,3,4)[match(mystinttyre,myuntyre)]

	### is the entire race sow? indicate if so
	myraceprob=with(rddf,raceprob[which(race==myRace & driver==myDriv)])
	mytitle=paste(raceDF$race[ri],myDriv,'raceprob:',round(myraceprob,4))

	plot(lbl$lap[drsax],lbl$impsec[drsax],cex=0.75,xlim=myxlim,ylim=myylim,xlab='lap',ylab='sec',main=mytitle)

	for (si in 1:mynstint) {
		subsax=drsax[which(lbl$stint[drsax]==si & !lbl$isWet[drsax] & !is.na(lbl$mod4PredSec[drsax]+lbl$qualSmoothedRacePredSec[drsax]))]
		if (length(subsax)>0) {
			lines(lbl$lap[subsax],lbl$mod4PredSec[subsax],lty=mylty[si],lwd=3)
			lines(lbl$lap[subsax],lbl$qualSmoothedRacePredSec[subsax],lty=mylty[si],col='red')
		}
	}
	### hatch in safety cars and or wet periods
	dum=as.numeric(lbl$isWet[drsax])
	if (sum(dum)>0) {
		dum2=rle(dum)
		dum2$start=c(1,cumsum(dum2$len)[-length(dum2$len)]+1)
		dum2$end=cumsum(dum2$len)
		for (k in which(dum2$val==1)) rect(dum2$start[k],myylim[1],dum2$end[k],myylim[2],density=5,col='cyan')
	}
	dum=as.numeric(lbl$isSafetyCar[drsax])
	if (sum(dum)>0) {
		dum2=rle(dum)
		dum2$start=c(1,cumsum(dum2$len)[-length(dum2$len)]+1)
		dum2$end=cumsum(dum2$len)
		for (k in which(dum2$val==1)) rect(dum2$start[k],myylim[1],dum2$end[k],myylim[2],density=7,col='red')
	}
	### nice to know when a lap time has been imputed during pit window
	saxpit=drsax[lbl$inlap[drsax] | lbl$outlap[drsax]]
	if (length(saxpit)>0) points(lbl$lap[saxpit],lbl$impsec[saxpit],cex=0.75,col='cyan')
	# points(lap[sax][outlap[sax]==1],predsec[sax][outlap[sax]==1])
	## good to know when a lap has been dropped for being blocked or overtaking - flag when this has coincided with outlier
	saxblock=drsax[lbl$EOLGap[drsax]<1.5]
	if (length(saxblock)>0) points(lbl$lap[saxblock],lbl$impsec[saxblock],cex=0.75,pch='+')
	saxDidOt=drsax[lbl$didOt[drsax]>0]
	if (length(saxDidOt)>0) text(lbl$lap[saxDidOt],pmin(lbl$impsec[saxDidOt],myylim[2]),lbl$didOt[saxDidOt],cex=0.75,col='green',pos=2,offset=0.1)
	saxGotOt=drsax[lbl$gotOt[drsax]>0]
	if (length(saxGotOt)>0) text(lbl$lap[saxGotOt],pmin(lbl$impsec[saxGotOt],myylim[2]),lbl$gotOt[saxGotOt],cex=0.75,col='green',pos=4,offset=0.1)
	## also mark any occasions driver got lapped - but flag up where this has caused outlier
	saxGotLap=drsax[lbl$gotLap[drsax]>0]
	if (length(saxGotLap)>0) text(lbl$lap[saxGotLap],pmin(lbl$impsec[saxGotLap],myylim[2]),lbl$gotLap[saxGotLap],cex=0.75,col='cyan',pos=3,offset=0.1)
	## also note occasions a driver has got a comfortable lead
	saxcomflead=drsax[!is.na(lbl$leadGap[drsax]) & lbl$leadGap[drsax]>2.5]
	if (length(saxcomflead)>0) text(lbl$lap[saxcomflead],pmin(lbl$impsec[saxcomflead],myylim[2]),round(lbl$leadGap[saxcomflead]),cex=0.75,col='purple',pos=3,offset=0.1)
	### and alert user to when an outlying time is set
	saxoutlier=drsax[lbl$isOutlier0[drsax]]
	points(lbl$lap[saxoutlier],pmin(lbl$impsec[saxoutlier],myylim[2]),cex=0.75,pch='x',col='red')
	### demarcate the pitstops
	abline(v=lbl$lap[drsax][lbl$inlap[drsax] & lbl$replaceTyre[drsax]],col='black',lty=2)
	### and indicate with a legend which stint was which

	### but has this one been looked at by us alrady? if so, display what we already had
	if (!is.null(myPrevCarProblemDF)) {
		for (j in 1:nrow(myPrevCarProblemDF)) {
			abline(v=myPrevCarProblemDF[j,c('startLap','endLap')],lty=3,lwd=2,col=c('red','green'))
		}
	}

	### now highlight where we think the issue might be
	ministintDF=stintDF %>%
				filter(race==myRace & driver==myDriv) %>%
				select(startLap,endLap,stint,
						stintProb,switchGain,optSwitchLap,switchIssue,slowIssue)

	for (j in 1:nrow(ministintDF)) {
		if (!is.na(ministintDF$switchIssue[j]) & ministintDF$switchIssue[j]) {
			#with(ministintDF[j,],lines(c(startLap,endLap),c(myylim[2],myylim[2]),col='blue',lwd=2))
			with(ministintDF[j,],text(optSwitchLap,myylim[2],round(switchGain,1),col='blue'))
			abline(v = ministintDF$optSwitchLap[j], col = 'blue', lty = 3)
		}
		if (!is.na(ministintDF$slowIssue[j]) & ministintDF$slowIssue[j]) {
			#with(ministintDF[j,],lines(c(startLap,endLap),c(myylim[2]-0.25,myylim[2]-0.25),col='grey',lwd=2))
			with(ministintDF[j,],text(0.5*(startLap+endLap),myylim[2]-0.25,round(stintProb,4),col='grey'))
		}
	}

	## display any currently declared issues
	if (!is.null(myCarProblemDF)) {
		for (j in 1:nrow(myCarProblemDF)) {
			abline(v=myCarProblemDF$startLap[j],col='red',lty=3,lwd=2)
			if (!is.na(myCarProblemDF$endLap[j])) abline(v=myCarProblemDF$endLap[j],col='green',lty=3,lwd=2)
		}
	}

	legend(myxlim[1],myylim[1],c(myuntyre,'optsm','switch issue','slow issue'),lty=c(mylty[1:length(myuntyre)],3,1,1),lwd=c(rep(1,length(myuntyre)),1,2,2),col=c(rep('black',length(myuntyre)),'red','blue','grey'),cex=0.65,yjust=0)
}

LoadExistingCarProblem = function() {
	carProblemList = NULL
	for (ri in which(raceDF$doneCarProblem)) {
		carProblemFile = MakeRaceFile(myRace = raceDF$race[ri], 'carproblem.csv')
		carProblemList[[ri]] = ReadF1Data(carProblemFile, 'carProblem')
	}
	carProblemDF = bind_rows(carProblemList)
	return(carProblemDF)
}

BolsterStintDF=function(stintDF, raceDF, rddf, lbl) {

	### we're going to include laps we'd marked as outliers before, because we're going to reevaluate what laps are outliers
	### but exclude overtaking/overtaken/lapped laps, they just dominate everything otherwise
	#lbl = lbl %>%
	#		mutate(isgoodcarprobsd = isGoodCarProblem & didOt ==0 & gotOt == 0 & gotLap == 0)

	### right, now we want to model the difference between lap time and that by driver-stint
	stintDF$dcoef=stintDF$safedcoef=rep(NA,nrow(stintDF))
	for (ri in which(raceDF$isValidRace4)) {

		sax=with(lbl, which(rr==ri & isGoodCarProblemStintBolster))
		mymod=lm(lbl$adjsec[sax]~factor(paste(lbl$driver[sax],lbl$stint[sax]))-1)
		### take away the mean as before
		mydcoef=coef(mymod)-mean(coef(mymod))
		### matching it up to stintDF...
		sax=which(stintDF$rr==ri)
		stintDF$dcoef[sax]=as.numeric(mydcoef)[match(paste(stintDF$driver,stintDF$stint)[sax],gsub('^.+\\)','',names(mydcoef)))]

		### however, in order to get the relationship between number of laps in a stint and how well we can estimate the stint, need a 'safe' version that has eliminated all the outliers and overtaking etc laps
		sax=with(lbl, which(rr==ri & isGoodCarProblemStintBolster & !isOutlier0))
		mymod=lm(lbl$adjsec[sax]~factor(paste(lbl$driver[sax],lbl$stint[sax]))-1)
		### take away the mean as before
		mydcoef=coef(mymod)-mean(coef(mymod))
		### matching it up to stintDF...
		sax=which(stintDF$rr==ri)
		stintDF$safedcoef[sax]=as.numeric(mydcoef)[match(paste(stintDF$driver,stintDF$stint)[sax],gsub('^.+\\)','',names(mydcoef)))]
	}

	### need to know number of lap times informing each stint estimate
	isGoodCount = lbl %>%
					group_by(race, driver, stint) %>%
					summarise(numisgood = sum(isGoodCarProblemStintBolster))
	stintDF = lazy_left_join(stintDF, isGoodCount, c('race', 'driver', 'stint'), 'numisgood')

	stintDF = lazy_left_join(stintDF, rddf, c('race', 'driver'), 'qualSmoothedRaceDCoef')

	return(stintDF)
}

GetStintSDCoef=function(stintDF) {

	### but we then need to get line of best fit of the standard deviation (predsec is not a good guide because smdcoef is nowhere near as good a prediction as predsec is)
	sdbynum=with(stintDF,tapply(qualSmoothedRaceDCoef-safedcoef,numisgood,sd,na.rm=T))
	ndum=as.numeric(table(stintDF$numisgood))
	### weed out the ones that are NA
	keep=which(!is.na(sdbynum) & as.numeric(names(sdbynum))>2)

	dumf=function(theta) {
		fitsd=exp(theta[1]) + exp(theta[2])*exp(-exp(theta[3])*as.numeric(names(sdbynum)))
		sqdiff=weighted.mean( (fitsd-as.numeric(sdbynum))[keep]^2,ndum[keep])
		return(sqdiff)
	}
	maxinfo=nlm2(dumf,p=c(0,0,0),iterlim=500)
	numsdcoef=exp(maxinfo$est)
	dum=numsdcoef[1] + numsdcoef[2]*exp(-numsdcoef[3]*as.numeric(names(sdbynum)))
	### let's just check that the line of best fit looks ok
	plot(1:length(sdbynum),sdbynum)
	lines(1:length(sdbynum),dum,col='red')

	return(numsdcoef)
}

FitStintSD=function(x,numsdcoef) {
	numsdcoef[1] + numsdcoef[2]*exp(-numsdcoef[3]*x)
}

GetOutlierCarProblem = function(lbl) {

	.GetOOSDelta=function(myadjsec) {
		### want to have mean of all other laps by driver in race, want to use this to pick out solo outliers
		myadjsec-(rep(sum(myadjsec),length(myadjsec))-myadjsec)/(length(myadjsec)-1)
	}

	OOSDeltaByDriverRace= lbl %>%
			filter(isGoodCarProblem) %>%
			group_by(race, driver) %>%
			mutate(OOSDelta = .GetOOSDelta(adjsec)) %>%
			ungroup()

	lbl$OOSDelta = 0
	lbl = subset_join(lbl,
						OOSDeltaByDriverRace,
						c('race', 'driver', 'lap'),
						isGoodCarProblem)

	# now we want to detect something is slow and not part of a sequence of slow laps
	.DetectSingleSlowLap = function(myOOSDelta) {
		slowFastSequence = rle(myOOSDelta > 3)
		myIsOutlierCarProblem = rep(FALSE, length(myOOSDelta))
		# don't actually understand what is happening here, it seems to be looking for the final slow lap in any sequence of slow laps. why? no idea - i think it's so that only one lap rather than all get counted, that must be useful later on
		isOutlierIndex = cumsum(slowFastSequence$lengths)[which(slowFastSequence$values)]
		myIsOutlierCarProblem[isOutlierIndex] = TRUE

		return(myIsOutlierCarProblem)
	}
	lbl = lbl %>%
			group_by(race, driver) %>%
			arrange(lap) %>%
			mutate(isOutlierCarProblem = .DetectSingleSlowLap(OOSDelta)) %>%
			ungroup()

	# simpler version:
	if (FALSE) {
	outlierDF = lbl %>%
			filter(isGoodCarProblem) %>%
			group_by(race, driver) %>%
			mutate(OOSDelta = .GetOOSDelta(adjsec),
					isOutlierCarProblem = OOSDelta > 3) %>%
			filter(isOutlierCarProblem)

	lbl = indicate_overlapping_combination(
						lbl,
						outlierDF,
						c('race', 'driver', 'lap'),
						'isOutlierCarProblem')
	}

	# does this really add that much? can't we just use the difference between sec and mod4predsec...?

	return(lbl)
}

CalcStintProb=function(stintDF, stintsdcoef) {

	### have to include number of points used to estimate as part of the sd
	stintDF$sdval=rep(0,dim(stintDF)[1])
	stintDF$sdval[stintDF$numisgood>0]=FitStintSD(stintDF$numisgood[stintDF$numisgood>0],stintsdcoef)

	### what is prob they were as slow as they were
	stintDF$stintProb=with(stintDF,round(1-pnorm(dcoef,qualSmoothedRaceDCoef,sdval),4))
	return(stintDF)
}

CalcRaceProb=function(rddf, stintsdcoef) {

	### have to include number of points used to estimate as part of the sd
	rddf$sdval=rep(0,dim(rddf)[1])
	rddf$sdval[rddf$mod4PredNValid>0]=FitStintSD(rddf$mod4PredNValid[rddf$mod4PredNValid>0],stintsdcoef)

	### what is prob they were as slow as they were
	rddf$raceprob=with(rddf,round(1-pnorm(mod4DCoef,qualSmoothedRaceDCoef,sdval),4))
	return(rddf)
}

Fit2SumProb=function(myadjsec, approxsd, rettype) {
	mylen=length(myadjsec)
	optfit=t(apply(matrix(1:mylen,nrow=1),2,function(x) rep(c(mean(myadjsec[1:x]),mean(myadjsec[(x+1):mylen])),c(x,mylen-x))))
	sumlik=apply(optfit,1,function(x) sum(log(dnorm(myadjsec,x,approxsd))))
	### what is loglik if you just fit one line?
	sumlik0=sum(log(dnorm(myadjsec,mean(myadjsec),approxsd)))
	### we want the difference between that and what you get just fitting one line
	if (rettype=='gain') retval=max(sumlik)-sumlik0
	if (rettype=='switchover') retval=which.max(sumlik)+1
	return(retval)
}

GetSwitchGain=function(stintDF, lbl, rrToModel) {
	### another thing we want is, what gain could you make by inserting a switch point for each driver? if high, then also worth looking at
	approxsd = with(lbl, sd( (sec - mod4PredSec)[isGood4]))

	switchGainAndLap = lbl %>%
						filter(race %in% raceDF$race[rrToModel]) %>%
						filter(isGoodCarProblem & !isOutlierCarProblem) %>%
						group_by(race, driver, stint) %>%
						summarise(switchGain = f1carproblem::Fit2SumProb(adjsec,
														approxsd = approxsd,
														rettype = 'gain'),
									optSwitchLapwithinstint = f1carproblem::Fit2SumProb(adjsec,
														approxsd = approxsd,
														rettype = 'switchover'))

	stintDF = lazy_left_join(stintDF,
							switchGainAndLap,
							c('race', 'driver', 'stint')) %>%
				mutate(optSwitchLap = optSwitchLapwithinstint + startLap - 1)

	return(stintDF)
}

MakeChoice=function(myPrevCarProblemDF) {
	actiondescription=NA
	if (is.null(myPrevCarProblemDF)) {
		actiondescription[1]='you want to declare the start and end of a problem'
		actiondescription[2]='you want to write off the entire race'
		actiondescription[3]='there is nothing (more) to do'
		actiondescription[4]='you want to override the y axis limits'
		actiondescription[5]='you want to reset everything for this driver/race'
		actionletter=c('s','w','n','y','r')
	}
	if (!is.null(myPrevCarProblemDF)) {
		actiondescription[1]='you are happy with the choice you have previously made'
		actiondescription[2]='you want to reset everything for this driver/race'
		actionletter=c('h','r')
	}
	print(paste('Type \'',actionletter,'\' if ',actiondescription,sep=''))
	satis=F
	while(!satis) {
		useryn=askcond(F,F)
		if (!useryn %in% actionletter) cat('Invalid entry, please try again\n')
		if (useryn %in% actionletter) satis=T
	}
	return(useryn)
}

DeclareCarProblem=function(myRace, myDriv, prevCarProblemDF, lbl, rddf, raceDF, stintDF) {
	### we will return an array of problems, one line for each interval of problem laps (almost always of length 1)
	### along with the race/driver/startLap info, we also want a description and an indication of complete confidence so that it doesn't need checking again
	### no this is not the right way to store the data - it should be a single line for each race/driver, with two columns: car problem yes/no and confident yes/no
	### then a separate table, one line for each problem, with startLap/endLap/explanation info
	### have problems previously been declared for this race/driver?
	if (!is.null(prevCarProblemDF)) sax=which( with(prevCarProblemDF,paste(race,driver)) %in% paste(myRace,myDriv))
	if (is.null(prevCarProblemDF) || length(sax)==0) {
		myPrevCarProblemDF=NULL
		sax = NULL
	}
	if (length(sax)>0) myPrevCarProblemDF=prevCarProblemDF[sax,c('startLap','endLap','explanation')]
	mymaxLap=rddf$maxLap[which(rddf$race==myRace & rddf$driver==myDriv)]
	myupperylim=NULL

	print(f1data:::ViewDriverComment(myRace, myDriv))

	probcount=0
	myCarProblemDF=NULL

	finished=F
	while(!finished) {
		f1carproblem::DiagPlot(myRace,myDriv,myupperylim=myupperylim,
						myPrevCarProblemDF=myPrevCarProblemDF,
						myCarProblemDF=myCarProblemDF,
						lbl = lbl,
						rddf = rddf,
						raceDF = raceDF,
						stintDF = stintDF)
		mychoice=MakeChoice(myPrevCarProblemDF)
		### do we need to create or augment myCarProblemDF? do so now if so
		if (mychoice %in% c('s','w')) {
			if (probcount==0) {
				myCarProblemDF=tibble(startLap=NA,endLap=NA,explanation=NA)
			}
			if (probcount>0) {
				myCarProblemDF=add_row(myCarProblemDF)
			}
			probcount=probcount+1
		}
		if (mychoice=='r') {
			myCarProblemDF=NULL
			probcount=0
		}
		### set an interval:
		if (mychoice == 's') {
			cat('Click the point of the graph where you think a problem started\n')
			dum=locator(n=1)
			myCarProblemDF$startLap[probcount]=round(dum$x)
			f1carproblem::DiagPlot(myRace, myDriv,myupperylim=myupperylim,
							myPrevCarProblemDF=myPrevCarProblemDF,
							myCarProblemDF=myCarProblemDF,
							lbl = lbl,
							rddf = rddf,
							raceDF = raceDF,
							stintDF = stintDF)
			cat('Click the point of the graph where you think this problem ended\n')
			dum=locator(n=1)
			myCarProblemDF$endLap[probcount]=pmin(round(dum$x),mymaxLap)
			f1carproblem::DiagPlot(myRace, myDriv,myupperylim=myupperylim,
							myPrevCarProblemDF=myPrevCarProblemDF,
							myCarProblemDF=myCarProblemDF,
							lbl = lbl,
							rddf = rddf,
							raceDF = raceDF,
							stintDF = stintDF)
			print('what is the explanation for this problem?')
			myCarProblemDF$explanation[probcount]=scan(what='',sep='\n',nmax=1,quiet=T)
		}
		### write off entire race:
		if (mychoice == 'w') {
			myCarProblemDF[probcount,c('startLap','endLap')]=c(1,mymaxLap)
			f1carproblem::DiagPlot(myRace, myDriv,myupperylim=myupperylim,
							myPrevCarProblemDF=myPrevCarProblemDF,
							myCarProblemDF=myCarProblemDF,
							lbl = lbl,
							rddf = rddf,
							raceDF = raceDF,
							stintDF = stintDF)
			print('what is the explanation for this problem?')
			myCarProblemDF$explanation[probcount]=scan(what='',sep='\n',nmax=1,quiet=T)
			finished=T
		}
		### override y-limits
		if (mychoice == 'y') {
			cat('What do you want the upper ylimit to be?\n')
			myupperylim=askcond(T,F)
		}
		### reset everything
		if (mychoice=='r') {
			if (!is.null(myPrevCarProblemDF)) myPrevCarProblemDF=NULL
		}
		### declare no (more) problems:
		if (mychoice == 'n') {
			finished=T
		}
		### you're happy with what was previously done
		if (mychoice == 'h') {
			myCarProblemDF=myPrevCarProblemDF
			finished=T
		}
	}
	### finally, ask user if they are so sure about this that no more checking is required of this race/driver
	cat('Are you confident in these choices, so that no further checking is needed (y/n)?\n')
	satis=F
	while(!satis) {
		dum=askcond(F,F)
		if (dum %in% c('y','n')) satis=T
		if (!dum %in% c('y','n')) {
			print('Invalid entry, please try again')
		}
	}
	if (dum=='y') confident=1
	if (dum=='n') confident=0
	return(list(myCarProblemDF=myCarProblemDF, confident=confident))
}

ProcessCarProblem = function() {

	LoadAllData(omitPreDelta = FALSE) # intermediate ones are needed for

	carProblemDF = f1carproblem::LoadExistingCarProblem()
	prevCarProblemDF = carProblemDF

	lbl = f1gaptrafficpitstop::AdjustLapTimeForPitStop(lbl)
	lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE)
	lbl = f1gaptrafficpitstop::GetLeadGap(lbl)

	lbl = f1gaptrafficpitstop::AlignPitStopData(lbl)

	### firstly find out which ones we haven't done - this is a lttle different to normal, because we might well want to change things in retrospect, so we'll take all races in the current season
	### NB want to use qualifying data as well to help us here
	### except to get a link between number of observations and standard error, we need to resmooth from the start - we need to redo this in the light of new races, so will redo the whole lot each time. if runtime was a problem we'd find a way around this, but YAGNI
	### this bit of code is adapted from modelphase22.r
	### downweight races based on time and lack of observations

	### get hold of some smoothed values
	rddf = MixQualifyingRaceSmoothDCoef(4, rddf, fwbw='fwbw')

	lbl$qualSmoothedRacePredSec =
			f1laptimelm::MakePredSecFromNormalisedDCoef(lbl, rddf, 4, 'qualSmoothedRaceDCoef')

	rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, 4, 'race')
	lbl = f1laptimelm::MakePredSec(lbl, 4)

	### we're going to use our fuel and tyre coefficients to get stint-specific driver coefs now

	lbl = f1laptimelm::CalculateFuelTyreEffect(lbl, 4) %>%
			mutate(adjsec = impsec - fuelTyreEffect)
	# adjsec should be called 'fuelTyreCorrectedSec' i think

	lbl = lazy_left_join(lbl, rddf,	c('race', 'driver'), 'mod4PredNValid')
	lbl$driverHasDCoef = (lbl$mod4PredNValid > 0)
	if (FALSE) {
	lbl = f1validity::MakeIsRogue(lbl, inlapOutlapAreRogue = FALSE)
	}
	# just a fudge to make sure it agrees with old world while checking, let's say short stints are good now - but they shouldn't be, delete when you're happy everything else is ok
	if (TRUE) {
	lbl = f1data:::DetectWetTyreOnDryTrack(lbl)

	lbl$isRogue = with(lbl, isSafetyCar | isWet | isRed | lap == 1 | isSCRestart |
							isWetTyreOnDryTrack)

	lbl = lazy_left_join(lbl, raceDF, 'race', 'isValidRace4')
	lbl$isGoodCarProblem = with(lbl, !isRogue & isValidRace4)
	lbl$isGoodCarProblemStintBolster = with(lbl, !isRogue & isValidRace4 &
												didOt == 0 & gotOt ==0 & gotLap == 0 &
												driverHasDCoef)
	}
	lbl = lazy_left_join(lbl, raceDF, 'race', 'isValidRace4')
	lbl$isGoodCarProblem = with(lbl, !isRogue & isValidRace4 & driverHasDCoef)
	lbl$isGoodCarProblemStintBolster = with(lbl, !isRogue & isValidRace4 &
												didOt == 0 & gotOt ==0 & gotLap == 0 &
												driverHasDCoef)

	lbl = lbl %>%
			mutate(isGoodPlot = !isRed & !isSafetyCar)

	stintDF=f1carproblem::BolsterStintDF(stintDF, raceDF, rddf, lbl)

	stintsdcoef=f1carproblem::GetStintSDCoef(stintDF)

	rrToModel=which(raceDF$year==max(unYear) & raceDF$isValidRace4)
	rrToUpdate=which(raceDF$year==max(unYear)) # necessary, because we need to fill in FALSE for all cars in the case of an invalid race
	
	### now use those to produce a 'stint probability', factoring in number of observations
	stintDF=f1carproblem::CalcStintProb(stintDF, stintsdcoef)
	### but equally, want to know entire race probability in case whole race has been driven with damaged car
	rddf=f1carproblem::CalcRaceProb(rddf, stintsdcoef)

	### get genuine outliers i.e. not part of a sequence of slow laps
	lbl=f1carproblem::GetOutlierCarProblem(lbl)

	### another thing we want is how much we gain by allowing 2 parameters for each stint
	stintDF = f1carproblem::GetSwitchGain(stintDF, lbl, rrToModel)

	### right, now let's indicate possible issues
	### possible issues are:
	### big gain in switchGain
	### low probability of stint
	### low probability of entire race
	### not previous recorded as nocarproblem
	### racetocheck is 1

	stintDF$switchIssue=stintDF$switchGain>10
	stintDF$slowIssue=stintDF$stintProb<0.1

	rddf = indicate_overlapping_combination(rddf,
											stintDF %>%
												filter(switchIssue),
											c('race', 'driver'),
											'switchIssue')
	rddf = indicate_overlapping_combination(rddf,
											stintDF %>%
												filter(slowIssue),
											c('race', 'driver'),
											'slowIssue')


	rddf$tocheck = with(rddf, rr %in% rrToModel &
							!confidentCarProblem &
							(switchIssue | slowIssue | (!is.na(raceprob) & raceprob < 0.1)))

	# now loop over them, display issues
	newcarproblist=NULL
	problemcount=0
	if (sum(rddf$tocheck) > 0) {
  	for (si in which(rddf$tocheck)) {
  		checkinfo=f1carproblem::DeclareCarProblem(rddf$race[si], rddf$driver[si],
  										prevCarProblemDF = prevCarProblemDF,
  										lbl = lbl,
  										rddf = rddf,
  										raceDF = raceDF,
  										stintDF = stintDF)
  		if (!is.null(checkinfo$myCarProblemDF)) {
  			problemcount = problemcount+1
  			newcarproblist[[problemcount]] = checkinfo$myCarProblemDF
  			newcarproblist[[problemcount]]$race=rddf$race[si]
  			newcarproblist[[problemcount]]$driver=rddf$driver[si]
  		}
  		rddf$anyCarProblem[si]=as.numeric(!is.null(checkinfo$myCarProblemDF))
  		rddf$confidentCarProblem[si]=checkinfo$confident
  	}
	}

	if (problemcount == 0) {
		newCarProblemDF = tibble(startLap = integer(),
									endLap = integer(),
									explanation = character(),
									race = character(),
									driver = character())
	}
	if (problemcount>0) {
		### now combine all new carproblem info into one single array
		newCarProblemDF=do.call(rbind,newcarproblist)
	}
	carProblemDF = bind_rows(carProblemDF, newCarProblemDF)

	rrToModify = unique(c(with(rddf, rr[tocheck]), with(raceDF, rr[!doneCarProblem])))
	for (ri in rrToModify) {
		carProblemFile = MakeRaceFile(myRace = raceDF$race[ri], 'carproblem.csv')
		write_csv(carProblemDF %>% filter(race == raceDF$race[ri]),
						path = carProblemFile)
	}

	sax=which(rddf$tocheck)
	if (length(sax) > 0) {
		sqlLazyUpdate(rddf[sax,], 'racedriver', c('race', 'driver'), c('anyCarProblem','confidentCarProblem'))
		if (FALSE) {
		sqlUpdate_multikey('racedriver',
							c('anyCarProblem','confidentCarProblem'),
							c('race', 'driver'),
							rddf[sax,c('anyCarProblem','confidentCarProblem')],
							rddf[sax,c('race', 'driver')])
		}
	}

	# then repopulate carproblem - this could maybe be more efficient, but easy to mess up
	lblRrToModelIndex = with(lbl, which(rr %in% rrToUpdate))
	lbl$isCarProblem[lblRrToModelIndex] = FALSE #
	for (ci in 1:nrow(carProblemDF)) {
		lbl = lbl %>%
				mutate_cond(driver == carProblemDF$driver[ci] &
							race == carProblemDF$race[ci] &
							between(lap,
									carProblemDF$startLap[ci],
									carProblemDF$endLap[ci]),
							isCarProblem = TRUE)
	}
	for (ri in rrToUpdate) {
		lblCurrentRrIndex = with(lbl, which(rr == ri))
		sqlLazyUpdate(lbl[lblCurrentRrIndex,], 'racedriverlap', c('race', 'driver', 'lap'), 'isCarProblem')
		if (FALSE) {
		sqlUpdate_multikey('racedriverlap',
						'isCarProblem',
						c('race', 'driver', 'lap'),
						lbl[lblCurrentRrIndex, 'isCarProblem'],
						lbl[lblCurrentRrIndex, c('race', 'driver', 'lap')])
		}
		message('Have overwritten carproblem for ', raceDF$race[ri])
	}

	raceDF[rrToModel,'doneCarProblem']=TRUE
	sqlLazyUpdate(raceDF[rrToModel,], 'race', 'race', 'doneCarProblem')
}

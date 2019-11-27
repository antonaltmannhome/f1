
### and don't worry about prioring tyres coefs to be similar to each other, the phase1/phase2 approach seems to be a simpler way to do that
### does it make sense to optimise priors on phase1, store them, then do all of phae 2? much easier to optimise I'd have thought

dum = lblupdate.preparedcoefmix(30)
rddf = dum$rddf

### so we need one vector for 'how many laps behind safety car in this race until this point, which we will use for fuel
### and another for 'how many laps beind the safety car so far in this stint'
dum=with(lbl,tapply(isscar,paste(rr,driver),cumsum))
dum2=with(lbl,tapply( lap,paste(rr,driver),function(x) x))
ndum=rep(names(dum),sapply(dum,length))
ndum2=paste(ndum,unlist(dum2))
lbl$cssafetycar=as.numeric(unlist(dum))[match(with(lbl,paste(rr,driver,lap)),ndum2)]

dum=with(lbl,tapply(isscar,paste(rr,driver,stint),cumsum))
dum2=with(lbl,tapply( lap,paste(rr,driver,stint),function(x) x))
ndum=rep(names(dum),sapply(dum,length))
ndum2=paste(ndum,unlist(dum2))
lbl$cssafetycarstint=as.numeric(unlist(dum))[match(with(lbl,paste(rr,driver,stint,lap)),ndum2)]

### values it comes up with are basically 1 for tyres, and -0.64 for fuel
lbl$fuel2=lbl$fuel+0.64*lbl$cssafetycar
lbl$tlap2=lbl$tlap-lbl$cssafetycarstint

lbl[,'isvalid']=with(lbl,(isgood30==4 & carproblem==0 & inlap+outlap==0))
validrace=intersect(with(lbl,unique(rr[(isvalid==1)])),
					with(racedb, rr[isvalidrace30==1]))

sax1013=with(racedb,which(yr<=2013))
sax14plus=with(racedb,which(yr>=2014))
mod1013=lm(racedb[sax1013,'mod30fuelcoef']~racedb[sax1013,'perim'])
mod14plus=lm(racedb[sax14plus,'mod30fuelcoef']~racedb[sax14plus,'perim'])
racedb[,'fuelprior']=rep(NA,nrace)
racedb[sax1013,'fuelprior']=predict(mod1013,data.frame(racedb[sax1013,'perim']))
racedb[sax14plus,'fuelprior']=predict(mod14plus,data.frame(racedb[sax14plus,'perim']))

### what should these be? need to do some cross validation to find out

### let's have tyres in order of softness
untyre=c('ultrasoft','supersoft','soft','medium','hard')
untyreprior=0.2*(1:length(untyre)) - mean(0.2*(1:length(untyre)))
### but then want to rejig that by race to mean centre
dumf=function(myrr) {
	myuntyre=with(lbl,unique(tyre[rr==myrr & isgood30==4]))
	dum=rep(NA,length(untyre))
	dum[match(myuntyre,untyre)]=untyreprior[match(myuntyre,untyre)]
	dum=round(dum-mean(dum,na.rm=T),2)
	return(dum)
}
tyrepriorarr=cbind(1:nrace,as.data.frame(t(sapply(1:nrace,dumf))))
names(tyrepriorarr)=c('rr',untyre)

lbl = lbl %>%
		mutate(insample = sample(1:2, n(), replace = TRUE))
		
allValidRace = racedb$racename[validrace]

GetDriverPriorDF = function(phase1Coef, mod30PriorDF) {
	phase1PriorDF = phase1Coef %>%
					filter(coefType == 'driver') %>%
					select(coefname, coefvalue) %>%
					dplyr::rename(driver = coefname)
	combinedPriorDF = left_join(phase1PriorDF, mod30PriorDF, by = 'driver')
	interceptAdjustment = with(combinedPriorDF, mean(coefvalue - mod30mixqrsmdcoef))
	mod30PriorDF = mod30PriorDF %>%
					mutate(prior = mod30mixqrsmdcoef + interceptAdjustment)
	# what if a driver is new? have to prick about with finding out who their teammate is, ugh
	# think we've done that though in teamprior
	# no, ignore all that, all has been done in mixqrsmdcoef, hooray former me
	driverPriorDF = left_join(phase1PriorDF,
							mod30PriorDF,
							by = 'driver')
	# however, in the case where a driver has been omitted from the sample, helpful to have a list where they're in it
	return(list(driverPriorDF = driverPriorDF, mod30PriorDF = mod30PriorDF))
}

priorlm = function(myLbl, mod30PriorDF, priorValue,  priorScale, phase12) {
	
	myundriv=unique(myLbl$driver)
	myuntyre=unique(myLbl$tyre)
	
	# first need to fudge the driver priors
	
	xmat.intercept=matrix(1, nrow = nrow(myLbl))
	xmat.driv=t(sapply(myLbl$driver, function(x) {myundriv==x}))
	xmat.tyre=t(sapply(myLbl$tyre, function(x) {myuntyre==x}))
	# but sapply is a dick when there's only one column...
	if (nrow(xmat.tyre) == 1) xmat.tyre = t(xmat.tyre)
	xmat.tlap = xmat.tyre[,1,drop=F] * myLbl$tlap2
	if (length(myuntyre) > 1) {
		for (j in 2:length(myuntyre)) {
			xmat.tlap=cbind(xmat.tlap, xmat.tyre[,j,drop=F]*myLbl$tlap2)
		}
	}
	xmat.fuel=myLbl$fuel2
	rownames(xmat.intercept)=NULL
	rownames(xmat.driv)=NULL
	rownames(xmat.tyre)=NULL
	rownames(xmat.tlap)=NULL
	rownames(xmat.fuel)=NULL
	
	xmat=as.matrix(cbind(xmat.intercept,xmat.driv, xmat.tyre, xmat.tlap, xmat.fuel))
	yvec=myLbl$sec
	
	### but we then get hold of priors for this race. drivers:
	interceptprior=-999
	driverprior = with(mod30PriorDF, mod30mixqrsmdcoef[match(myundriv, driver)])
	tyreprior=rep(priorValue$tyre, ncol(xmat.tyre))
	tlapprior=rep(priorValue$tlap, ncol(xmat.tyre))
	fuelprior=priorValue$fuel
	
	prior=c(interceptprior, driverprior, tyreprior, tlapprior, fuelprior)

	precisionmat=matrix(0,nrow=length(prior), ncol=length(prior))
	drivix=2:(length(myundriv)+1)
	tyreix=(length(myundriv)+2):(length(myundriv)+1+length(myuntyre))
	tlapix=(length(myundriv)+length(myuntyre)+2):(length(myundriv)+1+2*length(myuntyre))
	fuelix=length(myundriv)+2+2*length(myuntyre)
	diag(precisionmat)[drivix]=priorScale$driver
	diag(precisionmat)[tyreix]=priorScale$tyre
	diag(precisionmat)[tlapix]=priorScale$tlap
	diag(precisionmat)[fuelix]=priorScale$fuel
	### same prior on everything - that should work to start with
	coefType = rep(NA, length(prior))
	coefType[1] = 'intercept'
	coefType[drivix] = 'driver'
	coefType[tyreix] = 'tyre'
	coefType[tlapix] = 'tlap'
	coefType[fuelix] = 'fuel'
	
	betavec = solve(t(xmat) %*% xmat + precisionmat) %*%
	(precisionmat%*%prior + t(xmat)%*%yvec)
	
	coefdf=tibble(coefname=c('intercept', myundriv, myuntyre, myuntyre, 'fuel'),
					coefType = coefType,
					prior=prior,
					coefvalue=as.vector(betavec))
	return(coefdf)
}

### step 1, get driver intercept and the tlap prior

TidyMleDF = function(myMleDF, mod30PriorDF) {

	# now extract the coefs into nice lists
	interceptCoef = with(myMleDF, coefvalue[coefType == 'intercept'])
	driverCoefDF = left_join(mod30PriorDF,
								myMleDF %>%
								filter(coefType == 'driver') %>%
								select(coefname, coefvalue) %>%
								dplyr::rename(driver = coefname),
								by = 'driver') %>%
					mutate(driverCoef = ifelse(!is.na(coefvalue), coefvalue, mod30mixqrsmdcoef)) %>%
					select(driver, driverCoef)
	
	tyreCoefDF = myMleDF %>%
					filter(coefType == 'tyre') %>%
					dplyr::rename(tyre = coefname, tyreCoef = coefvalue) %>%
					select(tyre, tyreCoef)
	tlapCoefDF = myMleDF %>%
					filter(coefType == 'tlap') %>%
					dplyr::rename(tyre = coefname, tlapCoef = coefvalue) %>%
					select(tyre, tlapCoef)
	fuelCoef = with(myMleDF, coefvalue[coefType == 'fuel'])
	
	return(list(intercept = interceptCoef,
				driver = driverCoefDF,
				fuel = fuelCoef,
				tyre = tyreCoefDF,
				tlap = tlapCoefDF))
}

JoinLblToCoef = function(thisRaceLbl, myMleList, samplei) {

	thisRaceLbl$interceptCoef[which(thisRaceLbl$insample != samplei)] = myMleList$intercept
	thisRaceLbl = subset_join(thisRaceLbl,
								myMleList$driver,
								by = 'driver',
								subsetBool = thisRaceLbl$insample != samplei)
	thisRaceLbl = subset_join(thisRaceLbl,
								myMleList$tyre,
								by = 'tyre',
								subsetBool = thisRaceLbl$insample != samplei)
	thisRaceLbl = subset_join(thisRaceLbl,
								myMleList$tlap,
								by = 'tyre',
								subsetBool = thisRaceLbl$insample != samplei)
	thisRaceLbl$fuelCoef[which(thisRaceLbl$insample != samplei)] = myMleList$fuel
	return(thisRaceLbl)
}

FitSingleRace = function(myRacename, priorScale, phase12, crossValidStatus) {

	mod30PriorDF = rddf %>% 
					filter(racename==myRacename & !is.na(startinggrid)) %>%
					select(driver, mod30mixqrsmdcoef)
	thisRaceLbl = lbl %>%
					filter(racename == myRacename & isvalid) %>%
					select(driver, fuel2, tyre, tlap2, sec, insample) %>%
					mutate(interceptCoef = NA,
							driverCoef = NA,
							tyreCoef = NA,
							tlapCoef = NA,
							fuelCoef = NA)
	if (phase12 == 1) {
		thisRaceLbl$tyre = 'combinedTyre'
	}
					
	myFuelPrior = with(racedb, fuelprior[racename == myRacename])
	myTyrePrior = 0
	if (phase12 == 1) {
		myTlapPrior = 0.1
	}
	if (phase12 == 2) {
		myTlapPrior = with(racedb, fuelprior[racename == myRacename])
	}
	priorValue = list(fuel = myFuelPrior, tyre = myTyrePrior, tlap = myTlapPrior)
	
	if (crossValidStatus == 'cv') {
		for (samplei in 1:2) {
			myLbl = thisRaceLbl %>%
					filter(insample == samplei)
					
			myMleDF = priorlm(myLbl, mod30PriorDF, priorValue, priorScale, phase12)
			myMleList = TidyMleDF(myMleDF, mod30PriorDF)
		
			# match up those coefs to half they need to predict:
			thisRaceLbl = JoinLblToCoef(thisRaceLbl, myMleList, samplei)
		}
				
		thisRaceLbl = thisRaceLbl %>%
					mutate(predSec = interceptCoef + driverCoef + fuelCoef * fuel2 + tyreCoef + tlapCoef * tlap2)
					
		thisRaceLbl$sqDiff = with(thisRaceLbl, (predSec - sec)^2)
		meanSqDiff = mean(thisRaceLbl$sqDiff)
		toReturn = meanSqDiff
	}

	if (crossValidStatus == 'no') {
		myLbl = thisRaceLbl
					
		myMleDF = priorlm(myLbl, mod30PriorDF, priorValue, priorScale, phase12)
		myMleList = TidyMleDF(myMleDF, mod30PriorDF)

		toReturn = myMleList
	}
	
	return(toReturn)
}

# ok, that's really fast and sensible looking now, just have to integrate it into the cross val setup
# but before doing that, let's see if nlm behaves itself
# no, we have got to do all of that crap sadly

### process:
# 1. Find out optimal values for phase 1 driver prior and phase 1 fuel prior.
# 2. Using those values, run the model for all races but not cross validating. This should yield tlap coefficient.
# 3. With those set up, and aligned with rddf, repeat step 1, find optimal strength for all four prior strengths
# 4. Using those values, run the model for all races but not cross validating
# 5. That will give you all the fuel, tyre and tlap parameters. Then with those as an offset, get the driver estimates for each race

FitAllRace = function(theta, phase12) {
	if (phase12 == 1) {
		priorScale = list(driver = exp(theta[1]), tyre = 0.1, tlap = 0.1, fuel = exp(theta[2]))
	}
	if (phase12 == 2) {
		priorScale = list(driver = exp(theta[1]), tyre = exp(theta[2]), tlap = exp(theta[3]), fuel = exp(theta[4]))
	}
		
	print('Prior scale:')
	print(paste(priorScale, collapse = ' '))

	if (any(exp(theta) > 10E7)) return(10E6)
	
	allRaceMeanSqDiff = rep(NA, length(allValidRace))
	for (j in 1:length(allValidRace)) {
		allRaceMeanSqDiff[j] = FitSingleRace(allValidRace[j], priorScale, phase12, crossValidStatus = 'cv')
	}

	meanMeanSqDiff = mean(allRaceMeanSqDiff)
	cat('Squared diff:', meanMeanSqDiff, '\n')
	return(meanMeanSqDiff)
}

maxInfo1 = nlm(FitAllRace, p = c(log(10),log(100)), phase12 = 1)
#> maxInfo1
#$minimum
#[1] 0.2460666
#$estimate
#[1] 0.09179143 8.18710708

priorScale = list(driver = exp(maxInfo1$est[1]), tyre = 0.1, tlap = 0.1, fuel = exp(maxInfo1$est[2]))

racedb$tlapPrior = NA
for (j in 1:length(allValidRace)) {
	dum = FitSingleRace(allValidRace[j], priorScale, 1, crossValidStatus = 'no')
	racedb$tlapPrior[which(racedb$racename == allValidRace[j])] = dum$tlap$tlapCoef
}

maxInfo2 = nlm(FitAllRace, p = c(log(10),log(10),log(10),log(100)), phase12 = 2)
#$minimum
#[1] 0.2123667
#$estimate
#[1] -0.09177337  0.10106972 -3.72717243  7.80146707

# seems like the priors make almost no difference - although maybe that's because we've forced the tyres to be the same rather than expecting softs to wear quicker than hard. ie giving it wrong priors is always likely to lower how much it wants them

## also, let's see what priors it wants on races that actually would need it, compare to ones that don't - what does it do when you combine them, does it do whatever the sensitive one does?

# we badly need some nice plots to help us out here. let's make a tidier setup of all of this that is easier to work with

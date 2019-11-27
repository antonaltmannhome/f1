
racetodo=which(racedb[,'donewetrace']==0)

if (length(racetodo)==0) {
	stop('All races have had safety cars processed, nothing to do\n')
}


plotsec=function(sax, ovylim, wetinfo, miscinfo) {
	plot(myRaceDf$temptelapse[sax],myRaceDf$sec[sax],ylim=c(ovylim[1],ovylim[2]),xlab='temptelapse',ylab='lap time',main=racedb$racename[ri])
	### highlight in and out laps
	points(myRaceDf$temptelapse[sax][myRaceDf$inlap[sax]==1],myRaceDf$sec[sax][myRaceDf$inlap[sax]==1],col='green')
	points(myRaceDf$temptelapse[sax][myRaceDf$outlap[sax]==1],myRaceDf$sec[sax][myRaceDf$outlap[sax]==1],col='yellow')
	### highlight any wet or intermediate tyre usage
	points(myRaceDf$temptelapse[sax][myRaceDf$tyre[sax] == 'wet'],myRaceDf$sec[sax][myRaceDf$tyre[sax] == 'wet'],col='purple')
	points(myRaceDf$temptelapse[sax][myRaceDf$tyre[sax] == 'intermediate'],myRaceDf$sec[sax][myRaceDf$tyre[sax] == 'intermediate'],col='magenta')
	### highlight leader at each stage
	points(myRaceDf$temptelapse[miscinfo$ixlead],myRaceDf$sec[miscinfo$ixlead],col='cyan',pch='+',cex=0.75)
	### indicate where the qualifying intercept was - add 3 seconds to qualifying seems to be a good guide
	if (!is.na(miscinfo$modqualqual)) abline(h=miscinfo$modqualqual+3,lty=3,col='blue')
	### overlay line of fitted intercept for each lap
	for (j in 1:miscinfo$nlap) {
		lines(with(myRaceDf,c(min(temptelapse[templeadlap==j]),max(temptelapse[templeadlap==j]))),c(miscinfo$lapcoef[j],miscinfo$lapcoef[j]),col='red',lwd=2)
	}
	### and draw in lines that we already have for wetness
	if (wetinfo$wetstartcount>0) {
		for (j in 1:wetinfo$wetstartcount) {
			abline(v=wetinfo$wetstartvec[j],col='cyan',lty=3)
		}
	}
	if (wetinfo$wetendcount>0) {
		for (j in 1:wetinfo$wetendcount) {
			abline(v=wetinfo$wetendvec[j],col='cyan')
		}
	}
}

#racedb=sqlQuery2('select * from race')

for (ri in racetodo) {
	
	miscinfo=NULL
	miscinfo$nlap=racedb[ri,'nlap']

	myRaceDf=lbl[lbl$rr==ri,]
	### what will be very useful is the qualifying intercept so let's tack it on
	miscinfo$modqualqual=racedb[ri,'modqualqualintercept']
	
	### get rid of any massive outlier times, they're so annoying
	dum=with(myRaceDf,tapply(sec[inlap+outlap==0],lap[inlap+outlap==0],median))
	dum2=as.numeric(dum)[match(myRaceDf$lap,names(dum))]
	sax=with(myRaceDf,which((sec>dum2*1.1 & inlap+outlap==0) | (inlap+outlap>0 & sec>dum2*1.5)))
	myRaceDf[sax,'sec']=NA
	
	### then supply a lm curve fitting intercept for each lap - eliminate the slowest three drivers to get rid of outliers though
	myRaceDf$isvalid=rep(0,dim(myRaceDf)[1])
	for (j in 1:racedb[ri,'nlap']) {
		sax=with(myRaceDf,which(lap==j & inlap+outlap==0 & !is.na(sec)))
		rdum=rank(myRaceDf[sax,'sec'])
		myRaceDf[sax,'isvalid']=as.numeric(rdum<=length(rdum)-3)
	}
	mod=with(myRaceDf[myRaceDf$isvalid==1,],lm(sec~factor(driver)+factor(lap)-1))
	dum=coef(mod)[grep('factor\\(lap\\)',names(coef(mod)))]
	miscinfo$lapcoef=rep(NA,racedb[ri,'nlap'])
	miscinfo$lapcoef[match(gsub('(^.+\\))([0-9]+$)','\\2',names(dum)),1:racedb[ri,'nlap'])]=as.numeric(dum)
	### except we want it to represent the average for driers so it overlays nicely on the plot, so let's add that on
	miscinfo$lapcoef=miscinfo$lapcoef + mean(coef(mod)[grep('factor\\(driver\\)',names(coef(mod)))])
	
	miscinfo$ixlead=which(myRaceDf$tempisleader==1)
	
	### check to see if there is already safety car info for this race
	alldone=F
	ovylim=range(c(miscinfo$modqualqual+3,myRaceDf$sec),na.rm=T)
	wetinfo=list(wetstartvec=rep(NA,20),wetendvec=rep(NA,20),wetstartcount=0,wetendcount=0)
	wetinfofile=makeracefile(myrr=ri,myfilename='wetinfo.dat')
	while(!alldone) {
		
		plotsec(1:dim(myRaceDf)[1],ovylim=ovylim,wetinfo=wetinfo,miscinfo=miscinfo)
		### now offer user choice of actions
		cat("Type 's' to identify start of rain\n")
		cat("Type 'e' to identify end of rain\n")
		cat("Type 'y' to override the y limits\n")
		cat("Type 'r' to reset any choices you have made so far\n")
		cat("Type 'x' when there is nothing more to do\n")
		
		### click on a area that seems to correspond to safety car coming out
		
		uresp=scan(what='',nmax=1,quiet=T)
		
		if (!uresp %in% c('s','e','y','r','x')) {
			print('invalid response, try again')
			next
		}
		
		if (uresp=='y') {
			cat('Please enter new upper y limit:\n')
			ovylim[2]=as.numeric(scan(what='',nmax=1,quiet=T))
			next
		}
		
		if (uresp %in% c('s','e')) {
			
			inout=c('starts','ends')[match(uresp,c('s','e'))]
			onedone=F
			ovxwid=1500
			cat('OK, click on where you think the rain',inout,'\n')

			dum=locator(n=1)
			if (inout=='ends') {
				wetinfo$wetendcount=wetinfo$wetendcount+1
				wetinfo$wetendvec[wetinfo$wetendcount]=dum$x
			}
			if (inout=='starts') {
				wetinfo$wetstartcount=wetinfo$wetstartcount+1
				wetinfo$wetstartvec[wetinfo$wetstartcount]=dum$x
			}
			onedone=T
			next
		}
		
		if (uresp=='r') {
			wetinfo=list(wetstartvec=rep(NA,20),wetendvec=rep(NA,20),wetstartcount=0,wetendcount=0)
			next
		}
		if (uresp=='x') {
			if (wetinfo$wetstartcount!=wetinfo$wetendcount) {
				print('hold on there, you need to have the same number of start and end periods')
				alldone=F
				next
			}
			if (wetinfo$wetstartcount==wetinfo$wetendcount) {
				print('ok, all done for this race')
				alldone=T
				next
			}
		}
	}
	
	### and save to disk in case you need to rewrite the database at any point
	dput(file=wetinfofile,wetinfo)
}

### NB you might have already done all the safety cars and are just resetting the database, if so, just set racetodo to be 1:nrace here

for (ri in racetodo) {

	wetinfofile=makeracefile(myrr=ri,myfilename='wetinfo.dat')
	wetinfo=dget(wetinfofile)
	
	### now loop through the periods that have been declared wet
	lbl$iswet[lbl$rr==ri]=0
	if (wetinfo$wetstartcount>0) {
		for (k in 1:length(wetinfo$wetstartvec)) {
			sax=which(lbl$rr==ri & lbl$temptelapse>wetinfo$wetstartvec[k] & lbl$temptelapse<wetinfo$wetendvec[k])
			lbl$iswet[sax]=1
		}
	}
	
	### finally update the database
	sqlUpdate('lapbylap','iswet','rdlkey',lbl[which(rr==ri),'iswet'],lbl$rdlkey[which(rr==ri)])
	### and let database know that we have done outliers for this racetoadd
	racedb[ri,'donewetrace']=1
	sqlUpdate('race','donewetrace','sqlrr',racedb[ri,'donewetrace'],racedb[ri,'sqlrr'])
}

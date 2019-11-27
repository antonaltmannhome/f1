# no this is too fiddly. just take team mates pace or pace at end of previous year. hard code the new teams

maketeamprior=function(modelchoice,myfwbw='bw') {
	### if it's the start of a season, or a new driver joins the field but misses qualifying, we've got no data to start simulating race runs from - this uses the most recent possible non-cheaty team estimate
	### note that qualifying data for the race is considered cheaty, but race simulation code is clever enough to know when to use that
	### you would only use the myfwbw='fwbw' option if you want to do the team seasonal plots over time
	dum = f1smoothing:::GetSmooth(qrToFit = 'qr',
                               qrToPredict = 'r',
                              useStretch = TRUE,
                              modelChoice=30,
                              fwbw = 'bw')
	rddf = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'smoothDCoef')

	### need to make data frame of priors for each team based on previous year
	### but it's not necessarily just a case of using whatever was there in previous year - there might be a new team, or an old team might have disappeared

	### need to just put in manual priors for start of 2010 season - it's really not very important at all
	prior2009=read.csv('data/team_prior_2009.csv',sep='~')
	### except, we need to recalibrate these because we've got three new slow teams
	prior2010=data.frame(aateam=unique(rddf$aateam[rddf$yr==2010]),coef=NA)
	### any new teams? set them to 2.5 seconds slow
	newsax=which(!prior2010$aateam %in% prior2009$team)
	oldsax=which(prior2010$aateam %in% prior2009$team)
	prior2010$coef[newsax]=2.5
	### the others, we want to scan them in, but need to make sure mean is adjusted to zero
	odum=prior2009$coef[match(prior2010$aateam[oldsax],prior2009$team)]
	prior2010$coef[oldsax]=odum+(sum(odum)-sum(prior2010$coef[newsax]))/length(oldsax)

	teamYearDF = rddf %>%
								distinct(year, team)
	### need to adapt that, want to build an array of priors for each team at the end of each season and the start of each season
	### end of the season is fine, just use whatever final rating was for them
	### but the start, we need to adjust for any new teams or teams disappearing
	teamyrdf[,c('startprior','endprior')]=NA
	### so let's put our end of 2009 data into this firstly
	teamyrdf[which(teamyrdf$yr==2010),'startprior']=prior2010$coef[match(teamyrdf$aateam[which(teamyrdf$yr==2010)],prior2010$aateam)]

	### now fill out the rest
	calceoyprior=function(x) {
		### want to calculate team coef at end of previous season
		endofyeardaynum=NULL
		sax=which(rddf$yr==teamyrdf$yr[x] & rddf$team==teamyrdf$team[x])
		if (length(sax)>0) {
			endofyeardaynum=max(rddf$daynum[sax])
		}
		lsprior=NA
		if (!is.null(endofyeardaynum)) {
			dum=which(!is.na(rddf$startinggrid) & !is.na(rddf$chrsmdcoef) & rddf$team==teamyrdf$team[x] & rddf$daynum==endofyeardaynum)
			lsprior=mean(rddf$chrsmdcoef[dum])
		}
		return(lsprior)
	}
	teamyrdf[,'endprior']=sapply(1:nrow(teamyrdf),calceoyprior)
	### ok, why is the mean not always zero...?
	### we then map previous years estimates to now, but normalise
	dum=teamyrdf[match(paste(teamyrdf$aateam,teamyrdf$yr-1),paste(teamyrdf$aateam,teamyrdf$yr)),'endprior']
	### if there are any new teams, insert 2.5 seconds in
	dum[is.na(dum)]=2.5
	dum2=tapply(dum,teamyrdf$yr,mean)
	teamyrdf$startprior[which(teamyrdf$yr>2010)]=(dum-dum2[match(teamyrdf$yr,names(dum2))])[which(teamyrdf$yr>2010)]

	### next thing to build, a previous race prior
	### need another dataframe for that...
	teamrrdf=expand.grid(rr=1:nrace,aateam=unique(rddf$aateam),stringsAsFactors=F)
	teamrrdf$yr=racedb$yr[match(teamrrdf$rr,racedb$rr)]
	### then fill with modelcoefs
	dum=tapply(rddf[,paste('mod',modelchoice,'dcoef',sep='')],paste(rddf$aateam,rddf$rr),mean,na.rm=T)
	teamrrdf$dcoef=as.numeric(dum)[match(paste(teamrrdf$aateam,teamrrdf$rr),names(dum))]
	teamrrdf$dcoef[which(teamrrdf$chrsmdcoef=='NaN')]=NA
	### but should put more weight on the one with more observations...ugh, no need to think more about this
	### then fill with the chosen smooth
	dum=tapply(rddf$chrsmdcoef,paste(rddf$aateam,rddf$rr),mean,na.rm=T)
	teamrrdf$chrsmdcoef=as.numeric(dum)[match(paste(teamrrdf$aateam,teamrrdf$rr),names(dum))]
	teamrrdf$chrsmdcoef[which(teamrrdf$chrsmdcoef=='NaN')]=NA

	### also want the smooth to continue even when the team drops out of the season, which is a massive pain in the arse
	dumf=function(sax) {
		tcoef=teamrrdf$chrsmdcoef[sax]
		yesna=which(is.na(tcoef))
		csnotna=cumsum(!is.na(tcoef))
		tcoefimpute=tcoef[match(0:max(csnotna),csnotna)][match(csnotna,0:max(csnotna))]
		return(tcoefimpute)
	}
	dum=tapply(1:nrow(teamrrdf),paste(teamrrdf$aateam,teamrrdf$yr),dumf)
	teamrrdf$chrsmdcoef=as.numeric(unlist(dum))[match(1:nrow(teamrrdf),with(teamrrdf,order(aateam,rr)))]

	### then just a simple matter of matching to the previous one
	### except at the start of the season where we need to normalise for departing/newly joing new teams
	### so use teamyrdf for that
	dum=match(paste(teamrrdf$aateam,teamrrdf$rr-1),paste(teamrrdf$aateam,teamrrdf$rr))
	sax=which(teamrrdf$yr==teamrrdf$yr[dum])
	teamrrdf$priorchrsmdcoef=rep(NA,nrow(teamrrdf))
	teamrrdf$priorchrsmdcoef[sax]=teamrrdf$chrsmdcoef[dum[sax]]
	### for any that are still NA, this ought to be the correct thing to do
	sax=which(is.na(teamrrdf$priorchrsmdcoef))
	teamrrdf$priorchrsmdcoef[sax]=teamyrdf$startprior[match(paste(teamrrdf$aateam,teamrrdf$yr)[sax],paste(teamyrdf$aateam,teamyrdf$yr))]

	### finally....allocate that to rddf
	teamprior=teamrrdf$priorchrsmdcoef[match(paste(rddf$aateam,rddf$rr),paste(teamrrdf$aateam,teamrrdf$rr))]
	return(teamprior)
}

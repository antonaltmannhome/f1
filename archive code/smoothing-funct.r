
ProcessCoef=function(theta, modelinfo, display=F) {
	# runmode can be 'lik' or 'display'
	mycoef=NULL
	count=1
	modelinfo$method = 'downweight'
	if (modelinfo$qrtofit %in% c('q','qr')) {
		if (modelinfo$method=='downweight') mycoef$qualtdwcoef=exp(theta[count])
		if (modelinfo$method=='timeseries') mycoef$qualrwvar=exp(theta[count])
		count=count+1
	}
	if (modelinfo$qrtofit %in% c('r','qr')) {
		if (modelinfo$method=='downweight') mycoef$racetdwcoef=exp(theta[count])
		if (modelinfo$method=='timeseries') mycoef$racerwvar=exp(theta[count])
		count=count+1
	}
	if (modelinfo$qrtofit=='qr') {
		if (modelinfo$method=='downweight') mycoef$qrmix=invlogit(theta[count])
		if (modelinfo$method=='timeseries') mycoef$qrmix=invlogit(theta[count])
		count=count+1
	}
	if (modelinfo$method=='timeseries') {
		mycoef$datavar=exp(theta[count])
		count=count+1
	}
	if (display) {
		cat(paste(names(mycoef),mycoef,sep=': '),sep='\n')
	}
	return(mycoef)
}



smtsfunct=function(mycoef, mydaynum, modelinfo, rddf, qualfitsax, racefitsax, dmatlist) {
	
	if (modelinfo$usequal) {
		if (any(rddf$modQualPredNValid[qualfitsax]<1E-12) | any(is.na(rddf$modQualPredNValid[qualfitsax]))) stop('Some of the coefficients to smooth have weighting of zero or are NA, exiting...\n')
	}
	if (modelinfo$userace) {
		if (any(rddf[racefitsax,'racePredNValid']<1E-12) | any(is.na(rddf[racefitsax,'racePredNValid'])) ) stop('Some of the coefficients to smooth have weighting of zero or are NA, exiting...\n')
	}
	
	myundrivteamyear=unique(c(rddf$driverTeamYear[qualfitsax],rddf$driverTeamYear[racefitsax]))
	modelinfo$method = 'downweight'
	if (modelinfo$method=='downweight') {
		### qualy
		if (modelinfo$usequal) {
			qualtwgtvec=exp(-mycoef$qualtdwcoef*abs(mydaynum-raceDF$daynum[rddf$rr[qualfitsax]]))
		}
		### race
		if (modelinfo$userace) {
			racetwgtvec=exp(-mycoef$racetdwcoef*abs(mydaynum-raceDF$daynum[rddf$rr[racefitsax]]))
		}
		speedvec=dvec=wvec=NULL
		if (modelinfo$usequal) {
			speedvec=c(speedvec,rddf$qualDCoefToSmooth[qualfitsax])
			dvec=c(dvec,rddf$driverTeamYear[qualfitsax])
			if (modelinfo$qrtofit=='q') wvec=c(wvec,qualtwgtvec*rddf$qualnumobwgtvec[qualfitsax])
			if (modelinfo$qrtofit=='qr') wvec=c(wvec,mycoef$qrmix*qualtwgtvec*rddf$qualnumobwgtvec[qualfitsax])
		}
		if (modelinfo$userace) {
			speedvec=c(speedvec,rddf$raceDCoefToSmooth[racefitsax])
			dvec=c(dvec,rddf$driverTeamYear[racefitsax])
			if (modelinfo$qrtofit=='r') wvec=c(wvec,racetwgtvec*rddf$racenumobwgtvec[racefitsax])
			if (modelinfo$qrtofit=='qr') wvec=c(wvec,(1-mycoef$qrmix)*racetwgtvec*rddf$racenumobwgtvec[racefitsax])
		}
		
		dum1=tapply(speedvec*wvec,dvec,sum)
		dum2=tapply(wvec,dvec,sum)
		ovsm=dum1/dum2
		
		retval=list(sm=ovsm, numob=dum2)
	}
	
	if (modelinfo$method=='timeseries') {
		### going to sapply the solving algorithm to each driver one by one
		### need to build up big list of info to pass to the function
		if (modelinfo$usequal) {
			ixlist=plyr::llply(myundrivteamyear,function(x) qualfitsax[rddf$driverTeamYear[qualfitsax]==x])
			qtslist=plyr::llply(ixlist, tssolve, mycoef=mycoef, qrchoice='q', usestretch=modelinfo$usestretch, rddf=rddf, dmatlist=dmatlist)
			myqnumob=as.numeric(sapply(ixlist,length)>0)
			myqpred=sapply(1:length(myundrivteamyear),getspoint,mydaynum=mydaynum,modelinfo=modelinfo,dumlist=list(ixlist=ixlist,tslist=qtslist))
		}
		if (modelinfo$userace) {
			ixlist=plyr::llply(myundrivteamyear,function(x) racefitsax[rddf$driverTeamYear[racefitsax]==x])
			rtslist=plyr::llply(ixlist, tssolve, mycoef=mycoef, qrchoice='r', usestretch=modelinfo$usestretch, rddf=rddf, dmatlist=dmatlist)
			myrnumob=as.numeric(sapply(ixlist,length)>0)
			myrpred=sapply(1:length(myundrivteamyear),getspoint,mydaynum=mydaynum,modelinfo=modelinfo,dumlist=list(ixlist=ixlist,tslist=rtslist))
		}
		### at the moment, only return the point in the series that prediction is needed for
		### will think about how to return the entire series, and also some form of variance
		if (modelinfo$qrtofit=='q') {
			mypred=myqpred
			mynumob=myqnumob
		}
		if (modelinfo$qrtofit=='r') {
			mypred=myrpred
			mynumob=myrnumob
		}
		if (modelinfo$qrtofit=='qr') {
			mynumob=mycoef$qrmix*myqnumob + (1-mycoef$qrmix)*myrnumob
			mypred=(mycoef$qrmix*myqnumob*myqpred + (1-mycoef$qrmix)*myrnumob*myrpred)/mynumob
		}
		names(mypred)=names(mynumob)=myundrivteamyear
		retval=list(sm=mypred,numob=mynumob)
	}
	return(retval)
}

getspoint=function(nn, mydaynum, modelinfo, dumlist) {
	### bit of an arse trying to get the relevant point to use for each timeseries so let's have it in this function
	myts=dumlist$tslist[[nn]]
	mydaylist=rddf$daynum[dumlist$ixlist[[nn]]]
	if (modelinfo$usecurrentrace==1) retval=myts[which(mydaylist==mydaynum)]
	if (modelinfo$usecurrentrace==0) {
		if (modelinfo$fwbw=='bw') retval=tail(myts,1)
		if (modelinfo$fwbw=='fwbw') {
			if (length(myts)==1) retval=myts
			if (length(myts)>1) {
				if (mydaynum<mydaylist[1]) retval=myts[1]
				if (mydaynum>tail(mydaylist,1)) retval=tail(myts,1)
				if (mydaynum>mydaylist[1] & mydaynum<tail(mydaylist,1)) {
					retval=approx(mydaylist,myts,mydaynum)$y
				}
			}
		}
	}
	return(retval)
}

tssolve=function(sax, mycoef, qrchoice, usestretch, rddf, dmatlist) {
	npoint=length(sax)
	retval=NULL
	if (npoint==0) {retval=-1E12}
	if (qrchoice=='q') {
		wgtvec=rddf$qualnumobwgtvec[sax]
		dcoeftosmooth=rddf$qualDCoefToSmooth[sax]
		rwvar=mycoef$qualrwvar
	}
	if (qrchoice=='r') {
		wgtvec=rddf$racenumobwgtvec[sax]
		dcoeftosmooth=rddf$raceDCoefToSmooth[sax]
		rwvar=mycoef$racerwvar
	}
	if (npoint==1) retval=dcoeftosmooth
	if (npoint>=2) {
		ddgap=c(NA,diff(rddf$daynum[sax])/14)
		msax2=NULL
		if (npoint>2) msax2=2:(npoint-1)
		lhmat=matrix(0,nrow=npoint,ncol=npoint)
		lhmat[dmatlist[[npoint]]$dmat0[1,,drop=F]]=wgtvec[1]/mycoef$datavar + 1/(rwvar*ddgap[2]^2)
		lhmat[dmatlist[[npoint]]$dmat1[1,,drop=F]]=-1/(rwvar*ddgap[2]^2)
		lhmat[dmatlist[[npoint]]$dmat0[npoint,,drop=F]]=wgtvec[npoint]/mycoef$datavar + 1/(rwvar*ddgap[npoint]^2)
		lhmat[dmatlist[[npoint]]$dmatm1[npoint-1,,drop=F]]=- 1/(rwvar*ddgap[npoint]^2)
		if (length(msax2)>0) {
			lhmat[dmatlist[[npoint]]$dmat0[msax2,,drop=F]]=wgtvec[msax2]/mycoef$datavar + 1/(rwvar*ddgap[msax2]^2) + 1/(rwvar*ddgap[msax2+1]^2)
			lhmat[dmatlist[[npoint]]$dmat1[msax2,,drop=F]]=-1/(rwvar*ddgap[msax2+1]^2)
			lhmat[dmatlist[[npoint]]$dmatm1[msax2-1,,drop=F]]=-1/(rwvar*ddgap[msax2]^2)
		}
		rhmat=wgtvec*dcoeftosmooth/mycoef$datavar
		tsopt=solve(lhmat,rhmat)
		retval=tsopt
	}
	return(retval)
}

fitbyrace=function(theta, myRace, modelinfo, rddf, runmode='max') {
	
	myrr = with(raceDF, rr[race == myRace])
	### now we do the smoothing/timeseries for all races
	### let's have theta in  a nice form:
	mycoef=ProcessCoef(theta, modelinfo)
	
	### let's make all of these matrices at once so we don't need to redfine them
	### they are necessary for the timeseries solving
	dmatlist=NULL
	for (k in 2:max(table(rddf$driverTeamYear))) {
		dmatlist[[k]]=list(dmat0=cbind(1:k,1:k), dmat1=cbind(1:(k-1),2:k), dmatm1=cbind(2:k,1:(k-1)))
	}
	
	smval=rep(NA,nrow(rddf))
	numobval=rep(0,nrow(rddf))
	for (ri in 1:myrr) {
		qualfitsax=NULL
		racefitsax=NULL
		if (modelinfo$usequal) qualfitsax=with(rddf,which(year==raceDF$year[ri] & modQualPredNValid>0))
		if (modelinfo$userace) racefitsax=with(rddf,which(year==raceDF$year[ri] & racePredNValid>0))
		
		### however, in the (common) case we don't want to use current race, need to flush out those points
		if (modelinfo$usecurrentrace==0) {
			qualfitsax=setdiff(qualfitsax,which(rddf$rr==ri))
			racefitsax=setdiff(racefitsax,which(rddf$rr==ri))
		}
		### and if we're only looking backwards, then get rid of everything that happened after ri
		if (modelinfo$fwbw=='bw') {
			qualfitsax=setdiff(qualfitsax,which(rddf$daynum>raceDF$daynum[ri]))
			racefitsax=setdiff(racefitsax,which(rddf$daynum>raceDF$daynum[ri]))
		}
		if (length(qualfitsax)>0 | length(racefitsax)>0) {
			### build up a environment for smtsfunct
			mydaynum=raceDF$daynum[ri]
			#### here's the big call
			ovsm=smtsfunct(mycoef, mydaynum, modelinfo, rddf, qualfitsax, racefitsax, dmatlist)
			rsax=which(rddf$rr==ri)
			sax=which(names(ovsm$sm) %in% rddf$driverTeamYear[rsax])
			smval[rsax[match(names(ovsm$sm)[sax],rddf$driverTeamYear[rsax])]]=ovsm$sm[sax]
			### now, are we stretching or not? if we are, need to multiply stretch coef back in here
			if (modelinfo$usestretch==1) {
				smval[which(rddf$rr==ri)]=smval[which(rddf$rr==ri)] * rddf$stretch[which(rddf$rr==ri)]
			}
			numobval[rsax[match(names(ovsm$sm)[sax],rddf$driverTeamYear[rsax])]]=ovsm$numob[sax]
		}
	}
	
	### now, if we're doing 'fit' mode, our work is done
	if (runmode=='fit') {
		### we want to return the likelihood stat, but be careful to only calculate it over points that both the more picky qrtofit choice ('q' and 'r')
		if (modelinfo$qrtopredict == 'q') sax=with(rddf,which(isvalidqpred==1 & rr<=myrr))
		if (modelinfo$qrtopredict == 'r') sax=with(rddf,which(isvalidrpred==1 & rr<=myrr))
		if (modelinfo$qrtopredict == 'qr') sax=with(rddf,which(isvalidqrpred==1 & rr<=myrr))
		sqdiff=mean( (rddf$numobForPredict*(smval-rddf$dCoefForPredict)^2)[sax])
		retval=list(smval=smval,numobval=numobval,sqdiff=sqdiff)
	}
	if (runmode=='max') {
		### we now use our validpred vectors
		if (modelinfo$qrtofit=='q') sax=with(rddf,which(isvalidqpred==1 & rr<=myrr))
		if (modelinfo$qrtofit=='r') sax=with(rddf,which(isvalidrpred==1 & rr<=myrr))
		if (modelinfo$qrtofit=='qr') sax=with(rddf,which(isvalidqrpred==1 & rr<=myrr))
		sqdiff=mean( (rddf$numobForPredict*(smval-rddf$dCoefForPredict)^2)[sax])
		retval=sqdiff
		
		### update function count ans display if necessary
		assign('itercount',itercount+1,env=globalenv())
		if ( (itercount %% 10)==0) {
			cat('Have done',itercount,'iterations so far, sqdiff is',sqdiff,'...\n')
			cat('Latest thetas tried are:\n')
			dum=ProcessCoef(theta, modelinfo, display=T)
		}
		
	}
	
	return(retval)
}

RetrieveMaxTheta = function(myRace, modelinfo) {
	
	smoothmodelfile=MakeRaceFile(myRace, 'smoothmodel.csv')
	modelparamname = GetModelParamName()
	### so, has this model already been run?
	if (!file.exists(smoothmodelfile)) {
		alreadyDone = FALSE
		maxtheta = NULL
	}
	if (file.exists(smoothmodelfile)) {

		allcombo=ReadF1Data(smoothmodelfile, 'smoothing')
		allcombo$fileisdone = !is.na(allcombo$sqdiff)

		modelinfoAsRow = purrr::map_df(modelinfo, function(x) x)
		allcombo = indicate_overlapping_combination(
					allcombo,
					modelinfoAsRow,
					modelparamname,
					'isCurrentModel')
		
		alreadyDone = with(allcombo, fileisdone[isCurrentModel])
		if (!alreadyDone) {
			maxtheta = NULL
		}
		if (alreadyDone) {
			maxtheta=as.numeric(unlist(strsplit(as.character(with(allcombo, maxtheta[isCurrentModel])),split=',')))
		}
	}
	
	return(maxtheta)
}

CheckValidModelInfo = function(modelinfo) {
	
	modelparamname = GetModelParamName()
	modelinfo$method = 'downweight'
	if (!modelinfo$method %in% c('downweight','timeseries')) stop('method has to be downweight or timeseries\n')
	### check that you haven't chosen a nonsensical combination of choices
	allcombo=GenerateCombo(filterlist=NULL)
	modelinfoAsRow = purrr::map_df(modelinfo, function(x) x)
	modelinfoAsRow = indicate_overlapping_combination(
						modelinfoAsRow, allcombo, modelparamname, 'inAllCombo')
	if (!modelinfoAsRow$inAllCombo) {
		stop('That is a strange combination of choices, assume it\'s a mistake, exiting\n')
	}
	if (!modelinfo$fwbw %in% c('bw','fwbw')) stop('fwbw has to be fwbw or bw\n')
	if (!modelinfo$usestretch %in% c(FALSE,TRUE)) stop('usestretch has to be FALSE or TRUE\n')
}

GetSmooth=function(method, qrtofit, qrtopredict, fwbw, modelchoice, usecurrentrace=FALSE, usestretch, resetMaxTheta = FALSE, myRace) {
	
	## method should be 'downweight' or 'timeseries'
	## qrtofit and qrtopredict can be 'q','r' or 'qr'
	## usecurrentrace can be 1 or 0
	## fwbw is 'fwbw' or 'bw'
	## optparam will optimise the parameters if you haven't saved them yet
	## myrr is last race you want to include when optimising parameters
	
	modelinfo=list(qrtofit=qrtofit,
					qrtopredict=qrtopredict,
					usecurrentrace=usecurrentrace,
					modelchoice=modelchoice,
					usestretch=usestretch,
					fwbw=fwbw)
	modelparamname=GetModelParamName()
	
	cat('About to process the following combination:\n')
	DebugDisplay(modelinfo)

	CheckValidModelInfo(modelinfo)
	
	if (modelinfo$modelchoice=='qual') {
	
		rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, 'qual', 'qual')
	
		rddf$racePredNValid=NULL
		rddf$raceDCoef=NULL
		rddf$qualDCoef=rddf$modQualQualDCoef
		if (modelinfo$usestretch) {
			rddf$stretch=raceDF$stretchQual[rddf$rr]
		}
	}
	
	if (modelinfo$modelchoice!='qual') {

		rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, modelinfo$modelchoice, 'qual')
		rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, modelinfo$modelchoice, 'race')

		rddf = rddf %>%
				mutate(racePredNValid = get(paste0('mod',ToUpperFirstLetter(modelchoice),'PredNValid')),
						raceDCoef = get(paste0('mod',ToUpperFirstLetter(modelchoice),'DCoef')),
						qualDCoef = get(paste0('mod',ToUpperFirstLetter(modelchoice),'QualDCoef')))
		if (modelinfo$usestretch) {
			stretchname = paste('stretch',ToUpperFirstLetter(modelchoice),sep='')
			rddf = lazy_left_join(rddf, raceDF, 'race', stretchname) %>%
					mutate(stretch = get(stretchname))
		}
	}
		
	### firstly start building up our list of valid points that we can make predictions for
	rddf$isvalidqpred=rep(FALSE,nrow(rddf))
	rddf$isvalidrpred=rep(FALSE,nrow(rddf))
	rddf$isvalidqrpred=rep(FALSE,nrow(rddf))
	
	### going to want to know this constantly, so define these variables just for brevity
	modelinfo$usequal=modelinfo$qrtofit %in% c('q','qr')
	modelinfo$userace=modelinfo$qrtofit %in% c('r','qr')
	
	for (yi in 1:length(unYear)) {
		if (modelinfo$usequal) qsax=with(rddf,which(rddf$year==unYear[yi] & rddf$modQualPredNValid>0))
		if (modelinfo$userace) {
			rsax=with(rddf,which(year==unYear[yi] & racePredNValid>0))
		}
		
		for (ri in which(raceDF$year==unYear[yi])) {
			if (modelinfo$usequal) {
				if (modelinfo$fwbw=='fwbw') subqsax=qsax[rddf$rr[qsax] != ri]
				if (modelinfo$fwbw=='bw') subqsax=qsax[rddf$rr[qsax] < ri]
				qdumdriv=unique(rddf$driverTeamYear[subqsax])
			}
			if (modelinfo$userace) {
				if (modelinfo$fwbw=='fwbw') subrsax=rsax[rddf$rr[rsax] != ri]
				if (modelinfo$fwbw=='bw') subrsax=rsax[rddf$rr[rsax] < ri]
				rdumdriv=unique(rddf$driverTeamYear[subrsax])
			}
			
			if (modelinfo$usequal) rddf$isvalidqpred[rddf$rr==ri]=with(rddf[rddf$rr==ri,],driverTeamYear %in% qdumdriv & modQualPredNValid>0)
			if (modelinfo$userace) rddf$isvalidrpred[rddf$rr==ri]=with(rddf[rddf$rr==ri,],driverTeamYear %in% rdumdriv & racePredNValid>0)
			if (modelinfo$qrtofit == 'qr') {
				if (modelinfo$qrtopredict=='q') rddf$isvalidqrpred[rddf$rr==ri]=with(rddf[rddf$rr==ri,],driverTeamYear %in% c(qdumdriv,rdumdriv) & modQualPredNValid>0)
				if (modelinfo$qrtopredict=='r') rddf$isvalidqrpred[rddf$rr==ri]=with(rddf[rddf$rr==ri,],driverTeamYear %in% c(qdumdriv,rdumdriv) & racePredNValid>0)
				if (modelinfo$qrtopredict=='qr') rddf$isvalidqrpred[rddf$rr==ri]=with(rddf[rddf$rr==ri,],driverTeamYear %in% c(qdumdriv,rdumdriv) & (modQualPredNValid>0 | racePredNValid>0) )
			}
		}
	}
	
	### next, we'll use the values derived from timeseries analysis to determine how much weight to put on points
	
	rddf$qualnumobwgtvec=rddf$racenumobwgtvec=rep(NA,dim(rddf)[1])
	
	if (modelinfo$usequal) {
		rddf$qualnumobwgtvec[which(rddf$modQualPredNValid<=1)]=0.8
		rddf$qualnumobwgtvec[which(rddf$modQualPredNValid>1)]=1
	}
	if (modelinfo$userace) {
		rddf$racenumobwgtvec[which(rddf$racePredNValid<=30)]=0.5 + 0.5*rddf$racePredNValid[which(rddf$racePredNValid<=30)]/30
		rddf$racenumobwgtvec[which(rddf$racePredNValid>30)]=1
	}
	
	### now build up our lists saying how we're going to weight qualy/race when optimising
	
	dum=MixQualifyingRaceDCoef(modelinfo$modelchoice, rddf)
	### but only use what is appropriate
	if (modelinfo$qrtopredict=='q') {
		dum$qrdcoefmat[,2]==-10E6
		dum$qrwgtmat[,2]=0
	}
	if (modelinfo$qrtopredict=='r') {
		dum$qrdcoefmat[,1]==-10E6
		dum$qrwgtmat[,1]=0
	}
	
	rddf$dCoefForPredict=with(dum,rowSums(qrdcoefmat*qrwgtmat)/rowSums(qrwgtmat))
	rddf$numobForPredict=with(dum,rowSums(qrwgtmat))
	
	### now we need to define what we're going to be smoothing - it depends on whther we're doing stretching, or mixing qs with rs
	### if we're fitting qr, then ought to adjline correct the coefs of whatever we're not predicting
	
	rddf$qualDCoefToSmooth=rddf$raceDCoefToSmooth=NA
	if (modelinfo$qrtofit=='q') {
		if (!modelinfo$usestretch) rddf$qualDCoefToSmooth=rddf$qualDCoef
		if (modelinfo$usestretch) rddf$qualDCoefToSmooth=rddf$qualDCoef/rddf$stretch
	}
	if (modelinfo$qrtofit=='r') {
		if (!modelinfo$usestretch) rddf$raceDCoefToSmooth=rddf$raceDCoef
		if (modelinfo$usestretch) rddf$raceDCoefToSmooth=rddf$raceDCoef/rddf$stretch
	}
	if (modelinfo$qrtofit=='qr') {
		isgoodqualac=rddf$racePredNValid>5 & rddf$modQualPredNValid>0
		sax=which(isgoodqualac)
		if (modelinfo$qrtopredict=='q') {
			mod=with(rddf[sax,],lm(qualDCoef~raceDCoef-1))
			rddf$acRaceDCoef=coef(mod)*rddf$raceDCoef
			if (!modelinfo$usestretch) {
				rddf$qualDCoefToSmooth=rddf$qualDCoef
				rddf$raceDCoefToSmooth=rddf$acRaceDCoef
			}
			if (modelinfo$usestretch) {
				rddf$qualDCoefToSmooth=rddf$qualDCoef/rddf$stretch
				rddf$raceDCoefToSmooth=rddf$acRaceDCoef/rddf$stretch
			}
		}
		if (modelinfo$qrtopredict=='r') {
			mod=with(rddf[sax,],lm(raceDCoef~qualDCoef-1))
			rddf$acQualDCoef=coef(mod)*rddf$qualDCoef
			if (!modelinfo$usestretch) {
				rddf$qualDCoefToSmooth=rddf$acQualDCoef
				rddf$raceDCoefToSmooth=rddf$raceDCoef
			}
			if (modelinfo$usestretch) {
				rddf$qualDCoefToSmooth=rddf$acQualDCoef/rddf$stretch
				rddf$raceDCoefToSmooth=rddf$raceDCoef/rddf$stretch
			}
		}
	}
	
	maxtheta = RetrieveMaxTheta(myRace, modelinfo)
	if (is.null(maxtheta) | resetMaxTheta) {
		optparam = TRUE
	}
	if (!is.null(maxtheta) & !resetMaxTheta) {
		optparam = FALSE
		cat('Using previously saved parameter values for smoothing:\n')
	}

	if (optparam) {
		
		itercount=0
		assign('itercount',itercount,env=globalenv())
		
		cat('About to obtain smoothing parameters via optimisation\n')
		modelinfo$method = 'downweight'
		if (modelinfo$qrtofit %in% c('q','r')) {
			if (modelinfo$method=='downweight') {
				maxinfo=optimise(fitbyrace,interval=c(log(0.00001),log(0.1)), myRace=myRace, modelinfo=modelinfo, rddf=rddf, runmode='max')
				maxtheta=maxinfo$min
			}
			if (modelinfo$method=='timeseries') {
				maxinfo=optim(f=fitbyrace,par=log(c(0.01,0.3)), myRace=myRace, modelinfo=modelinfo, rddf=rddf, runmode='max')
				maxtheta=maxinfo$par
			}
		}
		if (modelinfo$qrtofit=='qr') {
			if (modelinfo$method=='downweight') {
				maxinfo=nlm2(fitbyrace,p=c(log(c(0.01,0.01)),0), myRace=myRace, modelinfo=modelinfo, rddf=rddf, runmode='max')
				maxtheta=maxinfo$est
			}
			if (modelinfo$method=='timeseries') {
				maxinfo=nlm2(fitbyrace,p=c(log(0.01),log(0.01),0,log(0.3)), myRace=myRace, modelinfo=modelinfo, rddf=rddf, runmode='max')
				maxtheta=maxinfo$est
			}
		}
		cat('Maximum has been obtained, coefficients are:')
	}
	
	dum=ProcessCoef(maxtheta, modelinfo, display=T)
	
	### then generate the smooths to go with these
	sminfo=fitbyrace(maxtheta, myRace=myRace, modelinfo=modelinfo, rddf=rddf, runmode='fit')
	
	return(list(smval=sminfo$smval,numobval=sminfo$numobval,optcoef=maxtheta,sqdiff=sminfo$sqdiff))
}

GenerateCombo=function(filterlist) {
	### let's get in whatever we have done, don't complain about anything that's missing
	combolist=list(method=c('downweight','timeseries'),
					qrtofit=c('q','r','qr'),
					qrtopredict=c('q','r'),
					usecurrentrace=FALSE,
					modelchoice=c('qual',4,30),
					usestretch=c(FALSE,TRUE),
					fwbw=c('bw','fwbw'))
	## however, there are some combinations that make no sense
	if (!is.null(filterlist)) {
		for (j in 1:length(filterlist)) {
			combolist[names(filterlist)[j]]=filterlist[j]
		}
	}
	allcombo=expand.grid(combolist,stringsAsFactors=F)
	### however, there are combinations within that that make no sense, so let's filter a little more
	getrid=NULL
	getrid[[1]]=with(allcombo,which(qrtofit=='q' & qrtopredict!='q'))
	getrid[[2]]=with(allcombo,which(qrtofit=='r' & qrtopredict!='r'))
	getrid[[3]]=with(allcombo,which(modelchoice=='qual' & (qrtopredict!='q' | qrtofit!='q')))
	getrid=unique(do.call(c,getrid))
	if (length(getrid)>0) allcombo=allcombo[-getrid,]
	rownames(allcombo)=1:nrow(allcombo)
	return(allcombo)
}

GetModelParamName = function() {
	# very small function but total pain not having it
	modelparamname=c('qrtofit', 'qrtopredict', 'usecurrentrace', 'modelchoice', 'usestretch', 'fwbw')
	return(modelparamname)
}

# awful name, should be something like 'smoothing.storeallcoef'
RunModel=function(myRace, filterlist=NULL, reset=F) {
	### have we already prepared a file for this? if not, make it now
	smoothmodelfile=MakeRaceFile(myRace, 'smoothmodel.csv')
	modelparamname=GetModelParamName()

	filtercombo=GenerateCombo(filterlist=filterlist)

	if (!file.exists(smoothmodelfile)) {
		allcombo=GenerateCombo(filterlist=NULL)

		allcombo$fileisdone = FALSE
		allcombo$maxtheta=allcombo$smoothparam=allcombo$sqdiff=rep(NA,nrow(allcombo))
	}
	
	if (file.exists(smoothmodelfile)) {
		allcombo=ReadF1Data(smoothmodelfile, 'smoothing')
		allcombo$fileisdone = !is.na(allcombo$sqdiff)
	}
		
	allcombo = indicate_overlapping_combination(
				allcombo,
				filtercombo,
				modelparamname,
				'isInFilter')
				
	modeltorun=with(allcombo, which(isInFilter & !fileisdone))
	
	if (length(modeltorun)==0) {
		print('No new models to run, exiting\n')
		return(NULL)
	}
	if (length(modeltorun)>0) {
		cat('Have got',length(modeltorun),'models to run...\n')
		for (j in modeltorun) {
			dum=GetSmooth(method=allcombo$method[j],
									qrtofit=allcombo$qrtofit[j],
									qrtopredict=allcombo$qrtopredict[j],
									modelchoice=allcombo$modelchoice[j],
									usestretch=allcombo$usestretch[j],
									fwbw=allcombo$fwbw[j],
									myRace=myRace)
			allcombo$maxtheta[j]=paste(dum$optcoef,collapse=', ')
			cdum=ProcessCoef(dum$optcoef,modelinfo=allcombo[j,modelparamname])
			### now fill out the data frame
			allcombo$smoothparam[j]=paste(paste(names(cdum),round(unlist(cdum),6),sep=': '),collapse=', ')
			### we would also like the sqdiff
			allcombo$sqdiff[j]=dum$sqdiff
		}
	}
	# don't want to write anything that isn't either model parameters or maximum info
	maximumcolumnname = c('sqdiff', 'smoothparam', 'maxtheta')
	allcombo = allcombo %>%
				select(modelparamname, maximumcolumnname)
	write_csv(allcombo, path = smoothmodelfile)
}

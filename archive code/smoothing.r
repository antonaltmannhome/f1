### replacement to previous process_smooth and process_timeseries, this runs the set of smoothing functions in lapbylap_funct and stores parameters in a spreadsheet

### completely pointless to run this for the first couple of seasons, so declare them all done if they aren't already

## think the function below is basically crap, it's a rubbish version of 'smoothing.runmodel'


StoreSmoothOptCoef = function(myRaceName, modelchoice, usestretch) {
	
	if (modelchoice=='qual') {
		filterlist=list(qrtofit='q',qrtopredict='q',modelchoice='qual',usestretch=usestretch, method='downweight')
	}
	if (modelchoice %in% c(4,30)) {
		filterlist=list(modelchoice=modelchoice,usestretch=usestretch, method='downweight')
	}
	
	### have we already prepared a file for this? if not, make it now
	smoothmodelfile=MakeRaceFile(myRaceName, 'smoothmodel.csv')
	
	allcombo=smoothing.generatecombo(filterlist=NULL)
	modelparamname=names(allcombo)
	allcomboid=apply(allcombo[,modelparamname],1,paste,collapse=',')
	
	filtercombo=smoothing.generatecombo(filterlist=filterlist)
	filtercomboid=apply(filtercombo[,modelparamname],1,paste,collapse=',')
	
	allcombo$maxtheta=allcombo$smoothparam=allcombo$sqdiff=rep(NA,nrow(allcombo))
	if (!file.exists(smoothmodelfile)) {
		modeltorun=match(filtercomboid, allcomboid)
	}
	
	if (file.exists(smoothmodelfile)) {
		filecombo=read.csv(smoothmodelfile)
		filecomboid=apply(filecombo[,modelparamname],1,paste,collapse=',')
		fileisdone=as.numeric(!is.na(filecombo$sqdiff))
		
		allcombo$sqdiff[fileisdone==1]=filecombo[fileisdone==1,'sqdiff']
		allcombo$smoothparam[fileisdone==1]=filecombo[fileisdone==1,'smoothparam']
		allcombo$maxtheta[fileisdone==1]=filecombo[fileisdone==1,'maxtheta']
		
		modeltorun=which(allcomboid %in% filtercomboid & fileisdone==0)
	}
	
	if (length(modeltorun)==0) {
		print('No new models to run, exiting\n')
		return(NULL)
	}
	if (length(modeltorun)>0) {
		cat('Have got',length(modeltorun),'models to run...\n')
		for (j in modeltorun) {
			cat('About to process the following combination (',match(j,modeltorun),' out of ',length(modeltorun),') :\n',sep='')
			cat(paste(modelparamname,allcombo[j,modelparamname],sep=': '),sep='\n')
			dum=smoothing.getsmooth(method=allcombo[j,'method'],
									qrtofit=allcombo[j,'qrtofit'],
									qrtopredict=allcombo[j,'qrtopredict'],
									modelchoice=allcombo[j,'modelchoice'],
									fwbw=allcombo[j,'fwbw'],
									usestretch=allcombo[j,'usestretch'],
									usecurrentrace=FALSE,
									myRaceName=myRaceName)
			allcombo$maxtheta[j]=paste(dum$optcoef,collapse=', ')
			cdum=smoothing.processcoef(dum$optcoef,modelinfo=allcombo[j,modelparamname])
			### now fill out the data frame
			allcombo$smoothparam[j]=paste(paste(names(cdum),round(unlist(cdum),6),sep=': '),collapse=', ')
			### we would also like the sqdiff
			allcombo$sqdiff[j]=dum$sqdiff
		}
		write.csv(file=smoothmodelfile, allcombo,row.names=F)
	}
}

RunSmooth('qual', racedb$racename[nrace], usestretch = FALSE)

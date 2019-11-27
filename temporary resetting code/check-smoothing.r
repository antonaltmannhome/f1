

method= "downweight";
qrtofit= "q";
qrtopredict= "q";
usecurrentrace= FALSE;
modelchoice= "qual";
usestretch= FALSE;
fwbw= "fwbw";
myRace = '2018japan'

LoadAllData()
b = read.csv('c:/temp/temp.csv')
rddf = left_join(rddf, b, c('rr','driver'))
rddf$modQualQualDCoef = rddf$modqualqualdcoef
rddf$qualDCoef = rddf$modqualqualdcoef
rddf$modQualPredNValid = rddf$qualprednvalid

# in old r window:

write.csv(file='c:/temp/temp.csv',rddf[,c('rr','driver','modqualqualdcoef','qualprednvalid')])
method= 'downweight'
qrtofit= 'q'
qrtopredict= 'q'
usecurrentrace= 0
modelchoice= 'qual'
usestretch= 0
fwbw= 'fwbw'

stagename=paste('smooth',modelchoice,sep='')
donestagename=paste('donesmooth',modelchoice,sep='')
### careful - usestretch should be 1 or 0, not T or F
usestretch=as.numeric(usestretch)
if (usestretch==1) {
	stagename=paste(stagename,'stretch',sep='')
	donestagename=paste(donestagename,'stretch',sep='')
}
isvalidracename=paste('isvalidrace',modelchoice,sep='')

if (modelchoice=='qual') {
	filterlist=list(qrtofit='q',qrtopredict='q',modelchoice='qual',usestretch=usestretch, method='downweight')
}

updatestatusdf=getupdatestatusdf()
updatemodel(stagename)


	allcombo=smoothing.generatecombo(filterlist=NULL)
	modelparamname=names(allcombo)
	allcomboid=apply(allcombo[,modelparamname],1,paste,collapse=',')

	filtercombo=smoothing.generatecombo(filterlist=filterlist)
	filtercomboid=apply(filtercombo[,modelparamname],1,paste,collapse=',')
		modeltorun=match(filtercomboid, allcomboid)

	j =53
	myrr=nrace
	dum=smoothing.getsmooth(method=allcombo[j,'method'],qrtofit=allcombo[j,'qrtofit'],qrtopredict=allcombo[j,'qrtopredict'],modelchoice=allcombo[j,'modelchoice'],fwbw=allcombo[j,'fwbw'],usestretch=allcombo[j,'usestretch'],usecurrentrace=0,myrr=myrr,optparam = TRUE)
	

############################## model 4: #########################

method= "downweight";
qrtofit= "qr";
qrtopredict= "q";
usecurrentrace= FALSE;
modelchoice= "4";
usestretch= FALSE;
fwbw= "fwbw";
myRace = '2018japan'


b = read.csv('c:/temp/temp.csv')
rddf = left_join(rddf, b, c('rr','driver'))
rddf$mod4QualDCoef = rddf$mod4qualdcoef
rddf$qualDCoef = rddf$mod4qualdcoef
rddf$mod4DCoef = rddf$mod4dcoef
rddf$raceDCoef = rddf$mod4dcoef
rddf$racePredNValid = rddf$mod4prednvalid
rddf$modQualPredNValid = rddf$qualprednvalid


# in old r window:

write.csv(file='c:/temp/temp.csv',rddf[,c('rr','driver','mod4qualdcoef','mod4dcoef','mod4prednvalid','qualprednvalid')])
method= 'downweight'
qrtofit= 'qr'
qrtopredict= 'q'
usecurrentrace= 0
modelchoice= 4
usestretch= 0
fwbw= 'fwbw'

modelchoice=4

stagename=paste('smooth',modelchoice,sep='')
donestagename=paste('donesmooth',modelchoice,sep='')
### careful - usestretch should be 1 or 0, not T or F
usestretch=as.numeric(usestretch)
if (usestretch==1) {
	stagename=paste(stagename,'stretch',sep='')
	donestagename=paste(donestagename,'stretch',sep='')
}
isvalidracename=paste('isvalidrace',modelchoice,sep='')

if (modelchoice=='qual') {
	filterlist=list(qrtofit='q',qrtopredict='q',modelchoice='qual',usestretch=usestretch, method='downweight')
}
if (modelchoice %in% c(4,30,34)) {
	filterlist=list(modelchoice=modelchoice,usestretch=usestretch, method='downweight')
}

updatestatusdf=getupdatestatusdf()
updatemodel(stagename)


	allcombo=smoothing.generatecombo(filterlist=NULL)
	modelparamname=names(allcombo)
	allcomboid=apply(allcombo[,modelparamname],1,paste,collapse=',')

	filtercombo=smoothing.generatecombo(filterlist=filterlist)
	filtercomboid=apply(filtercombo[,modelparamname],1,paste,collapse=',')
		modeltorun=match(filtercomboid, allcomboid)

	j =57
	myrr=nrace
	dum=smoothing.getsmooth(method=allcombo[j,'method'],qrtofit=allcombo[j,'qrtofit'],qrtopredict=allcombo[j,'qrtopredict'],modelchoice=allcombo[j,'modelchoice'],fwbw=allcombo[j,'fwbw'],usestretch=allcombo[j,'usestretch'],usecurrentrace=0,myrr=myrr,optparam = TRUE)
	
##### stretching check

# in old window:

write.csv(file = 'c:/temp/temp.csv', racedb[,c('rr','isvalidracequal','isvalidrace4','mod4qualintercept','mod4intercept','qavgsp','ravgsp')],row.names=F)
write.csv(file = 'c:/temp/temp2.csv', rddf[,c('rr','driver','qsmdcoef','rsmdcoef')],row.names= F)

# in new window:

b = read.csv('c:/temp/temp.csv')
raceDF[,c('isValidQual','isValidRace')] = b[match(raceDF$rr,b$rr),c('isvalidracequal','isvalidrace4')]
raceDF = raceDF %>%
					mutate(isValidToFit = isValidQual | isValidRace)
					
	rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, modelchoice, 'qual')
	if (userace) {
		rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, modelchoice, 'race')
	}


	raceDF = raceDF %>%
				mutate(qualInterceptToUse = get(paste0('mod',ToUpperFirstLetter(modelchoice),'QualIntercept')))
	rddf = rddf %>%
				mutate(qualDCoefToUse = get(paste0('mod',ToUpperFirstLetter(modelchoice),'QualDCoef')))
	rddf = lazy_left_join(rddf, raceDF, 'race', 'isValidQual')
	
	if (userace) {
		raceDF = raceDF %>%
					mutate(raceInterceptToUse = get(paste0('mod',ToUpperFirstLetter(modelchoice),'Intercept')))
		rddf = rddf %>%
				mutate(raceDCoefToUse = get(paste0('mod',ToUpperFirstLetter(modelchoice),'DCoef')),
						racePredNValid = get(paste0('mod',ToUpperFirstLetter(modelchoice),'PredNValid')))
		rddf = lazy_left_join(rddf, raceDF, 'race', 'isValidRace')
		
	}
	
	### so what is slowness of race: here we divide the lap time by track perimeter
		
	raceDF$qavgsp=raceDF$qualInterceptToUse/raceDF$perim
	if (userace) raceDF$ravgsp=raceDF$raceInterceptToUse/raceDF$perim
b = read.csv('c:/temp/temp.csv')
raceDF[,c('oldqavgsp','oldravgsp','mod4intercept','mod4qualintercept')] = b[match(raceDF$rr,b$rr),c('qavgsp','ravgsp','mod4intercept','mod4qualintercept')]



# quick check to see that the actual runs are at least quite similar:

# old world:
dum=smoothing.getsmooth(method='downweight',qrtofit='qr',qrtopredict='q',usecurrentrace=0,modelchoice=4,fwbw='fwbw',usestretch=1,myrr=nrace)
rddf$sm.qrq4.fwbw.stretch = dum$smval
dum=smoothing.getsmooth(method='downweight',qrtofit='qr',qrtopredict='r',usecurrentrace=0,modelchoice=4,fwbw='fwbw',usestretch=1,myrr=nrace)
rddf$sm.qrr4.fwbw.stretch = dum$smval
dum=smoothing.getsmooth(method='downweight',qrtofit='qr',qrtopredict='q',usecurrentrace=0,modelchoice=4,fwbw='bw',usestretch=1,myrr=nrace)
rddf$sm.qrq4.bw.stretch = dum$smval
dum=smoothing.getsmooth(method='downweight',qrtofit='qr',qrtopredict='r',usecurrentrace=0,modelchoice=4,fwbw='bw',usestretch=1,myrr=nrace)
rddf$sm.qrr4.bw.stretch = dum$smval

write.csv(file = 'c:/temp/temp.csv', rddf[,c('rr','driver','sm.qrq4.fwbw.stretch','sm.qrr4.fwbw.stretch','sm.qrq4.bw.stretch','sm.qrr4.bw.stretch')],row.names=F)

# new world:
dum=f1smoothing::GetSmooth(method='downweight',
								qrtofit='qr',
								qrtopredict='q',
								usecurrentrace=FALSE,
								modelchoice=4,
								fwbw='fwbw',
								usestretch=TRUE,
								myRace=mostRecentRaceName)
rddf$sm.qrq4.fwbw.stretch = dum$smval
dum=f1smoothing::GetSmooth(method='downweight',
								qrtofit='qr',
								qrtopredict='r',
								usecurrentrace=FALSE,
								modelchoice=4,
								fwbw='fwbw',
								usestretch=TRUE,
								myRace=mostRecentRaceName)
rddf$sm.qrr4.fwbw.stretch = dum$smval
dum=f1smoothing::GetSmooth(method='downweight',
								qrtofit='qr',
								qrtopredict='q',
								usecurrentrace=FALSE,
								modelchoice=4,
								fwbw='bw',
								usestretch=TRUE,
								myRace=mostRecentRaceName)
rddf$sm.qrq4.bw.stretch = dum$smval
dum=f1smoothing::GetSmooth(method='downweight',
								qrtofit='qr',
								qrtopredict='r',
								usecurrentrace=FALSE,
								modelchoice=4,
								fwbw='bw',
								usestretch=TRUE,
								myRace=mostRecentRaceName)
rddf$sm.qrr4.bw.stretch = dum$smval

b = read.csv('c:/temp/temp.csv')

rddf2 = left_join(rddf, b, c('rr','driver'))

plot(rddf2$sm.qrq4.fwbw.stretch.x, rddf2$sm.qrq4.fwbw.stretch.y)
plot(rddf2$sm.qrr4.fwbw.stretch.x, rddf2$sm.qrr4.fwbw.stretch.y)
plot(rddf2$sm.qrq4.bw.stretch.x, rddf2$sm.qrq4.bw.stretch.y)
plot(rddf2$sm.qrr4.bw.stretch.x, rddf2$sm.qrr4.bw.stretch.y)

# some difference but always for agood reason: difference in isavlidrace between the two worlds, or old world making mistake with qualifying: not excluding ricciardo in belgium 2018 or stroll in russia 2018 - i can't check that, way too painful

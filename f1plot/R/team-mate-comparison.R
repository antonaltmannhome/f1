
TeamMateComparison=function(myYear, mydriv1,mydriv2,modelchoice,
						toFile=F,filepath = TEMPPATH, addmaintitle = FALSE) {

	#library(grid)
	#suppressWarnings(library(png))

	modelLabel = f1data:::MakeModelToUseName('rawModel30', 30)
	isValidLabel = f1data:::MakeModelToUseName('validity30', 30)

	raceDF = f1data:::MakePrettyRaceLabel(raceDF)

	myrddf=rddf %>%
				filter(year==myYear & !is.na(startingGrid) & (driver %in% c(mydriv1, mydriv2)))
	myrddf$longDriver=driverDF$longDriver[match(myrddf$driver,driverDF$driver)]
	myrddf$driverIndex = match(myrddf$driver, c(mydriv1, mydriv2))
	# spread won't work with variable names
	myrddf$rawDCoef = myrddf %>% pull(modelLabel$modRawDCoef)
	myrddf$predNValid = myrddf %>% pull(modelLabel$modPredNValid)

	myrddf$lowNumob = with(myrddf, !is.na(rawDCoef) & predNValid < 5)

	carProblemDF = f1laptimelm::DeriveCarProblemCoef(carProblemDF, modelchoice)
	myrddf = indicate_overlapping_combination(myrddf,
										carProblemDF %>%
											filter(isWholeRace),
										c('race', 'driver'),
										'isCarProblem')
	myrddf = subset_join(myrddf,
						carProblemDF %>%
						select(race, driver, mod30RawDCoef, mod30PredNValid) %>%
						rename(rawDCoef = mod30RawDCoef,
								predNValid = mod30PredNValid),
						c('race', 'driver'),
						isCarProblem)

	### note that we don't have dominant defined for model 30 so must empty that out
	if (modelchoice==30) {
		myrddf$isdominant = FALSE
	}

	### round numobs to 1 dp (although we may not use them eventually)
	if (modelchoice %in% c(30,35)) {
		myrddf[,'nicenumob']=myrddf[,modelLabel$modPredNValid]
	}
	if (modelchoice == 34) {
		myrddf[,'nicenumob']=niceround(myrddf[,modelLabel$modPredNValid],1)
	}

	myrddf = myrddf %>%
				select(race, team, driverIndex, driver, longDriver,
						rawDCoef, predNValid, lowNumob, isdominant, isCarProblem)
	sbsdf = spread_multiple(myrddf,
							keyCol = driverIndex,
							team, driver, longDriver,
							rawDCoef, predNValid, lowNumob, isdominant, isCarProblem)
	# need to add carproblem too
	names(sbsdf) = gsub('(driverIndex)(1|2)(\\_)(.+$)','\\4\\2',names(sbsdf))
	# why is prednvalid now a character, what bollox
	sbsdf$predNValid1 = as.numeric(sbsdf$predNValid1)
	sbsdf$predNValid2 = as.numeric(sbsdf$predNValid2)

	# driver(s) might have switched teams, it's common teams we want
	sbsdf = sbsdf %>% filter(team1 == team2)

	# what about badly fitted races, we don't want them
	sbsdf = lazy_left_join(sbsdf, raceDF, 'race', c('daynum', isValidLabel$isValidRace, 'prettyRace', 'prettyCountry'))
	names(sbsdf)[names(sbsdf)==isValidLabel$isValidRace] = 'isValidRace'
	terriblyFittedRace = scan(paste0(USERPATH,'data/valid-but-terribly-fitted-race.dat'), what = '', quiet = TRUE)
	sbsdf$isfittedok = TRUE
	sbsdf$isfittedok[which(sbsdf$race %in% terriblyFittedRace)] = FALSE
	curteam=sbsdf$team1[1]

	### now be careful, we don't want to pick up any numob=0 estimates
	sbsdf$diffcoef=sbsdf$rawDCoef1-sbsdf$rawDCoef2
	### truncate any above 1 to 1
	sbsdf$rawdiffcoef=sbsdf$diffcoef
	sbsdf$istrunc=as.numeric(abs(sbsdf$rawdiffcoef)>1)
	sbsdf$diffcoef=pmin(sbsdf$diffcoef,1)
	sbsdf$diffcoef=pmax(sbsdf$diffcoef,-1)
	# surely we need to this to avoid NAs? how did it ever work before?
	sbsdf$diffcoef[which(is.na(sbsdf$diffcoef))] = 0
	### put it in date order
	sbsdf=sbsdf[order(-sbsdf$daynum),]
	### but we want to add a empty line at the top which will then have driver's names inserted in
	truerace=sbsdf$race
	extrarace=c(truerace,'blankline')
	sbsdf=rbind(sbsdf,sbsdf[1,])
	### but we only want to retain the columns that are relevant,to avoid confusion
	sbsdf[dim(sbsdf)[1],]=NA
	sbsdf[dim(sbsdf)[1],'racename']='blankline'
	sbsdf[dim(sbsdf)[1],'diffcoef']=0
	sbsdf$race=factor(sbsdf$race,levels=extrarace)
	sbsdf$hjust=ifelse(sbsdf$diffcoef>0,1,0)
	fontchoice='Trebuchet MS'

	### however the thing we want to display for the race needs decorating
	# allow a little space between race name and bar
	sbsdf$racelabel=sbsdf$prettyCountry
	sbsdf$racelabel[which(sbsdf$diffcoef>0)]=paste(sbsdf$racelabel[which(sbsdf$diffcoef>0)],' ',sep='')
	sbsdf$racelabel[which(sbsdf$diffcoef<0)]=paste(' ',sbsdf$racelabel[which(sbsdf$diffcoef<0)],sep='')
	### then paste in the number of observations as before
	sbsdf$racelabel=with(sbsdf,paste(racelabel,'(',predNValid1,'/',predNValid2,')',sep=''))
	sbsdf$racelabel[sbsdf$race=='blankline']=''
	### however, we're not putting the race label in like normal in the case of races where either driver had no coef
	sbsdf$racelabel[which(is.na(sbsdf$rawDCoef1) | is.na(sbsdf$rawDCoef2))]=''

	### now start inserting the info about wet races or no observations by one driver
	sbsdf = sbsdf %>%
			mutate(truncReason = case_when(
				#!get(isValidLabel$isValidLabel$isValidRace) ~ 'WET RACE',
				!isValidRace ~ 'WET RACE',
				!isfittedok ~ 'ANALYSIS NOT ADEQUATE FOR RACE',
				isValidRace & near(predNValid1, 0) & near(predNValid2, 0) ~
					'NOT ENOUGH LAPS BY EITHER DRIVER',
				isValidRace & near(predNValid1, 0) & !near(predNValid2, 0) ~
					paste('NO VALID LAPS BY', toupper(longDriver1)),
				isValidRace & !near(predNValid1, 0) & near(predNValid2, 0) ~
					paste('NO VALID LAPS BY', toupper(longDriver2)),
				TRUE ~ '')) %>%
			mutate(reason = ifelse(truncReason == '',
									'',
									paste(toupper(prettyCountry), truncReason, sep = ' - ')))

	### then put coordinates that are relevant to these boxes in
	maxdiff=1
	nchar2=function(x) {
		#x2=plyr::llply(x,function(x) strsplit(x,split='')[[1]])
		x2 = purrr::map(x, function(x) strsplit(x,split='')[[1]])
		# x3=plyr::laply(x2,function(x) 0.5*sum(x=='I') + sum(x!='I'))
		x3 = purrr::map_dbl(x2,function(x) 0.5*sum(x=='I') + sum(x!='I'))
		return(x3)
	}
	sax=which(sbsdf$reason != '')
	sbsdf[,c('reasonycoord1','reasonycoord2','reasonxcoord1','reasonxcoord2')]=NA
	sbsdf$reasonycoord1[sax]=-0.5*nchar2(sbsdf$reason[sax])*0.5*maxdiff/17
	sbsdf$reasonycoord2[sax]=0.5*nchar2(sbsdf$reason[sax])*0.5*maxdiff/17
	sbsdf$reasonxcoord1[sax]=sax-0.35
	sbsdf$reasonxcoord2[sax]=sax+0.35

	### right, it is now time to import the pretty colours
	teamColour = ReadF1Data(paste0(USERPATH, 'data/team-colour.csv'), 'teamColour')

	mycol=with(teamColour, panel[year==myYear & team==curteam])
	myscheme=with(teamColour, scheme[year==myYear & team==curteam])
	myoutcol=with(teamColour, outlier[year==myYear & team==curteam])
	myouttext=with(teamColour, outlierText[year==myYear & team==curteam])

	if (addmaintitle) {
		maintitle = paste(toupper(curteam),myYear)
	}
	if (!addmaintitle) {
		maintitle = ''
	}

	panelbackground=element_rect(fill = mycol)
	pointcolour=NULL
	if (myscheme=='dark') {
		boxcolour='grey90'
		outlinecolour='grey20'
		textcolour='white'
		gridcolour='grey5'
		dlabelboxcolour='white'
		dlabeltextcolour='black'
	}
	if (myscheme=='light') {
		boxcolour='grey20'
		outlinecolour='grey90'
		textcolour='black'
		gridcolour='grey95'
		dlabelboxcolour='black'
		dlabeltextcolour='white'
	}

	### fiddly to come up with limits, and have them labelled nicely
	#maxdiff=ceiling(20*max(abs(sbsdf$diffcoef),na.rm=T))/20
	maxdiff=1
	ymanaxis=scale_y_continuous(limits=c(-maxdiff,maxdiff),breaks=c(seq(-maxdiff,0,le=3),seq(0,maxdiff,le=3)[-1]),labels=c(seq(maxdiff,0,le=3),seq(0,maxdiff,le=3)[-1]))
	# so get the nice drivers' names
	dtextdf=data.frame(x=dim(sbsdf)[1],y=c(-0.5,0.5)*maxdiff,dlabel=paste(sbsdf[1,c('longDriver1','longDriver2')],'advantage'))
	dtext=geom_text(data=dtextdf,mapping=aes(x=x,y=y,label=dlabel,hjust=0.5,size=3),colour=dlabeltextcolour,show.legend=F,family=fontchoice,size=4)
	### now prepare the rectangles for the drivers' names
	#boxwid=0.5 # that is relative to side of graph, not absolute
	boxwid=0.7/23*nchar(dtextdf$dlabel)
	drectdf=data.frame(x1=dim(sbsdf)[1]-0.5,x2=dim(sbsdf)[1]+0.5,y1=c(-maxdiff/2,maxdiff/2)-0.5*boxwid*maxdiff,y2=c(-maxdiff/2,maxdiff/2)+0.5*boxwid*maxdiff)
	d1rect=with(drectdf,geom_rect(xmin=x1[1],xmax=x2[1],ymin=y1[1],ymax=y2[1],fill=dlabelboxcolour))
	d2rect=with(drectdf,geom_rect(xmin=x1[2],xmax=x2[2],ymin=y1[2],ymax=y2[2],fill=dlabelboxcolour))

	### now work out where the blobs go
	### rules are: if it happens to the slower driver, pretty obvious: just align next to zero
	###            but to faster driver, then if advantage >0.5, inside their box
	###                                       if advantage <0.5, outside their box
	### we'll have a blob hierarchy too so we know where to align more than one blob
	blobwidth=0.1
	blobinnnerwidth=0.08
	blobheight=0.7
	### NB want a bit of blank space around blobs, looks awful otherwise
	blobrank=c('lowNumob','isCarProblem','isdominant')

	blobfile=c('lowNumob' = paste0(USERPATH,'icon/tmcomparison/nodata.png'),
				'isCarProblem' = paste0(USERPATH,'icon/tmcomparison/tools.png'),
				'isdominant' = paste0(USERPATH,'icon/tmcomparison/number1.png'))

	blobtypedf=expand.grid(type=blobrank,driver=1:2,stringsAsFactors = FALSE)
	blobtypedf$colname=apply(blobtypedf,1,paste,collapse='')
	blobtypedf$blobfile=blobfile[blobtypedf$type]
	blobdf=sbsdf[,c('race','driver1','driver2')]
	blobdf[,blobtypedf$colname]=NA
	for (drivi in 1:2) {
		blobsax=which(blobtypedf[,'driver']==drivi)
		myblobname=blobtypedf[blobsax,'colname']
		problemdf=sbsdf[,myblobname]
		problemdf[which(is.na(problemdf),arr=T)]=F # for the current purposes, NA is treated as F
		dumf=function(x) {
			x2=rep(NA,length(x))
			if (any(x)) x2[which(x==T)]=(1:sum(x))*blobwidth - blobwidth
			return(x2)
		}
		blobdf[,myblobname]=t(apply(problemdf,1,dumf))
		### but then need to realign according to speed coefs
		if (drivi==1) {
			saxslow=with(sbsdf,which(diffcoef>0))
			saxfast=with(sbsdf,which(diffcoef>-0.75 & diffcoef<0))
			saxveryfast=with(sbsdf,which(diffcoef>(-1+1E-06) & diffcoef<(-0.75)))
			saxveryveryfast=with(sbsdf,which(diffcoef<(-1+1E-06)))
			blobdf[saxslow,myblobname]=blobdf[saxslow,myblobname]-blobwidth*rowSums(!is.na(blobdf[saxslow,myblobname]))
			blobdf[saxfast,myblobname]=sbsdf$diffcoef[saxfast] - blobwidth*rowSums(!is.na(blobdf[saxfast,myblobname])) + blobdf[saxfast,myblobname]
			blobdf[saxveryfast,myblobname]=sbsdf$diffcoef[saxveryfast] + blobdf[saxveryfast,myblobname]
			### 0.8 to allow room for display of gaps > 1.0
			blobdf[saxveryveryfast,myblobname]=pmax(-0.85,sbsdf$diffcoef[saxveryveryfast]) + blobdf[saxveryveryfast,myblobname]
		}
		if (drivi==2) {
			saxslow=with(sbsdf,which(diffcoef<0))
			saxfast=with(sbsdf,which(diffcoef>0 & diffcoef<0.75))
			saxveryfast=with(sbsdf,which(diffcoef>0.75 & diffcoef<1-1E-06))
			saxveryveryfast=with(sbsdf,which(diffcoef>1-1E-06))
			## slow ones actually in the correct place
			blobdf[saxfast,myblobname]=sbsdf$diffcoef[saxfast] + blobdf[saxfast,myblobname]
			blobdf[saxveryfast,myblobname]=sbsdf$diffcoef[saxveryfast] - blobwidth*rowSums(!is.na(blobdf[saxveryfast,myblobname])) + blobdf[saxveryfast,myblobname]
			### 0.8 to allow room for display of gaps > 1.0
			blobdf[saxveryveryfast,myblobname]=pmin(0.85,sbsdf$diffcoef[saxveryveryfast]) - blobwidth*rowSums(!is.na(blobdf[saxveryveryfast,myblobname])) + blobdf[saxveryveryfast,myblobname]
		}
	}
	### however...don't want blobs when the comparison is null and void for serparate reason
	blobdf[which(sbsdf$reason!=''),blobtypedf$colname]=NA

	### final thing: where are we going to put the race labels? we want them to shift in line with the blobs

	### has the slower driver got a blob? if so, shift label along
	slowblobsum=rep(0,nrow(sbsdf))
	slowblobsum[which(sbsdf$diffcoef>0)]=-rowSums(!is.na(blobdf[which(sbsdf$diffcoef>0),blobtypedf[which(blobtypedf$driver==1),'colname']]))
	slowblobsum[which(sbsdf$diffcoef<0)]=rowSums(!is.na(blobdf[which(sbsdf$diffcoef<0),blobtypedf[which(blobtypedf$driver==2),'colname']]))
	sbsdf$ylabel=blobwidth*slowblobsum

	#localenv <- environment()
	myinit = ggplot(sbsdf,
				#aes(x = race, y = diffcoef, label = race, hjust = hjust), environment = localenv) +
				aes(x = race, y = diffcoef, label = race, hjust = hjust)) +
				labs(title = maintitle,
						y = 'seconds',
						family = fontchoice) +
						theme(plot.title = element_text(hjust = 0.5),
								axis.ticks.y = element_blank(),
								axis.text.y = element_blank(),
								axis.title.y = element_blank(),
								panel.border = element_rect(colour = boxcolour,
																fill = NA,
																size = 1),
								panel.background = panelbackground,
								panel.grid.major = element_line(colour = gridcolour), panel.grid.minor = element_line(colour = gridcolour)) + ymanaxis #
	myplot = myinit +
				geom_bar(fill = boxcolour,
							stat = 'identity',
							colour = outlinecolour) +
				coord_flip() +
				geom_text(aes(x = race, y = ylabel, label = racelabel),
							size = 4, colour = textcolour, family = fontchoice) +
				d1rect + d2rect + dtext
	### did we have to truncate any? if so, label what the value should have been
	if (any(sbsdf$istrunc==1,na.rm=T)) {
		sax=which(sbsdf$istrunc==1)
		truncdf=data.frame(rawdiffcoef=sbsdf$rawdiffcoef[sax])
		truncdf$hjust=ifelse(truncdf$rawdiffcoef<0,-0.1,1.1)
		truncdf$ypos=ifelse(truncdf$rawdiffcoef<0,-1,1)
		truncdf$rdiff=sprintf("%.2f",round(abs(truncdf$rawdiffcoef),2))
		### turn the text into ggplot object
		trunctext=geom_text(data=truncdf,mapping=aes(x=sax,y=ypos,label=rdiff,hjust=hjust,size=3),colour=dlabeltextcolour,show.legend=F,family='Arial Black')
		myplot=myplot+trunctext
	}

	### then make the rectangle code - seem to have to do this one at a time
	boxsax=which(sbsdf$reason!='')
	if (length(boxsax)>0) {
		for (ci in 1:length(boxsax)) {
			dum=with(sbsdf[boxsax[ci],],geom_rect(ymin=reasonycoord1[1],ymax=reasonycoord2[1],xmin=reasonxcoord1[1],xmax=reasonxcoord2[1],fill=myoutcol))
			myplot=myplot + dum
		}
		### and turn the text into ggplot object
		boxtext=geom_text(data=sbsdf[boxsax,],mapping=aes(x=boxsax,y=0,label=reason,hjust=0.5,size=3),colour=myouttext,show.legend=F,family=fontchoice,size=3.5)
		myplot=myplot+boxtext
	}

	## want to standardize the width of the bars - so plot height should be 2*0.75, for the margins, plus about 2.75 for each line of sbsdf
	aadev.off()
	mywidth=6
	myheight=1.25 + 0.25*dim(sbsdf)[1]
	dev.new(width = mywidth,height = myheight)

	### now cycle through the blobs
	for (j in 1:nrow(blobtypedf)) {
		myblobcol=blobtypedf[j,'colname']
		currentblobsax=which(!is.na(blobdf[,myblobcol]))
		if (length(currentblobsax)>0) {
			for (k in 1:length(currentblobsax)) {
				#myplot = myplot + annotation_custom(get(blobtypedf[j,'blobicon']),xmin=currentblobsax[k]-0.5*blobheight,xmax=currentblobsax[k]+0.5*blobheight,ymin=blobdf[currentblobsax[k],myblobcol],ymax=blobdf[currentblobsax[k],myblobcol]+blobwidth)
				myplot = myplot +
							aafunct:::AddPictureToGgPlot(
								blobtypedf$blobfile[j],
								xmin=currentblobsax[k]-0.5*blobheight,
								xmax=currentblobsax[k]+0.5*blobheight,
								ymin=blobdf[currentblobsax[k],] %>% pull(myblobcol),
								ymax=blobdf[currentblobsax[k],] %>% pull(myblobcol) +blobwidth)
			}
		}
	}

	#suppressWarnings(print(myplot))
	print(myplot)

	if (toFile) {
		shortFile = paste0(mydriv1,'_',mydriv2,'_',myYear,'_',modelchoice,'.png')
		fullFile = .TeamMateMakeFile(myYear, modelchoice, shortFile)
		#savePlot(fileout,type='png')
		aafunct:::NiceGgSave(myplot, fullFile, mywidth, myheight)
	}

	bringToTop(-1)

}

.TeamMateMakeFile = function(myYear, modelchoice, shortFile) {
	longerFile = paste0('tmcomparison/modelchoice', 30, '/',shortFile)
	fullFile = MakeYearFile(myYear = myYear, myFile = longerFile)
	return(fullFile)
}

CombineTeamMateComparison = function(myYear, modelchoice) {
	PLOTPATH = paste(USERPATH, 'data/',myYear, '/tmcomparison/modelchoice', modelchoice, '/', sep = '')
	if (!dir.exists(PLOTPATH)) dir.create(PLOTPATH)

	tmpairing = f1data:::GetAllDriverTeamPairingByYear(myYear)
	
	# do we want to add main title? if it's the first in the list, yes
	tmpairing = tmpairing %>%
	  group_by(team) %>%
	  mutate(addmaintitle = c(TRUE, rep(FALSE, n() - 1))) %>%
	  ungroup()
	
	for (j in 1:nrow(tmpairing)) {
		f1plot:::TeamMateComparison(myYear, tmpairing$driver1[j], tmpairing$driver2[j], modelchoice,
							toFile = TRUE, addmaintitle = tmpairing$addmaintitle[j])
	}

	# now we want to combine the pairings by team
	myunteam = unique(tmpairing$team)
	for (j in 1:length(myunteam)) {
		currentnumpair = sum(tmpairing$team == myunteam[j])
		currentTeamCombinedShortFile = gsub(' ', '', paste(myunteam[j], '_', myYear, '_',  modelchoice, '.png', sep = ''))
		currentPairingShortFile = with(tmpairing, paste(driver1, '_', driver2, '_', myYear, '_', modelchoice,'.png', sep = '')[team == myunteam[j]],)

		currentTeamCombinedFile = f1plot:::.TeamMateMakeFile(myYear, modelchoice, currentTeamCombinedShortFile)
		currentPairingFile = f1plot:::.TeamMateMakeFile(myYear, modelchoice, currentPairingShortFile)

		aafunct::PictureMerge(currentPairingFile,
								currentTeamCombinedFile,
								rep(1, currentnumpair),
								deleteSourceFile = TRUE)
	}
}

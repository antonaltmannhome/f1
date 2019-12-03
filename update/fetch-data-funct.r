FetchSeasonInfoFromForix = function(myYear, YRDIRTOUSE)	{
	### let's get latest incarnation of raceDF, it might have changed due to shortened or cancelled race..
	visitsave(paste('http://forix.autosport.com/cp.php?l=0&r=',myYear,'&c=1',sep=''), thiscomputer, paste(YRDIRTOUSE,'/raceDF.htm',sep=''))
	Sys.sleep(1)
	### now scan that in and save neatly
	MakeRaceDF(myYear, YRDIRTOUSE)
	raceDF=read.csv(paste(YRDIRTOUSE,'/raceDF.csv',sep=''),as.is=T)
	return(raceDF)
}

InitialiseYearlyDirectory = function(YRDIRTOUSE, HTMLYRDIRTOUSE, raceDF) {

	RACEDIRTOUSE=paste(YRDIRTOUSE,raceDF$race,sep='/')
	for (i in RACEDIRTOUSE) dircheckmake(i)
	HTMLRACEDIRTOUSE=paste(HTMLYRDIRTOUSE,raceDF$race,sep='/')
	for (i in HTMLRACEDIRTOUSE) dircheckmake(i)
	DRIVERRACEDIRTOUSE=paste(HTMLRACEDIRTOUSE,'/driverrace',sep='')
	for (i in DRIVERRACEDIRTOUSE) dircheckmake(i)
	### make a directory to put any relevant plots for the blog into
	WEBSITEDIRTOUSE=paste(RACEDIRTOUSE,'/websiteplot',sep='')
	for (i in WEBSITEDIRTOUSE) dircheckmake(i)
	### and also tmcomparison
	TMCOMPARISONDIRTOUSE=paste(YRDIRTOUSE,'/tmcomparison',sep='')
	dircheckmake(TMCOMPARISONDIRTOUSE)
}

### raceDF.csv is made by copying and pasting, as it probably should be, however you then need to do this to clean up the names and dates
MakeRaceDF=function(myYear, mydirtouse) {
	### NB ditch saving into openoffice as a csv, such a horrible method, let's do it the proper way
	b=scan(paste(mydirtouse,'/raceDF.htm',sep=''),quiet=T,sep='\n',what='')
	greg=grep('(gp|ng)\\.php.+[0-9]{8}.+ci\\.php.+[0-9]{10}',b)
	dum=b[greg]
	dum2=regexpr('>[0-9]{1,2}\\&nbsp',dum)
	minib=sapply(1:length(greg),function(j) substr(dum[j],dum2[j],nchar(dum[j])))
	### but first race of year is always full of crap at the start so clean up
	raceDF=array(NA,dim=c(length(minib),8))
	for (j in 1:length(minib)) {
		dum2=strsplit(minib[j],split='>')[[1]]
		### want the following columns: date,country,circuit,perim,nlap,totdist
		### but we have do do different things depending on whether race has happened yet
		dum3=gsub('\\&nbsp;','',gsub('<.+$','',dum2[c(6,11,17,22,26,30)]))
		### let's also tack on the html address to that
		dum4=gsub('(^.+r=)([0-9]+)(.+$)','\\2',dum2[10])
		raceDF[j,]=c(j,dum3,dum4)
	}
	raceDF=data.frame(raceDF,stringsAsFactors=F)
	names(raceDF)=c('rr','date','country','circuit','perim','nlap','totalDistance','htmlName')
	## remove trailing spaces in country and circuit names
	raceDF$country=tolower(gsub('[^[:alpha:] ]',' ',gsub('[[:space:]]+$','',raceDF$country)))
	raceDF$circuit=tolower(gsub('[^[:alpha:] ]',' ',gsub('[[:space:]]+$','',raceDF$circuit)))
	raceDF$race=gsub(' ','',paste(myYear,raceDF$country))

	### replace the horrible date format
	raceDF$date=lubridate::dmy(paste(raceDF$date,myYear))

	### no need for rr, remove to avoid confusion
	raceDF=raceDF[,which(names(raceDF)!='rr')]

	fileout=paste(mydirtouse,'/raceDF.csv',sep='')
	write.csv(file=fileout,raceDF,row.names=F)
}

CompileWebAndFileAddress = function(raceToGet, raceDF, myYear) {

	raceToGetIndex = which(raceDF$race %in% raceToGet)
	FORIXPATH='http://forix.autosport.com/gp.php?l=0&r='
	pageForixNumber = read.csv(paste0(USERPATH, 'f1admin/page-forixnumber.csv'), as.is = TRUE)

	# however, depending on the year, not all of these things will always be available
	forixHasGapsChart = f1admin::CheckYearAttribute(myYear, 'haveGapsChart')
	if (!forixHasGapsChart) {
		pageForixNumber = pageForixNumber %>%
							filter(pagetype != 'laptime')
	}

	filedf = tibble(file = paste0(pageForixNumber$pagetype, '.htm'))

	pagedf = tibble(race = raceToGet)
	pagedf[, pageForixNumber$pagetype] = NA
	for (j in 1:nrow(pageForixNumber)) {
		myWebAddress = paste0(FORIXPATH, raceDF$htmlName[raceToGetIndex], '&c=', pageForixNumber$forixnumber[j])
		pagedf = pagedf %>%
					mutate(!!pageForixNumber$pagetype[j] := myWebAddress)
	}
	# don't need the race any more
	pagedf = pagedf %>% select(-race)

	return(list(pagedf = pagedf, filedf = filedf))
}

FindRaceToGet = function(raceDF, rawDataStatus) {

	raceDF = left_join(raceDF, rawDataStatus, 'race')

	raceToGet = raceDF %>%
					filter(!fetched & date < lubridate::now()) %>%
					pull(race)

	return(raceToGet)
}

cleanline=function(htmlline) {
	## get rid of everything before driver id
	#dum=regexpr('driver\\.php\\?l=0\\&r=[0-9]{10}',htmlline)
	#dum=substr(htmlline,dum[1],nchar(htmlline))
	### weed out html crap
	dum=strsplit(gsub('</a','',gsub('</td','',gsub('</span','',gsub('<td nowrap align=\"right\"','',gsub('<td nowrap align=\"left\"','',gsub('<span class=\\\"TXT-Standard-[^"]+\\\"','',htmlline)))))),split='>')[[1]]
	dum=dum[dum!='']
	return(dum)
}

StripEntryEtc = function(myRace) {

	b=scan(MakeHtmlRaceFile(myRace, 'startinggrid.htm'),sep='\n','',quiet=T)
	gridline=b[grep('driver\\.php\\?l=0\\&(amp;)*r=[0-9]{10}',b)]
	# sometimes puts the header and pole driver on same line, hopefully this line will sort that out:
	dum=regexpr('span class="TXT-Standard-4B"',gridline)
	#dum=regexpr('pa\\.php\\?l=0\\&r=[0-9]{10}',gridline)
	gridline=substr(gridline,dum,nchar(gridline))

	sgstartpos=sgforixid=rep(NA,length(gridline))
	for (k in 1:length(gridline)) {
		dum=cleanline(gridline[k])
		### NB if a driver didn't start the race, they'll have NA for this, hopefully that will be sensible
		if (length(dum)==12) {
			sgstartpos[k]=dum[2]
			sgforixid[k]=gsub('(^.+driver\\.php\\?l\\=0\\&r\\=)([0-9]{10})(.+$)','\\2',dum[6])
		}
		if (length(dum)==18) {
			sgstartpos[k]=dum[2]
			sgforixid[k]=gsub('(^.+driver\\.php\\?l\\=0\\&*r\\=)([0-9]{10})(.+$)','\\2',dum[8])
		}
	}

	### same again for entry list
	b=scan(MakeHtmlRaceFile(myRace, 'entry.htm'),sep='\n','',quiet=T)
	entryline=b[grep('driver\\.php\\?l=0\\&(amp;)*r=[0-9]{10}',b)]
	entrynumber=as.numeric(gsub('(^.+pa\\.php[^>]+>)([^<]+)(<.+$)','\\2',entryline))
	entrycar=tolower(gsub('(^.+car\\.php[^>]+>)([^<]+)(<.+$)','\\2',entryline))
	# entryforixid=gsub('(^.+driver\\.php\\?l=[0-9]\\&(amp;)*r=)([0-9]+)(\\&.+$)','\\2',entryline)
	entryforixid=gsub('(^.+driver\\.php\\?l=[0-9]\\&*r=)([0-9]+)(\\&.+$)','\\2',entryline)
	entrytyre=tolower(gsub('(^.+tyre\\.php[^>]+>)([^<]+)(<.+$)','\\2',entryline))
	dum = sapply(entryline,function(x) strsplit(x, split='>')[[1]], simplify = FALSE, USE.NAMES = FALSE)
	dumf = function(x) {
		x2=gsub('<.+','',x[grep('driver\\.php\\?l=0\\&(amp;)*r=[0-9]{10}',x)+1])
		names(x2) = NULL
		return(x2)
	}
	entrylongdriver = sapply(dum, dumf)
	# except that has horrible special characters, need to replace
	entrylongdriver = iconv(entrylongdriver, from = 'latin1', to = 'ASCII//TRANSLIT')
	# don't want apostrophes or hyphens of course
	entrylongdriver = gsub('[^a-zA-Z ]', '', entrylongdriver)
	entrydriver = ConvertLongDriverToDriver(tibble(forixId = entryforixid,
													longDriver = entrylongdriver))

	### and again for the classification
	b=scan(MakeHtmlRaceFile(myRace, 'classification.htm'),sep='\n','',quiet=T)
	classline=b[grep('driver\\.php\\?l=0\\&(amp;)*r=[0-9]{10}',b)]
	# classforixid=gsub('(^.+driver\\.php\\?l=[0-9]\\&amp;r=)([0-9]+)(\\&.+$)','\\2',classline)
	classforixid=gsub('(^.+driver\\.php\\?l=[0-9]\\&r=)([0-9]+)(\\&.+$)','\\2',classline)
	classclass=classlap=classfinpos=rep(NA,length(classline))
	for (j in 1:length(classline)) {
		dum=strsplit(classline[j],split='>')[[1]]
		classlap[j]=gsub('<.+$','',dum[rev(grep('TXT-Standard',dum))[3]+1])
		classclass[j]=gsub('<.+$','',dum[rev(grep('TXT-Standard',dum))[1]+1])
		classfinpos[j]=gsub('<.+$','',dum[rev(grep('TXT-Standard',dum))[9]+1])
	}
	### classification is total pain in arse when greater that one minute, convert to seconds
	sax=grep("^[0-9]+\\'",classclass)
	if (length(sax)>0) classclass[sax]=60*as.numeric(gsub("\\'.+$",'',classclass[sax])) + as.numeric(gsub("^.+\\'",'',classclass[sax]))
	### finpos has brackets for non-finishers, get rid of them
	sax = grep('[^0-9]', classfinpos)
	if (length(sax) > 0) classfinpos[sax] = NA
	sax = which(classfinpos == '')
	if (length(sax) > 0) classfinpos[sax] = NA

	### now match them all up to the entry list
	entryclass=entrylap=entrystartpos=entryfinpos=rep(NA,length(entryforixid))
	sax=which(entryforixid %in% classforixid)
	entryclass[sax]=classclass[match(entryforixid[sax],classforixid)]
	entrylap[sax]=classlap[match(entryforixid[sax],classforixid)]
	entryfinpos[sax]=classfinpos[match(entryforixid[sax],classforixid)]
	sax=which(entryforixid %in% sgforixid)
	entrystartpos[sax]=sgstartpos[match(entryforixid[sax],sgforixid)]

	### now get quali positions
	b=scan(MakeHtmlRaceFile(myRace, 'qualifying.htm'),sep='\n','',quiet=T)
	### need to get rid of 'ideal qualifying laps' section
	if (length(grep('Ideal qualifying laps',b)) > 0) {
		b=b[1:(grep('Ideal qualifying laps',b)-1)]
	}
	qgreg=grep('driver\\.php\\?l=0\\&(amp;)*r=[0-9]{10}',b)
	qline=b[qgreg]
	qtarr=array(NA,dim=c(length(entrydriver),3))
	### need to know which session each event happened in
	qsessline=c(grep('1st Qualifying',b),grep('2nd Qualifying',b),grep('3rd Qualifying',b))
	qsessin=rep(NA,length(qline))
	### now we need to be flexible here, a qualy session might have been abandoned
	if (length(qsessline)<3) {
		cat('It appears that only',length(qsessline),'qualifying sessions happened for',myRace,', press return if that sounds reasonable\n')
		dum=scan(what='',quiet=T,nmax=1)
	}
	if (length(qsessline)==3) {
		qsessin[qgreg>qsessline[1] & qgreg<qsessline[2]]=1
		qsessin[qgreg>qsessline[2] & qgreg<qsessline[3]]=2
		qsessin[qgreg>qsessline[3]]=3
	}
	if (length(qsessline)==2) {
		qsessin[qgreg>qsessline[1] & qgreg<qsessline[2]]=1
		qsessin[qgreg>qsessline[2]]=2
	}
	if (length(qsessline)==1) {
		qsessin[qgreg>qsessline[1]]=1
	}
	for (j in 1:length(qline)) {
		dum=strsplit(qline[j],split='>')[[1]]
		mydrivid=gsub('(^.+r=)([0-9]{10})(\\&.+$)','\\2',dum[grep('driver.php',dum)])
		ltline=grep("[0-9]+'[0-9]{2}\\.[0-9]{3}",dum)
		if (length(ltline)>0) {
			ddum=as.numeric(strsplit(gsub('<.+$','',dum[ltline]),split="['\\.]")[[1]])
			tsec=60*ddum[1] + ddum[2] + 0.001*ddum[3]
			qtarr[which(entryforixid==mydrivid),qsessin[j]]=tsec
		}
	}

	tabdat=data.frame(entrynumber,entrylongdriver,entryforixid,entrydriver,entrycar,entrytyre,entrystartpos,entrylap,entryclass,entryfinpos,qtarr[,1],qtarr[,2],qtarr[,3],stringsAsFactors=F)
	names(tabdat)=c('number','longDriver','forixId','driver','car','tyre','startingGrid','lap','classification','officialFinPos','q1','q2','q3')

	print(tabdat %>%
			arrange(as.numeric(officialFinPos)))
	print('If the table above looks correct, press enter')
	dum = askcond(F,T)

	### just write to disk for now, will database it in data_to_db.r
	write.csv(file=MakeRaceFile(myRace, 'entryetc.csv'),tabdat,row.names=F)

	return(tabdat)
}

ConvertLongDriverToDriver = function(myForixIdLongDriverDF) {
	driverAbbreviationFile = paste0(USERPATH, 'f1admin/driver-abbreviation.csv')
	everythingMatched = FALSE
	while(!everythingMatched) {
		driverAbbreviation = ReadF1Data(driverAbbreviationFile, 'driverAbbreviation')
		myForixIdLongDriverDF = lazy_left_join(myForixIdLongDriverDF,
											driverAbbreviation,
											'forixId',
											c('driver', 'surname', 'adjustedSurname'), overwrite = TRUE)

		# are there any new drivers?
		newDriver = myForixIdLongDriverDF %>%
						filter(is.na(driver))
		everythingMatched = nrow(newDriver) == 0
		if (!everythingMatched) {
			print('There are some new drivers.')
			for (di in 1:nrow(newDriver)) {
				message('What is driver id (e.g. asenna) of ', newDriver$longDriver[di],'?')
				newDriver$driver[di] = askcond(F, F)
				message('And what is their surname (e.g. Vettel, Andretti, Villeneuve)?')
				newDriver$surname[di] = askcond(F, F)
				message('And what is their adjusted surname (e.g. Vettel, Ma Andretti, Villeneuve Sr)?')
				newDriver$adjustedSurname[di] = askcond(F, F)
			}
			write_csv(newDriver, path = driverAbbreviationFile, append = TRUE)
		}
	}

	return(myForixIdLongDriverDF$driver)
}

StripPitStop = function(myRace, entryDF) {
	### the pit stop files have to be done differently due to the different type of output
	b=scan(MakeHtmlRaceFile(myRace, 'pitstop.htm'),sep='\n','',quiet=T)
	psline=b[grep('pa\\.php\\?l=0\\&(amp;)*r=[0-9]{10}.+>[0-9]+<.+$',b)]
	psforixdriver=pstime=pslap=pspenalty=rep(NA,length(psline))
	for (k in 1:length(psline)) {
		dum=strsplit(psline[k],split='>')[[1]]
		psforixdriver[k]=gsub('(.+)([0-9]{10})(.+$)','\\2',dum[grep('driver\\.php.+[0-9]{10}',dum)])
		tdum=gsub('</.+$','',dum[rev(grep('</span',dum))[1]])
		if (tdum!='') {
			# was pitstop longer than 1 hour? need to convert to seconds
			if (isgrep("^[0-9]+:[0-9]+\\'",tdum)) tdum=3600*as.numeric(gsub(":.+$",'',tdum)) + 60*as.numeric(gsub("([0-9]+:)([0-9]+)(\\'.+$)",'\\2',tdum)) + as.numeric(gsub("^.+\\'",'',tdum))
			# was pitstop short than one hour but longer than 1 minute? need to convert to seconds
			if (isgrep("^[0-9]+\\'",tdum)) tdum=60*as.numeric(gsub("\\'.+$",'',tdum)) + as.numeric(gsub("^.+\\'",'',tdum))
			pstime[k]=as.numeric(tdum)
		}
		pslap[k]=gsub('</.+$','',dum[rev(grep('</span',dum))[2]])
		myYear = substr(myRace, 1, 4)
		yellowCircleForPitStopPenalty = f1admin::CheckYearAttribute(myYear, 'yellowCircleForPitStopPenalty')
		if (yellowCircleForPitStopPenalty) {
			pspenalty[k]=sum(isgrep('penalty',dum))
		}
	}
	message('NB, pitstop penalty ought to be a logical rather than 1 or 0')
	psdriver = entryDF$driver[match(psforixdriver, entryDF$forixId)]
	tabdat=data.frame(pslap,psdriver,pstime,pspenalty)
	names(tabdat)=c('lap','driver','time','penalty')
	write.csv(file=MakeRaceFile(myRace, 'pitstop_clean.csv'),tabdat,row.names=F)
}

FetchDriverRacePage = function(myRace, entryDF, myHtmlName) {

	### now make the driver pages
	### find out how many drivers there actually are, ignoring practice drivers
	## only want to extract for the drivers who started the race
	keep=which(!is.na(entryDF$startingGrid))
	entryDF=entryDF[keep,]
	dum=paste('0',ifelse(entryDF$number<10,paste('0',entryDF$number,sep=''),entryDF$number),sep='')
	dpageref=paste('http://forix.autosport.com/pa.php?l=0&r=',myHtmlName,dum,'&c=0',sep='')

	### don't know who the drivers will be so can't do meaningful filenames
	dfilename=MakeHtmlRaceFile(myRace, paste0('driverrace/',entryDF$driver,'.htm'))

	### now which of those have we already got
	gotlist=rep(0,length(dpageref))
	for (j in 1:length(dfilename)) gotlist[j]=file.exists(dfilename[j])
	cat('Have got ',sum(gotlist),', ',sum(gotlist==0),' to go...\n',sep='')
	for (j in which(gotlist==0)) visitsave(dpageref[j], thiscomputer, dfilename[j])

	### however, that will download faulty files sometimes, so we can check and delete those now:

	gotlist=correctlist=rep(0,length(dpageref))
	dum=gsub('(^.+\\/)([^\\.]+)(.+$)','\\2',dfilename)
	dforixid=entryDF$forixId[match(dum,entryDF$driver)]
	for (j in 1:length(dfilename)) gotlist[j]=file.exists(dfilename[j])
	for (j in which(gotlist==1)) {
		b=scan(dfilename[j],'',sep='\n',quiet=T)
		### check we've got the right driver downloaded
		if (length(grep(dforixid[j],b))>0) correctlist[j]=1
	}
	sax=which(gotlist==1 & correctlist==0)
	if (length(sax)>0) {
		cat('Have found',length(sax),'faulty pages, about to delete them, make sure you run the visitpage bit of code above for them\n')
		for (j in sax) file.remove(dfilename[j])
	}

	if (FALSE) {
	if (!newVersionIsLive) {
		tofile = dfilename
		fromfile = gsub('/f1/', '/lapbylap/', tofile)
		for (j in 1:length(tofile)) file.copy(fromfile[j], tofile[j], overwrite = TRUE)
	}
	}
}

MapDriverTLAToDriverId = function(TLAVector, myRace) {
	# by default assume that the first three letters are the TLA, but have file that has departures

	entryFile = MakeRaceFile(myRace, 'entryetc.csv')
	entryDF = ReadF1Data(entryFile, 'entryDF')

	driverAbbreviationFile = paste0(USERPATH, 'f1admin/driver-abbreviation.csv')
	driverAbbreviationDF = ReadF1Data(driverAbbreviationFile, 'driverAbbreviation')
	entryDF$surname = with(driverAbbreviationDF, surname[match(entryDF$driver, driver)])

	entryDF$driverTLA = toupper(substr(gsub(' ', '', entryDF$surname), 1, 3))

	allMatched = FALSE
	while(!allMatched) {
		myYear = substr(myRace, 1, 4)
		gapChartAbbreviationFile = MakeYearFile(myYear, 'drivertla-override.csv')
		haveOverride = FALSE
		if (file.exists(gapChartAbbreviationFile)) {
			gapChartAbbreviation = ReadF1Data(gapChartAbbreviationFile, 'gapChartAbbreviation') %>%
									filter(race == myRace)
			if (nrow(gapChartAbbreviation) > 0) {
				haveOverride = TRUE
			}
		}
		# do we have any overrides?
		if (haveOverride) {
			toOverrideIndex = which(entryDF$driver %in% gapChartAbbreviation$driver)
			entryDF$driverTLA[toOverrideIndex] = with(gapChartAbbreviation, TLA[match(entryDF$driver[toOverrideIndex], driver)])
		}

		driverVector = with(entryDF, driver[match(TLAVector, driverTLA)])
		unmatchedTLA = unique(TLAVector[is.na(driverVector)])
		if (length(unmatchedTLA) == 0) {
			allMatched = TRUE
		}
		if (length(unmatchedTLA) > 0) {
			print('Have not matched the following driver\'s abbreviation:')
			print(unmatchedTLA)
			message('You need to add a line into \'', gapChartAbbreviationFile,'\' for them')
			print('Press ENTER when you have done so')
			dum = scan(what = '', quiet = TRUE, nmax = 1)
		}
	}

	return(driverVector)
}

.StripLapTime.ConvertGap=function(dum) {
	### the gap may or may not extend to minutes and even hours
	ddum=sapply(dum,function(x) as.numeric(strsplit(x,split="[:'\\.]")[[1]]))
	tsec=rep(NA,length(dum))
	saxhour=grep(':',dum)
	saxmin=which(!grepl(':',dum) & grepl("'",dum))
	saxsec=which(!grepl("'",dum))
	if (length(saxhour)>0) tsec[saxhour]=sapply(ddum[saxhour],function(x) as.numeric(3600*x[1] + 60*x[2] + x[3] + 0.001*x[4]))
	if (length(saxmin)>0) tsec[saxmin]=sapply(ddum[saxmin],function(x) 60*x[1] + x[2] + 0.001*x[3])
	if (length(saxsec)>0) tsec[saxsec]=as.numeric(sapply(ddum[saxsec],function(x) x[1] + 0.001*x[2]))
	return(tsec)
}
.StripLapTime.ExtractLineInfo=function(myline) {
	splitline=strsplit(myline,split='>')[[1]]
	gline=sort(c(grep("^([0-9]:)*[0-9]+'[0-9]{2}\\.[0-9]{3}",splitline),grep('^[0-9]+\\.[0-9]{3}',splitline)))
	grawtime=gsub('<.+$','',splitline[gline])
	gtime=.StripLapTime.ConvertGap(grawtime)
	### except we want EVERY driver to be cumulative, so do this
	gtime[2:length(gtime)]=gtime[1] + gtime[2:length(gtime)]
	dline=grep('^[[:upper:]]{3}<',splitline)
	dabb=gsub('<.+$','',splitline[dline])
	lineinfo=cbind(dabb,gtime)
	return(lineinfo)
}

.StripLapTime.StripGapsChart = function(myRace) {
	b=scan(MakeHtmlRaceFile(myRace, 'laptime.htm'),'',sep='\n',quiet=T)

	g1=grep('[0-9]+\\.[0-9]{3}',b)
	g2=grep('pa\\.php\\?.+r=[0-9]{11}\\&',b)
	g3=grep("[0-9]+\\'[0-9]+\\.[0-9]{3}",b)

	dataline=b[intersect(g1,intersect(g2,g3))]

	### firstly find the list of all drivers who started the race
	dum=strsplit(dataline[1],split='>')[[1]]
	# occasionally puts some smaller text in, need to get rid of these
	sax=grep('<span class=\"TXT-Small-3\"',dum)
	if (length(sax)>0) dum=dum[-sax]
	gline=c(grep("^[0-9]+'[0-9]{2}\\.[0-9]{3}",dum),grep('^[0-9]+\\.[0-9]{3}',dum))
	alldriv=sort(gsub('<.+$','',dum[gline-2]))

	infolist=purrr::map(dataline,.StripLapTime.ExtractLineInfo)
	### now put that into a grid
	dumf=function(x) {
		dum=rep(NA,length(alldriv))
		dum[match(x[,1],alldriv)]=as.numeric(x[,2])
		return(dum)
	}
	cumltgrid=do.call(rbind, purrr::map(infolist,dumf))
	colnames(cumltgrid)=alldriv
	### now convert to lap times
	ltgrid=apply(cumltgrid,2,function(x) c(x[1],diff(x)))
	ltgrid =as_tibble(ltgrid)
	ltgrid$lap = 1:nrow(ltgrid)

	ltlist = gather(ltgrid, driver, sec, -lap)

	ltlist=ltlist[!is.na(ltlist$sec),]
	ltlist=ltlist[order(ltlist$lap,ltlist$driver),]

	ltlist$driver = MapDriverTLAToDriverId(ltlist$driver, myRace)

	fileout=MakeRaceFile(myRace, 'laptime.csv')
	write.csv(file=fileout,ltlist,row.names = FALSE)
}

.StripLapTime.ConvertArchiveLapTime = function(myRace) {
	archivedLapTimeFile = MakeRaceFile(myRace = myRace, myFile = 'laptime-archived.csv')
	targetLapTimeFile = MakeRaceFile(myRace = myRace, myFile = 'laptime.csv')
	lapTimeDF = read.csv(archivedLapTimeFile, sep = '~', head = FALSE)
	names(lapTimeDF) = c('lap', 'driver', 'sec')
	write_csv(x = lapTimeDF, path = targetLapTimeFile)
}

StripLapTime = function(myRace) {
	myYear = as.numeric(substr(myRace, 1, 4))
	haveGapsChart = f1admin::CheckYearAttribute(myYear, 'haveGapsChart')
	haveArchivedLapTimeCsv = f1admin::CheckYearAttribute(myYear, 'haveArchivedLapTimeCsv')

	if (haveGapsChart) {
		.StripLapTime.StripGapsChart(myRace)
	}
	if (!haveGapsChart & haveArchivedLapTimeCsv) {
		# we have the data but just need to modify a little
		.StripLapTime.ConvertArchiveLapTime(myRace)
	}
}

StripTyreSpeedtrap = function(myRace) {

	### now make the driver pages
	### find out how many drivers there actually are, ignoring practice drivers
	filein=MakeRaceFile(myRace,'entryetc.csv')
	b=read.csv(filein,as.is=T)
	## only want to extract for the drivers who started the race
	keep=which(!is.na(b$classification) & !b$classification%in%c('Not started','Withdrawn'))
	b=b[keep,]

	dfilename=MakeHtmlRaceFile(myRace, paste0('driverrace/',b$driver,'.htm'))
	drivdum=b$driver

	missingTyreInfoFile = paste0(OUTPUTPATH, 'tyre-override.csv')
	missingTyreInfo = ReadF1Data(missingTyreInfoFile, 'tyreOverride')
	missingTList = rep(NA, length(drivdum))
	if (any(missingTyreInfo$race == myRace)) {
		mdum = match(paste(myRace, drivdum), with(missingTyreInfo, paste(race, driver)))
		missingTList[which(!is.na(mdum))] = missingTyreInfo$tlist[mdum[which(!is.na(mdum))]]
	}
	#### so let's work through those
	tyreinfo = NULL
	speedtrapDF = tibble(driver = drivdum, qualifying = NA, race = NA)
	for (j in 1:length(dfilename)) {
		b=scan(dfilename[j],'',sep='\n',quiet=T)
		### but if it is the right page, let's get the info we want, which for now is the speedtrap in Q and the race, and the tyres used
		if (is.na(missingTList[j])) {
			tlist=strsplit(gsub('(^.+Tyres: )([^<]+)(<.+$)','\\2',b[grep('Tyres:',b)]),split=' - ')[[1]]
		}
		if (!is.na(missingTList[j])) {
			tlist = missingTList[j]
		}
		tyreinfo[[j]] = tibble(driver = drivdum[j], stint = 1:length(tlist), tyre = tlist)
		### speedtraps
		dum=grep('speed trap',tolower(b))
		dum2=intersect(grep('>qualifying<',tolower(b)),grep('>race<',tolower(b)))
		### awful procedure, only picks up speedtrap if you got it for both race and qualy
		if (length(dum2) > 0) {
			dum2=dum2[dum2>dum][1]
			dum=tolower(strsplit(b[dum2],split='>')[[1]])
			sdum=c(gsub('<.+$','',dum[grep('qualifying',dum)+8]),gsub('<.+$','',dum[grep('race',dum)+8]))
			### let's paste that all together
			speedtrapDF[j,c('qualifying', 'race')] = sdum
		}
	}
	tyreDF = bind_rows(tyreinfo)

	write.csv(file=MakeRaceFile(myRace, 'tyre-stint.csv'), tyreDF, row.names = FALSE)
	write.csv(file=MakeRaceFile(myRace, 'speedtrap.csv'), speedtrapDF, row.names = FALSE)
	cat('Have processed',myRace,'\n')
}

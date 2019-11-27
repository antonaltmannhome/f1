 # in 'f1' session: lbl = f1laptimelm::MakePredSec(lbl,4); write.csv(file='c:/temp/new.csv', lbl[,c('race','driver','lap', 'isOutlier0', 'mod4PredSec')])
 
 # in lapbylap session:
 
b = read.csv('c:/temp/new.csv',as.is=T)
mdum = match(with(lbl, paste(racename, driver, lap)), with(b, paste(race, driver, lap)))
lbl[,c('newmod4predsec','newisoutlier0')] = b[mdum,c('mod4PredSec','isOutlier0')]


checkrace = function(myracename) {
	#myLbl = lbl %>%
	#		filter(racename==myracename) %>%
	#		select(lap, driver, mod4predsec, newmod4predsec) %>%
	#		gather(model, predsec, -c(lap, driver))
	#ggplot(myLbl %>% filter(!is.na(predsec))) +
	#geom_point(aes(x = lap, y = predsec, col = model), size = 2)
	myLbl = lbl %>%
			filter(racename==myracename & !is.na(mod4predsec) & !is.na(newmod4predsec)) %>%
			mutate(bigdifference = abs(mod4predsec - newmod4predsec) > 0.5,
					outlierdifference = newisoutlier0 != isoutlier0)
	myLbl$driver2 = 'nodifference'
	myLbl = mutate_cond(myLbl,
						bigdifference,
						driver2 = driver)
	ggplot(myLbl) +
		geom_point(aes(x = mod4predsec, y = newmod4predsec, col = driver2, shape = outlierdifference),
					size = 2) +
		geom_abline(intercept = 0, slope = 1)
}


## ## let's patiently work our way through each step of the update, checking we're agreeing race by race

### transfer-data-to-database:
### let's compare rddf, just after writing it to db

oldvsnewcolumnname = read.csv('c:/research/f1/temporary transfer data/old-new-column-name.csv',as.is=T)

oldracedf = sqlQuery2('select * from f1.race')
olddrivdf = sqlQuery2('select * from f1.driver')

myRace = '2017australia'
checkpremodel = function(myRace) {
	mysqlrr = with(oldracedf, sqlrr[which(racename==myRace)])
	newracedrivdf = sqlQuery2(paste0('select * from cleanf1.racedriver where race = "', myRace,'"'))
	oldracedrivdf = sqlQuery2(paste0('select * from f1.racedriv where sqlrr = ', mysqlrr))
	oldracedrivdf$driver = olddrivdf$driver[match(oldracedrivdf$dnum, olddrivdf$dnum)]

	### now rename old columnsto get them into line
	for (j in which(oldvsnewcolumnname$table == 'racedriv')) {
		names(oldracedrivdf)[names(oldracedrivdf) == oldvsnewcolumnname$oldcolumn[j]] = oldvsnewcolumnname$newcolumn[j]
	}
	oldracedrivdf = oldracedrivdf[,names(newracedrivdf)[names(newracedrivdf) %in% names(oldracedrivdf)]]

	newracedrivdf = newracedrivdf %>% arrange(driver)
	oldracedrivdf = oldracedrivdf %>% arrange(driver)

	oldracedrivdf$startingGrid = as.integer(oldracedrivdf$startingGrid)
	newracedrivdf$classification[newracedrivdf$officialFinishingPosition==1]=''
	
	for (k in c('car', 'classification', 'officialFinishingPosition','startingGrid')) {
		print(k)
		print(all.equal(oldracedrivdf[,k], newracedrivdf[,k]))
		# print(cbind(oldracedrivdf[,k], newracedrivdf[,k]))
	}

	#### ok, that was painful, now let's look at the more interesting ones
	newracedrivlapdf = as_data_frame(sqlQuery2(paste0('select * from cleanf1.racedriverlap where race = "', myRace,'"')))
	oldracedrivlapdf = as_data_frame(sqlQuery2(paste0('select * from f1.lapbylap where sqlrr = ', mysqlrr)))
	oldracedrivlapdf$driver = olddrivdf$driver[match(oldracedrivlapdf$dnum, olddrivdf$dnum)]

	oldracedrivlapdf = oldracedrivlapdf %>% arrange(driver, lap)
	newracedrivlapdf = newracedrivlapdf %>% arrange(driver, lap)

	print(all(oldracedrivlapdf$tyre == newracedrivlapdf$tyre))
	print(all(oldracedrivlapdf$sec == newracedrivlapdf$sec))
	print(all(oldracedrivlapdf$inlap == as.numeric(newracedrivlapdf$inlap)))
	print(all(oldracedrivlapdf$outlap == as.numeric(newracedrivlapdf$outlap)))
	print(all(oldracedrivlapdf$isscar == as.numeric(newracedrivlapdf$isSafetyCar)))
	print(all(oldracedrivlapdf$isred == as.numeric(newracedrivlapdf$isRed)))
	print(all(oldracedrivlapdf$iswet == as.numeric(newracedrivlapdf$isWet)))
	print(all(oldracedrivlapdf$stint == newracedrivlapdf$stint))
	print(all(oldracedrivlapdf$tempdidot == newracedrivlapdf$preDeltaDidOt))
	print(all(oldracedrivlapdf$tempgotot == newracedrivlapdf$preDeltaGotOt))
	print(all(oldracedrivlapdf$tempdidlap == newracedrivlapdf$preDeltaDidLap))
	print(all(oldracedrivlapdf$tempgotlap == newracedrivlapdf$preDeltaGotLap))

	# got disagreements? use this to investigate?
	if (FALSE) {
		sax = which(oldracedrivlapdf$tempdidot != newracedrivlapdf$preDeltaDidOt)
		sax = which(oldracedrivlapdf$tempgotot != newracedrivlapdf$preDeltaGotOt)
		sax = which(oldracedrivlapdf$tempdidlap != newracedrivlapdf$preDeltaDidLap)
		sax = which(oldracedrivlapdf$tempgotlap != newracedrivlapdf$preDeltaGotLap)
	
		oldracedrivlapdf[sax,c('driver','lap','tempdidot','tempgotot','tempdidlap','tempgotlap')]
		newracedrivlapdf[sax,c('driver','lap','preDeltaDidOt','preDeltaGotOt','preDeltaDidLap','preDeltaGotLap')]
	}
}

### let's check on the qualifying now
checkpremodel = function(myRace) {
	mysqlrr = with(oldracedf, sqlrr[which(racename==myRace)])
	
	newracedrivdf = sqlQuery2(paste0('select * from cleanf1.racedriver where race = "', myRace,'"'))
	oldracedrivdf = sqlQuery2(paste0('select * from f1.racedriv where sqlrr = ', mysqlrr))
	oldracedrivdf$driver = olddrivdf$driver[match(oldracedrivdf$dnum, olddrivdf$dnum)]

	newqualidf = as_data_frame(sqlQuery2(paste0('select * from cleanf1.qualifying where race = "', myRace,'"')))
	oldqualidf = as_data_frame(sqlQuery2(paste0('select * from f1.qualifying where sqlrr = ', mysqlrr)))
	oldqualidf$driver = olddrivdf$driver[match(oldqualidf$dnum, olddrivdf$dnum)]

	### now rename old columnsto get them into line
	for (j in which(oldvsnewcolumnname$table == 'racedriv')) {
		names(oldracedrivdf)[names(oldracedrivdf) == oldvsnewcolumnname$oldcolumn[j]] = oldvsnewcolumnname$newcolumn[j]
	}
	oldracedrivdf = oldracedrivdf[,names(newracedrivdf)[names(newracedrivdf) %in% names(oldracedrivdf)]]

	### now rename old columnsto get them into line
	for (j in which(oldvsnewcolumnname$table == 'qualifying')) {
		names(oldqualidf)[names(oldqualidf) == oldvsnewcolumnname$oldcolumn[j]] = oldvsnewcolumnname$newcolumn[j]
	}
	oldqualidf = oldqualidf[,names(newqualidf)[names(newqualidf) %in% names(oldqualidf)]]

	oldracedrivdf = oldracedrivdf %>% arrange(driver)
	newracedrivdf = newracedrivdf %>% arrange(driver)

	oldqualidf = oldqualidf %>% arrange(session, driver)
	newqualidf = newqualidf %>% arrange(session, driver)
	
	oldqualidf = oldqualidf %>%
					mutate_cond(!is.na(qualProb) & qualProb < (-98),
								qualProb =  NA)
	newqualidf = newqualidf %>%
					mutate_cond(!is.na(qualProb) & qualProb < (-98),
								qualProb =  NA)
	
	par(mfrow=c(3,2))
	print(all(oldracedrivdf$modQualRawDCoef == newracedrivdf$modQualRawDCoef))
	print(all(oldracedrivdf$modQualPredNValid == newracedrivdf$modQualPredNValid))
	plot(oldracedrivdf$modQualRawDCoef, newracedrivdf$modQualRawDCoef)
	plot(oldracedrivdf$modQualPredNValid, newracedrivdf$modQualPredNValid)
	#dum = askcond(F,T)
	#par(mfrow=c(2,2))
	print(all(oldqualidf$predSec == newqualidf$predSec))
	print(all(oldqualidf$qualProb == newqualidf$qualProb))
	print(all(oldqualidf$qualWgt == newqualidf$qualWgt))
	plot(oldqualidf$predSec, newqualidf$predSec)
	plot(oldqualidf$qualProb, newqualidf$qualProb)
	plot(oldqualidf$qualWgt, newqualidf$qualWgt)
}

### so individual races seem to work, let's check to whole damn lot now
oldrddf = sqlQuery2('select sqlrr, dnum, qualrawdcoef, modqualqualdcoef from f1.racedriv')
oldrddf = lazy_left_join(oldrddf, oldracedf, 'sqlrr', 'racename')
oldrddf = lazy_left_join(oldrddf, olddrivdf, 'dnum', 'driver')

oldrddf$race = oldrddf$racename

rddf = lazy_left_join(rddf, oldrddf, c('race', 'driver'), c('qualrawdcoef', 'modqualqualdcoef'))

rddf %>% filter(abs(as.numeric(qualrawdcoef) - modQualRawDCoef) > 0.1) %>% select(race, driver)

### ok, so the differences detected so far are:
# abu dhabi 2011 is a freak, there's no overalp between q1 triers and triers in the other sessions, so it's very sensitive to a driver marginally being included as having a chance to qualify or not, ie sensitive to the sims

# now try to detect outliers difference


### now let's look for overtaking phase 2 differences

b = read.csv('c:/temp/temp.dat') %>% rename(race = racename)
lbl2 = left_join(lbl, b)
lbl2 %>% filter(year == 2010 &
				!(gotOt==gotot & gotlap == gotLap &
					didOt == didot & didlap == didLap)) %>%
					select(race, driver, lap, gotot, gotOt, didot, didOt, gotlap, gotLap, didlap, didLap)

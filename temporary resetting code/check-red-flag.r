### let's check red flag stuff

# these are the races to check:
lbl %>% filter(isRed) %>% distinct(race) %>% arrange(race)

               # race
              # <chr>
 # 1        2010korea
 # 2       2011canada
 # 3       2011monaco
 # 4     2012malaysia
 # 5       2013monaco
 # 6 2014greatbritain
 # 7        2014japan
 # 8    2016australia
 # 9      2016belgium
# 10       2016brazil
# 11   2017azerbaijan

viewred = function(myrace) {

	# is there just one red flag though? need to loop through all unique ones
	# can't really do this for wet race in old world
	driverSawAllRed = lbl %>%
						filter(racename == myrace) %>%
						group_by(driver) %>%
						summarise(sumred = sum(isred==1)) %>%
						ungroup() %>%
						filter(sumred == max(sumred))
				
	## those drivers saw all red flags, so just pick one and that should give us all we need
	exampleDriver = driverSawAllRed[1,] %>% pull(driver)
				
	redsax = with(lbl, which(racename==myrace & isred==1))
	# but want next lap by each driver too
	redplus1sax = match(with(lbl[redsax,], paste(racename, driver, lap + 1)),
						with(lbl, paste(racename, driver, lap)))
	isredlapdf = lbl[redsax,c('driver', 'lap', 'tyre', 'tlap', 'inlap', 'sec', 'impsec', 'starttelapse')]
	nextlapdf = lbl[redplus1sax,c('tyre', 'tlap', 'outlap', 'sec', 'impsec', 'isscar', 'screstart')] %>%
					dplyr::rename(nltyre = tyre, nltlap = tlap, nlsec = sec, nlscar = isscar, nlimpsec = impsec)
	mydf = cbind(isredlapdf, nextlapdf) %>%
			arrange(starttelapse)
	return(mydf)
}

miniviewred = function(myrace) {

	# we would like an idea of what a fast lap time is to verify safety car, here is fastest qualy time
	fastestqualytime = min(with(qdf[qdf$racename==myrace,],qt))
	message('Fastest qualy time:', fastestqualytime)
	# is there just one red flag though? need to loop through all unique ones
	# can't really do this for wet race in old world
	driverSawAllRed = lbl %>%
						filter(racename == myrace) %>%
						group_by(driver) %>%
						summarise(sumred = sum(isred==1)) %>%
						ungroup() %>%
						filter(sumred == max(sumred))
				
	## those drivers saw all red flags, so just pick one and that should give us all we need
	exampleDriver = driverSawAllRed[1,] %>% pull(driver)
				
	redsax = with(lbl, which(racename==myrace & isred==1))
	# but want next lap by each driver too
	redplus1sax = match(with(lbl[redsax,], paste(racename, driver, lap + 1)),
						with(lbl, paste(racename, driver, lap)))
	isredlapdf = lbl[redsax,c('driver', 'lap', 'tyre', 'tlap', 'inlap', 'sec', 'temptelapse')]
	nextlapdf = lbl[redplus1sax,c('tyre', 'tlap', 'outlap', 'sec', 'isscar', 'screstart')] %>%
					dplyr::rename(nltyre = tyre, nltlap = tlap, nlsec = sec, nlscar = isscar)
	mydf = cbind(isredlapdf, nextlapdf) %>%
			arrange(temptelapse)
	return(mydf)
}

## new world:

lbl =  f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE)
ViewRed = function(myRace) {

	# is there just one red flag though? need to loop through all unique ones
	# can't really do this for wet race in old world
	driverSawAllRed = lbl %>%
						filter(race == myRace) %>%
						group_by(driver) %>%
						summarise(sumRed = sum(isRed)) %>%
						ungroup() %>%
						filter(sumRed == max(sumRed))
				
	## those drivers saw all red flags, so just pick one and that should give us all we need
	exampleDriver = driverSawAllRed[1,] %>% pull(driver)
				
	redIndex = with(lbl, which(race == myRace & isRed))
	# but want next lap by each driver too
	redPlus1Index = match(with(lbl[redIndex,], paste(race, driver, lap + 1)),
						with(lbl, paste(race, driver, lap)))
	isRedLapDF = lbl[redIndex, c('driver', 'lap', 'tyre', 'tyreLap', 'inlap', 'sec', 'impsec', 'startTimeElapsed')]
	nextLapDF = lbl[redPlus1Index,c('tyre', 'tyreLap', 'outlap', 'sec', 'impsec', 'isSCRestart')] %>%
					rename(nltyre = tyre, nltlap = tyreLap, nlsec = sec, nlimpsec = impsec)
	myDF = cbind(isRedLapDF, nextLapDF) %>%
			arrange(startTimeElapsed)
	return(myDF)
}
lbl =  f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = TRUE)
MiniViewRed = function(myRace) {

	# is there just one red flag though? need to loop through all unique ones
	# can't really do this for wet race in old world
	driverSawAllRed = lbl %>%
						filter(race == myRace) %>%
						group_by(driver) %>%
						summarise(sumRed = sum(isRed)) %>%
						ungroup() %>%
						filter(sumRed == max(sumRed))
				
	## those drivers saw all red flags, so just pick one and that should give us all we need
	exampleDriver = driverSawAllRed[1,] %>% pull(driver)
				
	redIndex = with(lbl, which(race == myRace & isRed))
	# but want next lap by each driver too
	redPlus1Index = match(with(lbl[redIndex,], paste(race, driver, lap + 1)),
						with(lbl, paste(race, driver, lap)))
	isRedLapDF = lbl[redIndex, c('driver', 'lap', 'tyre', 'tyreLap', 'inlap', 'sec', 'startTimeElapsed')]
	nextLapDF = lbl[redPlus1Index,c('tyre', 'tyreLap', 'outlap', 'sec', 'isSafetyCar', 'isSCRestart')] %>%
					rename(nltyre = tyre, nltlap = tyreLap, nlscar = isSafetyCar, nlsec = sec)
	myDF = cbind(isRedLapDF, nextLapDF) %>%
			arrange(startTimeElapsed)
	return(myDF)
}

## issues:
# in general: isSCRestart is FALSE for new world
# 2010 korea, 2011 canada: inlap, outlap incorrectly declared for both, although impsec correct for new
# 2011 monaco, 2013monaco: both worlds incorrectly adjusts impsec for outlaps
# 2014 greatbritain: new world actualyl declares SCRestart, but nlimpsec all NA, just weird in general, old world meese up inlap rather than outlap
# 2016australia: old world adjusts magnussen's impsec on inlap (is it also an outlap?), new world does not
# 2016belgium: old world gets impsec horribly wrong.
# 2016belgium: how come everybody has tyreLap = 1? is red flag declared too late?
# 2016brazil: again, everybody has new tyres on lap red flag declared, surely not correct
# 2017azerbaijan: same problem as above

#### further comments: unfortunately it might be necessary to chenge stintdb, or if not change stintdb, then update the inlap and outlap columns of lbl - it might be simplest to just rewrite the database, all the slow things to make are stored on disk i believe:
### isscar, iswet, corrected pitstop, carproblem
### not qualifying outliers, but surely we don't need to regerenate that, or racedb, or rddf for that matter - just lbl and stintdb. how about we drop lbl and stintdb, and just remake all of them?
## but need to see what is causing the problem with belgium 2016, brazil, azerbaijan
## and need to check what's going on with screstart.
## for things like belgium 2016, i don't think there's anything wrong with the isred file, it's rather the process_inout tht is doing something wrong


### update: 2014japan: tlap looks wrong, should be 3, not 1
### 		2016 australia: red flag seems to have been picked up at wrong times
###			2016belgium: also wrong
###			2016brazil: also wrong
###			2017azerbaijan: also wrong


### wher we're up to: we've counted red flags without changing tyres as a new stint in old (world (which i agree with), but not so in new world, which we should change. inlap, outlap, tyre, tlap all agree though, yey
# A tibble: 13 x 3
             race      driver     n
            <chr>       <chr> <int>
 1     2011monaco      asutil     6
 2   2012malaysia     falonso    47
 3   2012malaysia      fmassa    47
 4   2012malaysia     jbutton    47
 5   2012malaysia   lhamilton    47
 6   2012malaysia mschumacher    47
 7   2012malaysia     mwebber    47
 8   2012malaysia     svettel    47
 9     2013monaco    jbianchi    13
10  2016australia  kmagnussen    38
11 2017azerbaijan  kraikkonen    24
12 2017azerbaijan   pwehrlein    28
13 2017azerbaijan      sperez    17

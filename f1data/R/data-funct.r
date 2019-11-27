
#' @export
LoadAllData = function(omitPreDelta = FALSE, raceToOmit = NULL) {

	raceDF = sqlReadTable('race')
	driverDF = sqlReadTable('driver')
	rddf = sqlReadTable('racedriver')
	lbl = sqlReadTable('racedriverlap')
	stintDF = sqlReadTable('stint')
	pitStopDF = sqlReadTable('pitstop')
	raceTyreDF = sqlReadTable('racetyre')
	qdf = sqlReadTable('qualifying')
	qualifyingSessionDF = sqlReadTable('qualifyingsession')

	if (!is.null(raceToOmit)) {
	  # if update crashes half way through it can be a proper nightmare because e.g. GetSmooth won't work. you want to be abel to fix the code without having this extra problem.
	  # so you can effectively make the last race disappear from the data this way
	  raceDF = raceDF %>%
	            filter(!race %in% raceToOmit)
	  rddf = rddf %>%
	          filter(!race %in% raceToOmit)
	  lbl = lbl %>%
	          filter(!race %in% raceToOmit)
    stintDF = stintDF %>%
              filter(!race %in% raceToOmit)
    pitStopDF = pitStopDF %>%
                  filter(!race %in% raceToOmit)
    raceTyreDF = raceTyreDF %>%
                  filter(!race %in% raceToOmit)
    qdf = qdf %>%
          filter(!race %in% raceToOmit)
    qualifyingSessionDF = qualifyingSessionDF %>%
                            filter(!race %in% raceToOmit)
  }
	
	qdf = lazy_left_join(qdf, qualifyingSessionDF, c('race', 'session'))

	nrace = nrow(raceDF)

	raceDF = raceDF %>%
				arrange(date) %>%
				mutate(rr = 1:nrace)

	raceDF$year = as.integer(substr(raceDF$race, 1, 4))

	rddf = lazy_left_join(rddf, raceDF, 'race', c('year', 'rr')) %>%
	arrange(rr)
	lbl = lazy_left_join(lbl, raceDF, 'race', c('year', 'rr')) %>%
	arrange(rr, lap)
	qualifyingSessionDF = lazy_left_join(qualifyingSessionDF, raceDF, 'race', c('year', 'rr')) %>%
	arrange(rr, session)
	qdf = lazy_left_join(qdf, raceDF, 'race', c('year', 'rr')) %>%
	arrange(rr, session)
	stintDF = lazy_left_join(stintDF, raceDF, 'race', c('year', 'rr')) %>%
	arrange(rr, driver, stint)
	raceTyreDF = lazy_left_join(raceTyreDF, raceDF, 'race', c('year', 'rr')) %>%
	arrange(rr)

	unYear = unique(raceDF$year)

	rddf = f1data:::GetTeamName(rddf)
	rddf$driverTeamYear = with(rddf, paste(driver, team, year))
	rddf = rddf %>% select(-car)

	raceDF$daynum = as.numeric(raceDF$date - min(raceDF$date), units = 'days')
	rddf = lazy_left_join(rddf, raceDF, 'race', 'daynum')

	raceDF = f1data:::DisambiguateCircuit(raceDF)

	# no point in having the drivers who weren't on the grid or didn't take part in qualifying
	rddf = indicate_overlapping_combination(rddf,
										qdf,
										c('race', 'driver'),
										'didQualifying')

	rddf = rddf %>% filter(!is.na(startingGrid) | didQualifying)

	lbl$fuel=raceDF$nlap[lbl$rr]-lbl$lap+1

	lbl = f1data:::MakeSCRestart(lbl)

	maxLap = lbl %>%
				group_by(driver, race) %>%
				summarise(maxLap = max(lap))
	rddf = lazy_left_join(rddf, maxLap, c('race', 'driver'))

	# nothing remotely good about the bad lap times with valencia's pit lane line, override asap
	lbl = f1gaptrafficpitstop::CorrectPitLinePositionProblem(lbl)

	carProblemDF = MakeCarProblemDF(lbl, raceDF, rddf)

	tyreDF = ReadF1Data(paste0(USERPATH, 'f1admin/tyre.csv'), 'tyre')

	yearGuideDF = ReadF1Data(paste0(USERPATH, 'f1admin/year-guide.csv'), 'yearGuide')

	if (omitPreDelta) {
		# once you've got the deltas for pit stops, unlikely you need some of the stuff that was used in order to calculate them
		lbl = within(lbl, rm(preDeltaDidOt, preDeltaGotOt, preDeltaDidLap, preDeltaGotLap))
	}

	raceDF = f1data:::ReorderDF(raceDF, 'race')
	lbl = f1data:::ReorderDF(lbl, 'racedriverlap')
	rddf = f1data:::ReorderDF(rddf, 'racedriver')
	qdf = f1data:::ReorderDF(qdf, 'qualifying')
	stintDF = f1data:::ReorderDF(stintDF, 'stint')
	raceTyreDF = f1data:::ReorderDF(raceTyreDF, 'racetyre')

	# then assign globally, not going to mess about returning into a list then assigning each individually
	objectList = c('raceDF', 'driverDF', 'rddf', 'lbl', 'qualifyingSessionDF', 'qdf',
					'pitStopDF', 'stintDF', 'raceTyreDF', 'carProblemDF', 'tyreDF',
					'yearGuideDF',
					'nrace', 'unYear')
	for (j in 1:length(objectList)) {
		assign(objectList[j], get(objectList[j]), envir = globalenv())
		message('Have created object: ',objectList[j],' in the global environment')
	}

	# finally we want any other miscellaneous useful constants
	f1data:::LoadConstant()

	invisible(NULL)
}

LoadConstant = function() {
	wetTyre = c('wet', 'intermediate')
	assign('wetTyre', wetTyre, env = globalenv())

	miscStat = list(model30ClearLapOutlierCutoff = 2,
					model30GotOvertakenOnceOutlierCutoff = 4,
					model30GotOvertakenGTOnceOutlierCutoff = 7)
	assign('miscStat', miscStat, env = globalenv())
}

LoadOvertakingDF = function() {
  # this might be a little slow to doevery time you want to load in any data so cordoned off. but maybe will move it into LoadAllData
  overtakingDF = sqlReadTable('overtaking')
  return(overtakingDF)
}

LoadPossibleOvertakingDF = function() {
	# this might be a little slow to doevery time you want to load in any data so cordoned off. but maybe will move it into LoadAllData
	possibleOvertakingDF = sqlReadTable('possibleovertaking')
	return(possibleOvertakingDF)
}

GetTeamName = function(rddf) {
	rddf$team = gsub('(^[a-z ]+)( .+$)', '\\1', rddf$car)
	rddf$team = gsub('mercedes mgp', 'mercedes', rddf$team)
	# for when we want to link season together, we need to know when the team name has changed - also, this is useful for if you can't remember what a team was known as when e.g when renault was lotus, lotus was caterham etc
	teamNameMapDF = ReadF1Data('data/team-name-map.csv', 'teamNameMapDF')
	rddf$aaTeam = rddf$team # except...
	for (ti in 1:nrow(teamNameMapDF)) {
		toChangeIndex = with(rddf, which(team == teamNameMapDF$team[ti] &
																			between(year, teamNameMapDF$startYear[ti], teamNameMapDF$endYear[ti])))
		rddf$aaTeam[toChangeIndex] = teamNameMapDF$aaTeam[ti]
	}
	return(rddf)
}

MakeSCRestart = function(lbl) {
	lbl = lbl %>%
			group_by(race, driver) %>%
			arrange(lap) %>%
			mutate(prevLapIsSafetyCar = lag(isSafetyCar)) %>%
			mutate_cond(lap == 1, prevLapIsSafetyCar = FALSE) %>%
			mutate(isSCRestart = !isSafetyCar & prevLapIsSafetyCar) %>%
			ungroup() %>%
			select(-prevLapIsSafetyCar)
	return(lbl)
}

DetectWetTyreOnDryTrack = function(lbl) {
	### a function only needed due to Vergne doing this in Monaco 2012
	lbl$isWetTyreOnDryTrack = with(lbl, !isWet & tyre %in% wetTyre)
	return(lbl)
}

.MakeDriverRaceCarProblemDF = function(lap, isCarProblem) {
	rleOutput = rle(isCarProblem)
	rleLength = rleOutput$lengths
	fullCarProblemDF = tibble(startLapIndex = c(1,lag(cumsum(rleLength))[-1]+1),
								endLapIndex = cumsum(rleLength),
								startLap = lap[startLapIndex],
								endLap = lap[endLapIndex],
								carProblemInd = rleOutput$values)
	carProblemDF = fullCarProblemDF %>%
					filter(carProblemInd == 1) %>%
					select(-carProblemInd)
	return(carProblemDF)
}

MakeCarProblemDF = function(lbl, raceDF, rddf) {

	carProblemDF = lbl %>%
				filter(isCarProblem) %>%
				group_by(rr, driver) %>%
				do(.MakeDriverRaceCarProblemDF(.$lap, .$isCarProblem)) %>%
				ungroup()
	# problem is we can't get hold of the reason like this, which might not matter
	carProblemDF = lazy_left_join(carProblemDF, raceDF, 'rr', 'race')
	# if i hadn't done htat it'd be in alphabetical race order, not rr, v annoying
	carProblemDF = lazy_left_join(carProblemDF, rddf, c('race', 'driver'), 'maxLap')
	carProblemDF = carProblemDF %>%
					mutate(isWholeRace = startLap == 1 & endLap == maxLap)

	carProblemDF = carProblemDF %>%
					select(race, driver, startLap, endLap, isWholeRace)

	return(carProblemDF)
}

ViewDriverComment = function(myRace, myDriv, runMode = 'view') {

	filein=MakeHtmlRaceFile(myRace = myRace,
							myFile = paste0('driverrace/',myDriv,'.htm'))
	b=scan(filein,sep='\n','',quiet=T)
	### format seems to change late july 2017
	havecommentstatus = NULL
	if (file.info(filein)$mtime < '2017-08-01') {
		rline=grep('Race:',b)
		if (length(rline)==0) {
			drivercomment='no driver comments'
			havecommentstatus = FALSE
		}
		if (length(rline)>0) {
			b[rline]=gsub('<BR>','',b[rline])
			dcomm=gsub('(^.+Race:.+Standard-3">)([^<]+)(<.+$)','\\2',b[rline])
			### but that's horrible having it all on one long line, so split into lines
			drivercomment=splittext(dcomm)
			havecommentstatus = TRUE
		}
	}
	if (file.info(filein)$mtime > '2017-08-01') {
		rstartline=grep('Race:',b)
		if (length(rstartline)==0) {
			drivercomment='no driver comments'
			havecommentstatus = FALSE
		}
		if (length(rstartline)>0) {
			dum = grep('<span class=\"TXT-Small-3\">',b)
			rcloseline = min(dum[dum>=rstartline])
			messycomment1=b[rstartline:rcloseline]
			### merge into single line
			messycomment2 = paste(messycomment1, collapse='')
			cleancomment = gsub('(^.+Standard-3\">)([^<]+)(<.+$)','\\2',messycomment2)
			drivercomment=splittext(cleancomment)
			havecommentstatus = TRUE
		}
	}
	if (runMode == 'view') toreturn = drivercomment
	if (runMode == 'check') toreturn = havecommentstatus
	return(toreturn)
}

DisambiguateCircuit = function(raceDF) {
	circuitDisambiguation = ReadF1Data(paste0(USERPATH, 'f1admin/circuit-disambiguation.csv'), 'circuitDisambiguation')
	for (ci in 1:nrow(circuitDisambiguation)) {
		thisCircuitIndex = which(raceDF$circuit == circuitDisambiguation$currentName[ci] &
								between(raceDF$year,
										circuitDisambiguation$startYear[ci],
										circuitDisambiguation$endYear[ci]))
		raceDF$circuit[thisCircuitIndex] = circuitDisambiguation$newName[ci]
	}

	return(raceDF)
}

MakePrettyRaceLabel=function(raceDF) {
	prettyNameMap = ReadF1Data(paste0(USERPATH, 'f1admin/pretty-name.csv'), 'prettyName')
	raceDF$country = gsub('^[0-9]{4}', '', raceDF$race)

	countryPrettyNameMap = prettyNameMap %>% filter(type == 'country')
	raceDF$prettyCountry = with(countryPrettyNameMap, prettyName[match(raceDF$country, codeName)])
	if (any(is.na(raceDF$prettyCountry))) {
		message('Need to add information to \'f1admin/pretty-name.csv\' for these countries:')
		print(raceDF %>% filter(is.na(prettyCountry)) %>% pull(country))
	}

	circuitPrettyNameMap = prettyNameMap %>% filter(type == 'circuit')
	raceDF$prettyCircuit = with(circuitPrettyNameMap, prettyName[match(raceDF$circuit, codeName)])
	if (any(is.na(raceDF$prettyCircuit))) {
		message('Need to add information to \'f1admin/pretty-name.csv\' for these circuits:')
		print(raceDF %>% filter(is.na(prettyCircuit)) %>% pull(circuit))
	}

	raceDF$prettyRace = with(raceDF, paste(year, prettyCountry))

	return(raceDF)
}

DetectRetirement = function(rddf) {
	rddf = rddf %>%
		mutate(isTime = grepl('[0-9]{1,2}\\.[0-9]{1,3}', classification),
				isLappedAtFinish = grepl('[0-9] Lap', classification),
				isRetirement = !(!is.na(officialFinishingPosition) &
								officialFinishingPosition == 1) &
								!isTime &
								!isLappedAtFinish)
	return(rddf$isRetirement)
}

ReorderDF = function(myDF, myDBDFName) {
	# nice to have columns that are part of the model in model order
	# the other ones like year, rr etc, they should go at the front
	modelColumnGuideFile = paste0(USERPATH, 'f1admin/model-column-guide.csv')
	modelColumnDetail = ReadF1Data(modelColumnGuideFile, 'modelColumnDetail') %>%
											filter(table == myDBDFName)

	inRDFName = intersect(modelColumnDetail$column, names(myDF))
	notInModelColumnDetailName = setdiff(names(myDF), modelColumnDetail$column)
	myDF = myDF[,c(notInModelColumnDetailName, inRDFName)]

	return(myDF)
}

MakeModelToUseName = function(myModelStage, toTruncate) {
	## really ugly code, let's move it here

	modelColumnGuideFile = paste0(USERPATH, 'f1admin/model-column-guide.csv')
	modelColumnDetail = ReadF1Data(modelColumnGuideFile, 'modelColumnDetail') %>%
						filter(modelStage == myModelStage) %>%
						select(table, column) %>%
						mutate(label = gsub(toTruncate, '', column))
	modelLabel = tibble_to_list(modelColumnDetail, 'column', 'label')

	return(modelLabel)
}

RenameModelColumn = function(modelLabel, myDF) {
	for (mi in 1:length(modelLabel)) {
		names(myDF)[names(myDF) == names(modelLabel)[[mi]]] = as.character(modelLabel[[mi]])
	}
	return(myDF)
}

ViewLap = function(myRace,lapNumber, addColumn = NULL) {
  ### what was the running order during a lap? this function tells you
  ### but allow possibility that a driver is on the verge of being lapped - useful to know this so indicate

  extraSecondsBuffer = 10
  maxNumLap = with(raceDF, nlap[race == myRace])
  if (lapNumber > maxNumLap) {
    stop('There were only ', maxNumLap,' laps in the race!\n')
  }
  if (lapNumber < maxNumLap) {
    timeElapsedRange = lbl %>%
                        filter(race == myRace & startRank == 1 & lap %in% c(lapNumber, lapNumber + 1)) %>%
                        summarise(lowerTimeCutoff = min(startTimeElapsed) - extraSecondsBuffer,
                                  currentLapLowerTimeCutoff = min(startTimeElapsed) - 0.001,
                                  currentLapUpperTimeCutoff = max(startTimeElapsed) - 0.001,
                                  upperTimeCutoff = max(startTimeElapsed) + extraSecondsBuffer)
  }
  if (lapNumber == maxNumLap) {
    timeElapsedRange = lbl %>%
                        filter(race == myRace & lap == lapNumber) %>%
                        summarise(lowerTimeCutoff = min(startTimeElapsed) - extraSecondsBuffer,
                                  currentLapLowerTimeCutoff = min(startTimeElapsed) - 0.001,
                                  currentLapUpperTimeCutoff = max(endTimeElapsed) + 0.001,
                                  upperTimeCutoff = max(endTimeElapsed) + 0.001)
  }

  lbl = lbl %>%
            mutate(onRelevantLap = between(startTimeElapsed,
                                            timeElapsedRange$currentLapLowerTimeCutoff,
                                            timeElapsedRange$currentLapUpperTimeCutoff),
                   onPreviousLap = between(startTimeElapsed,
                                           timeElapsedRange$lowerTimeCutoff,
                                           timeElapsedRange$currentLapLowerTimeCutoff),
                    onNextLap = between(startTimeElapsed,
                                        timeElapsedRange$currentLapUpperTimeCutoff,
                                        timeElapsedRange$upperTimeCutoff))

  relevantColumn = c('driver', 'startTimeElapsed', 'lap', 'startRank', 'mod30PredSec', 'impsec', 'endTimeElapsed','inlap','outlap')
  if (!is.null(addColumn)) relevantColumn = c(relevantColumn, addColumn)
  lapStatusColumn = c('onRelevantLap', 'onNextLap', 'onPreviousLap')

  currentLapDF = lbl %>%
            filter(race == myRace & (onRelevantLap | onNextLap | onPreviousLap)) %>%
            select(c(relevantColumn, lapStatusColumn)) %>%
            arrange(startTimeElapsed)

  # so that's the data, but would be useful to insert asterisks where it overflows between laps
  # which is bloody fiddly unfortunately
  adjustedRowIndex = 1:nrow(currentLapDF)
  if (any(currentLapDF$onPreviousLap)) {
    finalPreviousLapIndex = max(which(currentLapDF$onPreviousLap))
    toAugment = which(adjustedRowIndex > finalPreviousLapIndex)
    adjustedRowIndex[toAugment] = adjustedRowIndex[toAugment] + 1
  }
  if (any(currentLapDF$onNextLap)) {
    firstNextLapIndex = min(which(currentLapDF$onNextLap))
    toAugment = which(adjustedRowIndex > firstNextLapIndex)
    adjustedRowIndex[toAugment] = adjustedRowIndex[toAugment] + 1
  }
  asteriskedCurrentLapDF = bind_rows(currentLapDF,
                              slice(currentLapDF, as.numeric(any(currentLapDF$onPreviousLap))),
                             slice(currentLapDF, as.numeric(any(currentLapDF$onNextLap))))
  asteriskedCurrentLapDF[setdiff(1:nrow(asteriskedCurrentLapDF), adjustedRowIndex),] = '*'
  asteriskedCurrentLapDF[adjustedRowIndex,] = currentLapDF
  asteriskedCurrentLapDF = remove_column(asteriskedCurrentLapDF, lapStatusColumn)

  return(asteriskedCurrentLapDF)
}

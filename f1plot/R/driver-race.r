
.DriverRaceMakeColourScheme = function(myRace, myDriv) {

	myteam=rddf$team[with(rddf,which(driver==myDriv & race==myRace))]
	myYear=rddf$year[with(rddf,which(driver==myDriv & race==myRace))]

	colourdf=ReadF1Data('data/team-colour.csv', 'teamColour')

	colourScheme = colourdf[with(colourdf, which(year==myYear & team==myteam)),]
	colourScheme$carproblem = colourScheme$outlier

	if (colourScheme$scheme == 'dark') {
		colourScheme$fill = 'black'
		colourScheme$outline = 'white'
		colourScheme$gridcolour = 'grey20' # almost black
		colourScheme$line = 'white'
		colourScheme$stintLabel = 'grey50'
		colourScheme$tyreLabelBox = 'lightgoldenrod1'
		colourScheme$tyreLabelText = 'black'
	}
	if (colourScheme$scheme == 'light') {
		colourScheme$fill = 'white'
		colourScheme$outline = 'black'
		colourScheme$gridcolour = 'grey80' # almost white
		colourScheme$line = 'black'
		colourScheme$stintLabel = 'grey50'
		colourScheme$tyreLabelBox = 'grey25'
		colourScheme$tyreLabelText = 'white'
	}

	return(colourScheme)
}

.DriverRaceGetDoubleColourScheme = function(plotConstant = plotConstant) {
	colourScheme = tibble(
					driver1Fill = 'orangered',
					driver2Fill = 'seagreen',
					outline = 'black',
					panel = plotConstant$doublePanelBackground,
					corrected = 'deepskyblue',
					blocked = 'black',
					carproblem = 'red'
					)
	return(colourScheme)
}

.DriverRaceGetTheme = function(doublePlot, colourScheme = NULL, plotConstant = NULL) {

	gridcolour=element_line(colour=colourScheme$gridcolour)
	titleformat=element_text(size=15, face="bold", vjust=1)
	axiscolour=c('white','white')
	axislabelsize=c(13,13)
	legendsize=11
	legendtitlesize=12
	fontchoice='Trebuchet MS'
	if (!doublePlot) {
		panelbackground=element_rect(fill = colourScheme$panel)
		legendbackground=element_rect(fill = colourScheme$panel)
	}
	if (doublePlot) {
		panelbackground = element_rect(fill = plotConstant$doublePanelBackground)
		legendbackground=element_rect(fill = plotConstant$doublePanelBackground)
	}

	myTheme = theme(
				plot.background = element_rect(fill = 'black'),
				text=element_text(family=fontchoice, colour='white'),
				panel.background = panelbackground,
				plot.title=titleformat,
				legend.background=element_rect('black'),
				legend.key=legendbackground,
				legend.text=element_text(size=legendsize),
				legend.title=element_text(size=legendtitlesize),
				axis.text.x=element_text(colour="white"),
				axis.text.y=element_text(colour="white"),
				panel.grid.major=gridcolour,
				panel.grid.minor=gridcolour,
				axis.title.x=element_text(color=axiscolour[1], size=axislabelsize[1]),
				axis.title.y=element_text(color=axiscolour[2], size=axislabelsize[2])
				)
	return(myTheme)
}

.DriverRaceGetPlotConstant = function() {
	plotConstant = list(
		adjustedSecShape = 21,
		pointSize = 3,
		pitStopLineType = 2,
		pitStopLineSize = 1,
		averageSecLineSize = 1,
		meanAdjustedLineType = 1,
		adjSecLabel = 'fuel and tyre\ncorrected\nlap time\n',
		averageAdjSecLabel = 'average of fuel\nand tyre corrected\nlap times',
		blockedLapShape = 0,
		blockedLapBoxSize = 5,
		blockedLapLabel = 'blocked\n(excluded\nfrom analysis)',
		singleCarProblemLabel = '\ndamaged car\n(excluded\nfrom analysis)',
		doubleCarProblemLabel = 'damaged car\n(excluded\nfrom analysis)',
		doublePanelBackground = 'grey95',
		driverOutline = 'black',
		fuelTyreEffectPanelBackground = 'grey95',
		fuelTyreEffectDriver1Colour = 'orangered',
		fuelTyreEffectDriver2Colour = 'seagreen',
		fuelTyreEffectFontChoice = 'Trebuchet MS'
	)

	return(plotConstant)
}

.DriverRaceMakePlotTitle = function(myRace, myDriv1, myDriv2 = NULL, raceDF) {
	ylabel = ylab('lap time(seconds)')
	myPrettyRaceName = with(raceDF, prettyRace[race == myRace])
	myLongDriverName1 = with(driverDF, longDriver[driver == myDriv1])

	maintitle = paste0('lap times, ',myPrettyRaceName,', ')

	if (is.null(myDriv2)) {
		maintitle = paste0(maintitle, myLongDriverName1)
	}

	if (!is.null(myDriv2)) {
		mySurname1 = with(driverDF, adjustedSurname[driver == myDriv1])
		mySurname2 = with(driverDF, adjustedSurname[driver == myDriv2])
		maintitle = paste(maintitle, mySurname1, 'and',	mySurname2)
	}

	maintitle = ggtitle(maintitle)

	return(list(maintitle = maintitle,
				ylabel = ylabel))
}

GetDriverTLA = function(myDrivList) {
	mySurname = with(driverDF, adjustedSurname[match(myDrivList, driver)])
	myTLA = substr(toupper(mySurname), 1, 3)
	return(myTLA)
}

.DriverRaceGetTyreShape = function(myLbl, tyreDF) {

	myDryTyre = intersect(unique(myLbl$tyre), tyreDF$tyre)
	tyreShape = c(21, 22, 24)

	myHardness = with(tyreDF, hardness[match(myDryTyre, tyre)])
	myTyreShape = tyreShape[rank(myHardness)]
	names(myTyreShape) = myDryTyre

	return(myTyreShape)
}

.DriverRaceMakeInlapDF = function(myRace, myDriv1, myDriv2 = NULL) {
	# finally add on the pit lap indicators
	inlapDF = pitStopDF %>%
				filter(race == myRace & replaceTyre & driver %in% c(myDriv1, myDriv2)) %>%
				select(driver, endOfLap) %>%
				mutate(leglabel = 'pit stop')
				# but there's an edge case, where there's two drivers and they stop at the same time. need to tweak the data to ensure both lines are visible
	doubleStopLap = inlapDF %>%
					count(endOfLap) %>%
					filter(n == 2)
	if (nrow(doubleStopLap) > 0) {
		inlapDF = indicate_overlapping_combination(
						inlapDF,
						doubleStopLap,
						'endOfLap',
						'doubleStopLap')
		inlapDF = inlapDF %>%
					mutate_cond(doubleStopLap,
								endOfLap = endOfLap + c(-0.2, 0.2)) %>%
					select(-doubleStopLap)
	}

	return(inlapDF)
}

.DriverRaceInitialiseData = function(lbl) {

	lbl = f1laptimelm::MakePredSec(lbl, 30, includeIntermediateColumn = TRUE) %>%
			mutate(isCarProblemOutlier =
					isCarProblem & (sec > mod30PredSec + miscStat$model30ClearLapOutlierCutoff),
					fuelTyreAdjustedSec = sec - fuelTyreEffect)

	lbl = f1validity::MakeIsRogue(lbl)
	lbl = f1validity::MakeIsOutlier(30, lbl)
	# that removes fuela dn tyre effect which is annoying, will fix later, but for now, run this again
	message('remove calling of CalculateFuelTyreEffect form DriverRaceInitialiseData once intermediate column problem is fixed')
	lbl = f1laptimelm:::CalculateFuelTyreEffect(lbl, 30, includeIntermediateColumn = TRUE)
	lbl = indicate_overlapping_combination(lbl,
								carProblemDF %>%
									filter(isWholeRace),
								c('race', 'driver'),
								'isWholeRaceCarProblem')
	# nasty hack but we override the car problem column if it was for whole race
	lbl = lbl %>%
			mutate(hackedIsCarProblem = ifelse(isWholeRaceCarProblem, FALSE, isCarProblem))
	lbl = lbl  %>%
			mutate(clearBlockedCarProblem =
					case_when(!hackedIsCarProblem & isGood30 ~ 'clear',
								!hackedIsCarProblem & !isGood30 ~ 'blocked',
								hackedIsCarProblem ~ 'carproblem'))

	assign('lblForDR', lbl, envir = globalenv())
}

#' @export
DriverRacePlot = function(myRace, myDriv1, myDriv2 = NULL, includeAdjustedTime = FALSE, toFile = FALSE) {

	### the various flavours of this are:
	### 1 raw times
	### 2 times with major outliers gone, blocked laps highlighted, tyres displayed
	### 3 (2) but for two drivers
	### 4 (2) but with adjusted times overlaid
	### 5 (3) but with adjusted times overlaid , but disdplayed side by side
	# gets a bit fiddly here, need to have blue shapes for actual times, but black somethings for adjusted times

	raceDF = f1data:::MakePrettyRaceLabel(raceDF)
	if (!exists('lblForDR')) {
		lblForDR = f1plot:::.DriverRaceInitialiseData(lbl)
	}

	myLbl = lblForDR %>%
			filter(race == myRace &
					driver %in% c(myDriv1, myDriv2) &
					!isRogue &
					!isOutlier30 &
					!isCarProblemOutlier)

	plotConstant = f1plot:::.DriverRaceGetPlotConstant()

	if (is.null(myDriv2)) doublePlot = FALSE
	if (!is.null(myDriv2)) doublePlot = TRUE

	if (includeAdjustedTime) {
		myVertLbl = myLbl %>%
					select(driver, lap, sec, tyre, fuelTyreAdjustedSec, hackedIsCarProblem) %>%
					tidyr::gather(realAdjusted, sec, -c(lap, tyre, driver, hackedIsCarProblem)) %>%
					mutate(tyreAdjustedColumn =
							ifelse(realAdjusted == 'sec', tyre, plotConstant$adjSecLabel))
	}
	if (!includeAdjustedTime) {
		myVertLbl = myLbl %>%
					select(driver, lap, sec, tyre) %>%
					mutate(realAdjusted = 'sec')
	}
	myVertLbl = lazy_left_join(myVertLbl, myLbl, c('driver', 'lap'), 'clearBlockedCarProblem')

	plotTitle = f1plot:::.DriverRaceMakePlotTitle(myRace, myDriv1, myDriv2, raceDF = raceDF)

	if (!doublePlot) {
		colourScheme = f1plot:::.DriverRaceMakeColourScheme(myRace, myDriv1)
		myTheme = f1plot:::.DriverRaceGetTheme(FALSE, colourScheme = colourScheme)
	}
	if (doublePlot) {
		colourScheme = f1plot:::.DriverRaceGetDoubleColourScheme(plotConstant = plotConstant)
		myTheme = f1plot:::.DriverRaceGetTheme(TRUE, plotConstant = plotConstant)
		myTheme$legend.position = 'bottom'
	}
	myTheme$legend.title = element_blank()

	myXLim = c(1,with(raceDF,nlap[race == myRace]))

	myTyre = unique(myLbl$tyre)
	myTyreShape = f1plot:::.DriverRaceGetTyreShape(myLbl, tyreDF)

	myPitStopLineType = c('pit stop' = plotConstant$pitStopLineType)

	if (!doublePlot) {
		# NB crappy hack, the single plot squashes the legend labels together, hence this
		myBlockedLapCarProblemLabel = with(plotConstant, c(blockedLapLabel, singleCarProblemLabel))
	}
	if (doublePlot) {
		myBlockedLapCarProblemLabel = with(plotConstant, c(blockedLapLabel, doubleCarProblemLabel))
	}
	names(myBlockedLapCarProblemLabel) = c('blocked', 'carproblem')
	myBlockedLapCarProblemColour = with(colourScheme, c(blocked, carproblem))
	names(myBlockedLapCarProblemColour) = c('blocked', 'carproblem')

	if (doublePlot) {

		myVertLbl$driverTyre = with(myVertLbl, paste(driver, tyre, sep = '-'))

		myDriverFillDF = tibble(driver = c(myDriv1, myDriv2),
									fill = with(colourScheme,
												c(driver1Fill, driver2Fill)))

		# NB want all of 1st driver's tyres to appear in one column, all of second's to appear in second column, this fudge does that
		myDriverTyreDF = myVertLbl %>%
					distinct(driver, tyre) %>%
					mutate(isInData = TRUE) %>%
					complete(driver, tyre, fill = list(isInData = FALSE)) %>%
					arrange(driver, tyre) %>%
					mutate(driverTyre = factor(paste(driver, tyre, sep = '-')),
							TLATyre = paste(GetDriverTLA(driver), tyre, sep = '-')) %>%
					left_join(myDriverFillDF, by = 'driver') %>%
					mutate_cond(!isInData, fill = plotConstant$doublePanelBackground)
		myDriverTyreDF$shape = as.numeric(myTyreShape)[match(myDriverTyreDF$tyre, names(myTyreShape))]
		myDriverTyreDF$size = ifelse(myDriverTyreDF$isInData, plotConstant$pointSize, 0)

		# need to put in missing levels into data to get alignment correct in data
		myVertLbl$driverTyre = factor(myVertLbl$driverTyre, levels = myDriverTyreDF$driverTyre)
		myVertLbl = lazy_left_join(myVertLbl, myDriverTyreDF, 'driverTyre', 'isInData')

		nDistinctTyre = length(unique(myDriverTyreDF$tyre))

		# can't use that directly though, need to extract named lists
		myDriverTyreFill = myDriverTyreDF$fill
		names(myDriverTyreFill) = myDriverTyreDF$driverTyre

		myDriverTyreShape = myDriverTyreDF$shape
		names(myDriverTyreShape) = myDriverTyreDF$driverTyre

		myDriverTyreSize = myDriverTyreDF$size
		names(myDriverTyreSize) = myDriverTyreDF$driverTyre

		myDriverTyreLabel = myDriverTyreDF$TLATyre
		names(myDriverTyreLabel) = myDriverTyreDF$driverTyre

		missingDriverTyre = with(myDriverTyreDF, which(!isInData[match(myDriverTyreLabel, TLATyre)]))
		if (length(missingDriverTyre) > 0) {
			.RepeatingSpace = function(x) paste(rep(' ', x), collapse = '')
			myDriverTyreLabel[missingDriverTyre] =
				purrr:::map_chr(seq_len(length(missingDriverTyre)), .RepeatingSpace)
		}
	}

	if (includeAdjustedTime) {
		myTyreAdjustedColumnShape = c(myTyreShape, plotConstant$adjustedSecShape)
		names(myTyreAdjustedColumnShape)[length(myTyreAdjustedColumnShape)] = plotConstant$adjSecLabel

		if (!doublePlot) {
			myTyreAdjustedColumnColour = c(rep(colourScheme$fill, length(myTyreShape)), colourScheme$corrected)
			names(myTyreAdjustedColumnColour) = c(names(myTyreShape), plotConstant$adjSecLabel)
		}
		if (doublePlot) {
			myTyreAdjustedColumnColour = c(rep(colourScheme$fill, length(myTyreShape)), colourScheme$corrected)
			names(myTyreAdjustedColumnColour) = c(names(myTyreShape), plotConstant$adjSecLabel)
		}

		dum = with(rddf, mod30RawDCoef[race == myRace & driver == myDriv1])
		meanAdjustedSec = tibble(meanAdjustedSec = dum) %>%
						mutate(leglabel = plotConstant$averageAdjSecLabel)
		meanAdjustedSecLineType = plotConstant$meanAdjustedLineType
		names(meanAdjustedSecLineType) = plotConstant$averageAdjSecLabel
	}

	inlapDF = f1plot:::.DriverRaceMakeInlapDF(myRace, myDriv1, myDriv2)

	myDrivPlot = ggplot(myVertLbl) +
					myTheme +
					plotTitle$maintitle +
					plotTitle$ylabel +
					xlim(myXLim)

	if (!includeAdjustedTime & !doublePlot) {
		myDrivPlot = myDrivPlot +
					geom_point(data = myVertLbl,
								aes(x = lap, y = sec, shape = tyre),
								fill = colourScheme$fill,
								col = colourScheme$outline,
								size = plotConstant$pointSize)  +
					scale_shape_manual(values = myTyreShape) +
					geom_vline(data = inlapDF,
							aes(xintercept = endOfLap,
								linetype = leglabel),
								col = colourScheme$outline,
								size = plotConstant$pitStopLineSize)  +
					scale_linetype_manual(values = myPitStopLineType)
	}

	if (includeAdjustedTime & !doublePlot) {
		myDrivPlot = myDrivPlot +
					geom_point(data = myVertLbl %>%
								filter(realAdjusted == 'sec' |
										(realAdjusted == 'fuelTyreAdjustedSec' &
											clearBlockedCarProblem == 'clear')),
									aes(x = lap, y = sec,
									fill = tyreAdjustedColumn,
									shape = tyreAdjustedColumn),
								col = colourScheme$outline,
								size = plotConstant$pointSize)  +
					scale_shape_manual(values = myTyreAdjustedColumnShape)  +
					scale_fill_manual(values = myTyreAdjustedColumnColour) +
					geom_vline(data = inlapDF,
							aes(xintercept = endOfLap),
								col = colourScheme$outline,
								linetype = plotConstant$pitStopLineType,
								size = plotConstant$pitStopLineSize)

		# then we'll add on the average
		myDrivPlot = myDrivPlot +
					geom_hline(data = meanAdjustedSec,
							aes(yintercept = meanAdjustedSec, linetype = leglabel),
								size = plotConstant$averageSecLineSize,
								col = colourScheme$corrected) +
							scale_linetype_manual(values = meanAdjustedSecLineType)
	}

	if (!includeAdjustedTime & doublePlot) {
		myDrivPlot = myDrivPlot +
					geom_point(data = myVertLbl,
								aes(x = lap, y = sec,
									shape = driverTyre,
									fill = driverTyre,
									size = driverTyre),
								col = colourScheme$outline) +
					scale_shape_manual(labels = myDriverTyreLabel,
										values = myDriverTyreShape,
										drop = FALSE,
										guide = guide_legend(nrow = nDistinctTyre, ncol = 2)) +
					scale_fill_manual(labels = myDriverTyreLabel,
										values = myDriverTyreFill,
										drop = FALSE,
										guide = guide_legend(nrow = nDistinctTyre, ncol = 2)) +
					scale_size_manual(labels = myDriverTyreLabel,
										values = myDriverTyreSize,
										drop = FALSE,
										guide = guide_legend(nrow = nDistinctTyre, ncol = 2)) +
					geom_vline(data = inlapDF %>%
								filter(driver == myDriv1),
							aes(xintercept = endOfLap,
								linetype = leglabel),
								col = colourScheme$driver1Fill,
								size = plotConstant$pitStopLineSize)  +
							scale_linetype_manual(values = myPitStopLineType) +
					geom_vline(data = inlapDF %>%
								filter(driver == myDriv2),
							aes(xintercept = endOfLap),
								linetype = plotConstant$pitStopLineType,
								col = colourScheme$driver2Fill,
								size = plotConstant$pitStopLineSize) +
					guides(linetype = guide_legend(override.aes = list(colour = 'black')))
	}

	### add the blocked lap indicators in
	myDrivPlot = myDrivPlot +
				geom_point(
					data = myVertLbl %>%
							filter(clearBlockedCarProblem %in% c('blocked', 'carproblem') &
									realAdjusted == 'sec'),
					aes(x = lap, y = sec, colour = clearBlockedCarProblem),
						shape = plotConstant$blockedLapShape,
						size = plotConstant$blockedLapBoxSize) +
					scale_colour_manual(values = myBlockedLapCarProblemColour,
										labels = myBlockedLapCarProblemLabel)

	### this feels like a massive function - can it be made any shorter?
	### not sure it can, you'd have to have so many lists which would make it hard to read

	if (toFile) {
		if (!doublePlot) {
			fileout = MakeRaceFile(myRace=myRace, myFile=paste0(myDriv1, '.png'))
		}
		if (doublePlot) {
			fileout = MakeRaceFile(myRace=myRace, myFile=paste0(myDriv1, '-', myDriv2, '.png'))
		}
		aafunct::NiceGgSave(myPlot = myDrivPlot, myFile = fileout, inchHeight = 7, inchWidth = 7)
	}

	return(myDrivPlot)
}

DoubleDriverRacePlot = function(myRace, myDriv1, myDriv2) {

	myDriv1Plot = DriverRacePlot(myRace, myDriv1 = myDriv1, includeAdjustedTime = TRUE)
	myDriv2Plot = DriverRacePlot(myRace, myDriv1 = myDriv2, includeAdjustedTime = TRUE)

	myDriv1Plot$theme$legend.position = 'bottom'
	myDriv2Plot$theme$legend.position = 'bottom'

	myLbl = MakeMyLbl(myRace, myDriv1 = myDriv1, myDriv2 = myDriv2)

	myDriv1Plot = myDriv1Plot + ylim(range(c(myLbl$sec, myLbl$fuelTyreAdjustedSec)))
	myDriv2Plot = myDriv2Plot + ylim(range(c(myLbl$sec, myLbl$fuelTyreAdjustedSec)))

	myPrettyRaceName = with(racedb, prettyname[race == myRace])
	mySurname1 = with(driverdb, adjustedsurname[driver == myDriv1])
	mySurname2 = with(driverdb, adjustedsurname[driver == myDriv2])

	myDriv1Plot = myDriv1Plot + ggtitle(paste(myPrettyRaceName, mySurname1, sep = ', '))
	myDriv2Plot = myDriv2Plot + ggtitle(paste(myPrettyRaceName, mySurname2, sep = ', '))

	gridExtra::grid.arrange(myDriv1Plot, myDriv2Plot, ncol = 2)

	### no, these loook bloody awful, use facet grid instead
	ggplot(myLbl) + geom_point(aes(x = lap, y = sec, shape = driver, colour = (isgood30 == 4))) + facet_grid(.~driver) # something like that within the main code
}

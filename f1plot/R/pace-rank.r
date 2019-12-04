### try to adapt this so that it can handle both yearly and race plots
# they're so similar, let's try to make them work together

.PaceNumObGetFastestDriver = function(myPaceDF) {
	sax1=which.min(myPaceDF$dcoef)
	fastestDriver = myPaceDF$surname[sax1]
	return(fastestDriver)
}

.PaceNumObGetDriverSurname = function(myPaceDF) {
	myPaceDF = left_join(myPaceDF,
						driverDF %>%
							select(driver, adjustedSurname) %>%
							dplyr::rename(surname = adjustedSurname),
							by = 'driver')

	return(myPaceDF)
}

.PaceNumObGetDriverLabel = function(myPaceDF, raceOrSeason, haveCarproblemDriver = FALSE) {

	### we only need the absolute time for the winner - everyone else it's nicer to see the gap i think
	myPaceDF$dispdcoef=rep(NA, dim(myPaceDF)[1])
	fastestDriver = f1plot:::.PaceNumObGetFastestDriver(myPaceDF)
	sax1=which.min(myPaceDF$dcoef)
	saxnot1=setdiff(1:dim(myPaceDF)[1],sax1)
	# myPaceDF$dispdcoef[sax1]=sprintf("%.2f",round(myPaceDF$dcoef[sax1],2))
	myPaceDF$dispdcoef[sax1]=''
	myPaceDF$dispdcoef[saxnot1]=paste('+',sprintf("%.2f",round(myPaceDF$dcoef[saxnot1]-myPaceDF$dcoef[sax1],2)),sep='')

	if (raceOrSeason == 'season' | (raceOrSeason == 'race' & !haveCarproblemDriver)) {
		myPaceDF$driverLabel = with(myPaceDF, paste0(surname, dispdcoef))
	}

	if (raceOrSeason == 'race' & haveCarproblemDriver) {
			myPaceDF$driverLabel = with(myPaceDF, paste0(ifelse(isCarProblem, '*', ''),
												surname,
												dispdcoef))
	}

	# but we won't be able to see anything for fastest driver, so let's do little fudge
	myPaceDF = myPaceDF %>%
				mutate(dcoef = ifelse(dcoef < 0.01, 0.01, dcoef))

	return(myPaceDF)
}

.PaceNumObGetPlotTitle = function(myRace = NULL, myYear = NULL, raceDF = NULL) {
	if (!is.null(myRace)) {
		myPrettyRace = with(raceDF, prettyRace[race == myRace])
		myTitle = paste('Race pace estimates for', myPrettyRace)
	}
	if (!is.null(myYear)) {
		myTitle = paste('Overall average race pace for',myYear)
	}
	return(myTitle)
}

.PaceNumObGetMiscInfo = function(raceOrSeason) {
	miscInfo = list(
					gphubFileName = paste(USERPATH, 'icon/paceplotlabel/gphub-credit.png', sep = '')
					)
	if (raceOrSeason == 'race') {
		miscInfo = append(miscInfo, list(
			panelBackground = 'grey90',
			barFill = 'darkgreen',
			driverLabelFill = 'white',
			carProblemLabel = '* driver had damaged car',
			includedFileName = paste0(USERPATH, 'icon/paceplotlabel/pace-plot-number-of-laps-included.png'),
			completedFileName = paste0(USERPATH, 'icon/paceplotlabel/pace-plot-number-of-laps-completed.png')))
	}
	if (raceOrSeason == 'season') {
		miscInfo = append(miscInfo, list(
			panelBackground = 'grey95',
			barFill = 'dodgerblue2',
			driverLabelFill = 'lemonchiffon1',
			includedFileName = paste0(USERPATH, 'icon/paceplotlabel/pace-plot-number-of-races-included.png'),
			completedFileName = paste0(USERPATH, 'icon/paceplotlabel/pace-plot-number-of-races-entered.png')))
	}
	return(miscInfo)
}

.PaceNumObGetLabelLoc = function(raceOrSeason, overallPlotWidth, overallPlotHeight, slowestDCoef) {

	labelLoc = list(
			gphubCreditYLoc = overallPlotWidth * 0.25,
			gphubCreditWidth = overallPlotWidth * 0.225,
			completedXLoc = -0.5,
			completedHeight = 1,
			completedYLoc = overallPlotWidth * 0.65,
			completedRectWidth = overallPlotWidth * 0.1,
			liveryYMin = -0.62 * slowestDCoef,
			liveryYMax = -0.1,
			liveryHeight = 1)

	if (raceOrSeason == 'race') {
		labelLoc = append(labelLoc,
					list(includedXLoc = -0.5,
							includedHeight = 1,
							includedYLoc = overallPlotWidth * 0.89,
							includedRectWidth = overallPlotWidth * 0.1,
							carProblemXLoc = -0.45,
							carProblemHeight = 0.45,
							carProblemYLoc = overallPlotWidth * 0.25,
							carProblemRectWidth = overallPlotWidth * 0.175))
	}
	if (raceOrSeason == 'season') {
		labelLoc = append(labelLoc,
					list(includedXLoc = -1,
							includedHeight = 1.5,
							includedYLoc = overallPlotWidth * 0.9,
							includedRectWidth = overallPlotWidth * 0.1))
	}

	return(labelLoc)
}

.PaceNumObAddCompletedIncludedLabel = function(myPaceDF, myBarChart, labelLoc, miscInfo) {

	myPaceDF$completedYLoc = labelLoc$completedYLoc
	myPaceDF$includedYLoc = labelLoc$includedYLoc
	myBarChart = myBarChart +
					geom_label(data = myPaceDF,
						aes(y = completedYLoc,
							x = surnameLevels,
							label = totalAvailable),
						label.size = NA)

	myBarChart = myBarChart +
					geom_label(data = myPaceDF,
						aes(y = includedYLoc,
							x = surnameLevels,
							label = totalCompleted),
						label.size = NA)

	myBarChart = myBarChart +
					aafunct::AddPictureToGgPlot(miscInfo$completedFileName,
										xmin = labelLoc$completedXLoc - labelLoc$completedHeight,
										xmax = labelLoc$completedXLoc + labelLoc$completedHeight,
										ymin = labelLoc$completedYLoc - labelLoc$completedRectWidth,
										ymax = labelLoc$completedYLoc + labelLoc$completedRectWidth)

	myBarChart = myBarChart +
					aafunct::AddPictureToGgPlot(miscInfo$includedFileName,
										xmin = labelLoc$includedXLoc - labelLoc$includedHeight,
										xmax = labelLoc$includedXLoc + labelLoc$includedHeight,
										ymin = labelLoc$includedYLoc - labelLoc$includedRectWidth,
										ymax = labelLoc$includedYLoc + labelLoc$includedRectWidth)
	return(myBarChart)
}

.PaceNumObAddGPHubCredit = function(myPaceDF, myBarChart, labelLoc, miscInfo) {

	plotRange = aafunct::GetGgPlotRange(myBarChart)
	myBarChart = myBarChart +
					with(labelLoc,
					aafunct::AddPictureToGgPlot(miscInfo$gphubFileName,
										ymin = gphubCreditYLoc - gphubCreditWidth,
										ymax = gphubCreditYLoc + gphubCreditWidth,
										xmin = plotRange$y[1] + 0.1,
										xmax = plotRange$y[1] + 1.1))

	return(myBarChart)
}

.PaceNumObAddCarProblemLabel = function(myBarChart, labelLoc, miscInfo) {
	myBarChart = myBarChart +
					with(labelLoc, annotate("rect",
								xmin = carProblemXLoc - carProblemHeight,
								xmax = carProblemXLoc + carProblemHeight,
								ymin = carProblemYLoc - carProblemRectWidth,
								ymax = carProblemYLoc + carProblemRectWidth,
								fill = 'white',
								colour = 'white')) +
					with(labelLoc, annotate("text",
								x = carProblemXLoc,
								y = carProblemYLoc,
								label = miscInfo$carProblemLabel))
	return(myBarChart)
}

.PaceNumObMakeThePlot = function(myPaceDF, raceOrSeason, myYear, myTitle, haveCarproblemDriver = FALSE) {

	myYLabel = paste('gap (seconds/lap) to', f1plot:::.PaceNumObGetFastestDriver(myPaceDF))

	# ok, thinks that's all the data i need, now got to sort out plot and table

	fontchoice='Trebuchet MS'

	myPaceDF = myPaceDF %>%
				mutate(surname = factor(surname, levels = surname))
	myPaceDF$surnameLevels = 1:nrow(myPaceDF)

	slowestDCoef = max(myPaceDF$dcoef)

	# required plot width is complicated, if slowest driver has long name then we need to widen plot for it
	# generally have to allow about 0.049 units * slowest coef for each character

	widthOfBarPlusLabel = with(myPaceDF, dcoef + slowestDCoef * 0.049 * nchar(driverLabel))
	overallPlotWidth = 1.9 * max(widthOfBarPlusLabel)

	#myYLim = c(0, slowestDCoef * 2.75)
	myYLim = c(0, overallPlotWidth)
	yAxisStepSize = 0.5 * round(2 * slowestDCoef/5)
	myYAxisTicks = seq(0, 0.5 * floor(2 * slowestDCoef), yAxisStepSize)
	myXAxisTicks = seq(1, nrow(myPaceDF), 2)
	myXLim = c(-1.5, nrow(myPaceDF) + 0.501)
	myYLimScale = 2.65

	labelLoc = f1plot:::.PaceNumObGetLabelLoc(raceOrSeason, overallPlotWidth, myXLim[2], slowestDCoef)

	miscInfo = f1plot:::.PaceNumObGetMiscInfo(raceOrSeason)

	#myXLim = c(1, nrow(myPaceDF) + 1)
	# do bar chart quickly, it's not the challenging bit
	myBarChart = ggplot(myPaceDF, aes(x = surnameLevels, y = dcoef, label = driverLabel)) +
					geom_bar(stat = 'identity', fill = miscInfo$barFill, colour = 'black') +
					theme(axis.title.y=element_blank(),
							axis.title.x = element_text(hjust = 0),
							axis.text.y=element_blank(),
							axis.ticks.y=element_blank(),
							panel.background = element_rect(fill = miscInfo$panelBackground),
							plot.title = element_text(family = fontchoice, hjust = 0.5), plot.margin = margin(l = 70, t = 10, r = 5)
							) +
							coord_flip() +
							ggtitle(myTitle) +
							ylab(myYLabel) +
							geom_label(hjust = -0.1, fill = miscInfo$driverLabelFill) +
							scale_y_continuous(breaks = myYAxisTicks, limits = myYLim) +
							scale_x_continuous(breaks = myXAxisTicks, limits = myXLim)

	myBarChart = f1plot:::.PaceNumObAddCompletedIncludedLabel(myPaceDF, myBarChart, labelLoc, miscInfo)

	if (haveCarproblemDriver) {
		myBarChart = f1plot:::.PaceNumObAddCarProblemLabel(myBarChart, labelLoc, miscInfo)
	}

	# then add on the pictures

	# make data frame of all required liveries
	for (di in 1:nrow(myPaceDF)) {
		cleanTeamName = gsub(' ', '', myPaceDF$team[di])
		iconFileName = paste0(USERPATH, 'icon/livery/', myYear, '/', cleanTeamName, myYear, '.png')

		myBarChart = myBarChart +
						with(labelLoc, aafunct::AddPictureToGgPlot(iconFileName,
											ymin = liveryYMin,
											ymax = liveryYMax,
											xmin = di - 0.5 * liveryHeight,
											xmax = di +  0.5 * liveryHeight))
	}

	if (myYear %in% c(2010:2018)) {
		myBarChart = f1plot:::.PaceNumObAddGPHubCredit(myPaceDF, myBarChart, labelLoc, miscInfo)
	}
	# then you have to do this to allow plotting outside the margins
	gt = ggplot_gtable(ggplot_build(myBarChart))
	gt$layout$clip[gt$layout$name == "panel"] = "off"

	return(gt)
}

RacePaceAndNumObPlot = function(myRace) {

	rddf = f1laptimelm::MakeNormDriverCoef(rddf, raceDF, 30, 'race')
	carProblemDF = f1laptimelm::DeriveCarProblemCoef(carProblemDF, 30)
	raceDF = f1data:::MakePrettyRaceLabel(raceDF)

	myPaceDF = rddf %>%
			filter(race == myRace & mod30PredNValid > 2) %>%
			select(driver, team, mod30DCoef, mod30PredNValid) %>%
			mutate(isCarProblem = FALSE)

	# however we'd like to add in drivers whose entire race was done with car damage
	# provided they're not ridiculous and are at least based on a reasonable number of laps

	myCarProbPaceDF = carProblemDF %>%
						filter(race == myRace &
								mod30PredNValid > 5 &
								mod30DCoef < 5 &
								isWholeRace) %>%
						mutate(isCarProblem = TRUE) %>%
						select(driver, mod30DCoef, mod30PredNValid, isCarProblem)

	haveCarproblemDriver = nrow(myCarProbPaceDF) > 0
	if (haveCarproblemDriver) {
		myPaceDF = bind_rows(anti_join(myPaceDF, myCarProbPaceDF, 'driver'),
		                     myCarProbPaceDF)
	}
	myPaceDF = lazy_left_join(myPaceDF,
								rddf %>%
									filter(race == myRace),
								'driver',
								c('team', 'maxLap'))

	myPaceDF = myPaceDF %>%
			dplyr::rename(dcoef = mod30DCoef,
							totalAvailable = maxLap,
							totalCompleted = mod30PredNValid) %>%
			mutate(dcoef = dcoef - min(dcoef)) %>%
			arrange(-dcoef)

	myPaceDF = f1plot:::.PaceNumObGetDriverSurname(myPaceDF)

	myPaceDF = f1plot:::.PaceNumObGetDriverLabel(myPaceDF, 'race', haveCarproblemDriver)

	myYear = with(raceDF, year[race == myRace])

	myTitle = f1plot:::.PaceNumObGetPlotTitle(myRace = myRace, raceDF = raceDF)

	gt = f1plot:::.PaceNumObMakeThePlot(myPaceDF, 'race', myYear, myTitle, haveCarproblemDriver)

	grid::grid.draw(gt)

	#print(myBarChart)

	fileout = MakeRaceFile(myRace = myRace,
							myFile = paste(myRace, 'RacePace.png', sep = '_'))
	aafunct::NiceGgSave(myPlot = gt, myFile = fileout, inchHeight = 7, inchWidth = 7)
}


YearlyPacePlot = function(myYear) {

	myPaceDF = f1yearlycoef::GetYearlyCoef(myYear) %>%
				select(driver, team, driverTeamCoef, numRaceEntered, numRaceEffectiveIncluded) %>%
				dplyr::rename(dcoef = driverTeamCoef,
								totalAvailable = numRaceEntered,
								totalCompleted = numRaceEffectiveIncluded) %>%
				mutate(dcoef = dcoef - min(dcoef),
						totalCompleted = niceround(totalCompleted, 1)) %>%
				arrange(-dcoef)

	myPaceDF = f1plot:::.PaceNumObGetDriverSurname(myPaceDF)

	multiTeamDriver = myPaceDF %>%
						group_by(driver) %>%
						summarise(isMultiTeam = n() > 1) %>%
						filter(isMultiTeam) %>%
						pull(driver)
	myPaceDF = myPaceDF %>%
				mutate_cond(driver %in% multiTeamDriver,
							surname = paste(surname, ToCamel(team), sep = '-'))

	myPaceDF = f1plot:::.PaceNumObGetDriverLabel(myPaceDF, 'season')

	myTitle = f1plot:::.PaceNumObGetPlotTitle(myYear = myYear)

	gt = f1plot:::.PaceNumObMakeThePlot(myPaceDF, 'season', myYear, myTitle)

	grid::grid.draw(gt)

	#print(myBarChart)

	fileWithoutExplanation = MakeYearFile(myYear = myYear,
							myFile = paste(myYear, 'yearly-pace.png', sep = '-'))
	filePlusExplanation = MakeYearFile(myYear = myYear,
							myFile = paste(myYear, 'yearly-pace-plus-explanation.png', sep = '-'))
	aafunct::NiceGgSave(myPlot = gt, myFile = fileWithoutExplanation, inchHeight = 7, inchWidth = 7)

	# but want to tack on the explanation at the bottom too
	explanationFile = paste0(USERPATH, 'icon/paceplotlabel/yearly-plot-explanation-', myYear, '.png')
	aafunct::PictureMerge(c(fileWithoutExplanation, explanationFile), filePlusExplanation, c(1, 1))
}

### a few snags still:
# can't cope with drivers switching teams DONE
# colour scheme MAYBE DONE
# no plot title DONE
# need to insert number of started races DONE
# rounding not working N/A
# need to include number of races somehow - 'approx number of races included'? NO
# add explanatory comment?

# let's try to add explanatory comment in place of all the completed/included stats

# ideas we want to express
# (2) not a straight average, more weight on races with more data
# (1) cars are stronger in some races than others. don't want this to have an effect. so we estimate average gap between team mates, then add this to the average speed of the car. Effectively it is an average if every driver had driven in every race.
# (3) if driver had damaged car, data not included

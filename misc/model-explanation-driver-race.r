### want a suite of plots that are all a bit like ggdiagplot, try to share as much as possible
### think we're not ready for flowchart yet to go out, so let's try to combine the best bits of forumplot2018, flowchart and ggdiagplot

# let's get rid of stint labels, too hard to maintain

PlotTyreFuelEffect = function(myRace, myDriv) {
	myLbl = lblForDR %>%
			filter(race == myRace &
					driver == myDriv)
	myTyreShape = f1plot:::.DriverRaceGetTyreShape(myLbl, tyreDF)
	
	plotConstant = f1plot:::.DriverRaceGetPlotConstant()

	myPlot = .SubPlotTyreFuelEffectByDriver(myLbl, myRace, myDriv, myTyreShape, addInFuelEffect = TRUE, plotConstant)
	
	return(myPlot)
}

.SubPlotTyreFuelEffectByDriver = function(myLbl, myRace, myDriv, myTyreShape, addInFuelEffect = FALSE, plotConstant) {

	mySurname = with(driverDF, adjustedSurname[driver == myDriv])

	inlapDF = f1plot:::.DriverRaceMakeInlapDF(myRace, myDriv)
	
	if (!addInFuelEffect) {
		myLbl$currentEffect = with(myLbl, tyreEffect)
		myTitle = 'Effect of fuel load on DRIVER\'s lap times'
		myTitle = gsub('DRIVER', mySurname, myTitle)
		myYLab = WordWrap('seconds lost for every lap of fuel')
		myXLab = 'lap'
	}
	if (addInFuelEffect) {
		myLbl$currentEffect = with(myLbl, tyreEffect + fuelEffect)
		myTitle = 'Effect of fuel load, tyre choice and tyre wear on DRIVER\'s lap times'
		myTitle = gsub('DRIVER', mySurname, myTitle)

		myYLab = 'seconds lost due to fuel and tyre, compared to 1 lap of fuel and fresh set of FASTESTNEWTYRE'
		myTyreCoef = raceTyreDF %>%
						filter(race == myRace)
		fastestTyre = with(myTyreCoef, tyre[which.min(mod30TyreInt)])
		myYLab = gsub('FASTESTNEWTYRE', paste0(fastestTyre, 's'), myYLab)
		myYLab = WordWrap(myYLab, 15)

		myXLab = 'lap'
	}

	myYLim = c(0, max(myLbl$currentEffect))
	myXLim = c(1,with(raceDF,nlap[race == myRace]))
	
	myTheme = theme(panel.background = element_rect(fill = plotConstant$fuelTyreEffectPanelBackground),
					text = element_text(family = plotConstant$fuelTyreEffectFontChoice),
					legend.title=element_blank(),
					legend.position = 'bottom',
					axis.title.y = element_text(angle = 0, vjust = 0.5))

	myDrivTyrePlot = ggplot(myLbl %>% filter(driver == myDriv)) +
					geom_point(aes(x = lap, y = currentEffect, shape = tyre),
								fill = plotConstant$fuelTyreEffectDriver1Colour,
								size = 3,
								col = 'black') +
					ylim(myYLim) +
					xlim(myXLim) +
					scale_shape_manual(values = myTyreShape) +
					geom_vline(xintercept = with(inlapDF, endOfLap[driver == myDriv]),
							linetype = 2,
							col = plotConstant$fuelTyreEffectDriver1Colour) +
					xlab(myXLab) +
					ylab(myYLab) +
					ggtitle(myTitle) +
					myTheme
	
	return(myDrivTyrePlot)
}

.FitFuelModel = function(myDriv1, myRace, lblForDR) {
	lblForFuelLM = lblForDR %>%
					filter(race == myRace & isGood30 & !isCarProblem)
	fuelMod = lm(sec ~ factor(driver) + fuel - 1, data = lblForFuelLM)
	
	allDriverCoef = coef(fuelMod)[grep('factor\\(driver\\)', names(coef(fuelMod)))]
	names(allDriverCoef) = gsub('factor\\(driver\\)', '', names(allDriverCoef))
	myDriv1Coef = allDriverCoef[[myDriv1]]
	fuelCoef = coef(fuelMod)[['fuel']]
	
	return(c(myDriv1Coef, fuelCoef))
}

.GetAesthetic = function(plotMode, colourScheme, plotConstant, myLbl, myTyreShape) {
	myLineType = NULL
	myLineColour = NULL
	if (plotMode == 'raw') {
		myShape = 21
		myFill = c('yes' = colourScheme$fill,
						'safety car' = colourScheme$safetyCar,
						'inlap/outlap' = colourScheme$pitStop)
	}
	if (plotMode == 'blockedot') {
		myShape = c('clear lap' = 21,
						'blocked' = 22)
		myFill = c('clear lap' = colourScheme$fill,
						'blocked' = colourScheme$blocked)
	}
	if (plotMode == 'fuelfitted') {
		myShape = 21
		myFill = colourScheme$fill
		myLineType = 1
		names(myLineType) = 'fitted fuel effect'
		myLineColour = colourScheme$outline
	}
	if (plotMode == 'fuelcorrected') {
		myShape = c('fuel corrected lap time' = 21,
					myTyreShape)
		myFill = c(colourScheme$corrected, rep(colourScheme$fill, length(myTyreShape)))
		names(myFill) = names(myShape)
		myLineType = plotConstant$meanAdjustedLineType
		names(myLineType) = plotConstant$averageAdjSecLabel
		myLineColour = colourScheme$corrected
	}
	
	return(list(myShape = myShape, myFill = myFill,
				myLineType = myLineType, myLineColour = myLineColour))
}

.MakeVertLbl = function(plotMode, myLbl) {
		
	if (plotMode == 'raw') {
		myVertLbl = myLbl %>%
					select(lap, sec, isSafetyCar, inlap, outlap)
	}
	if (plotMode == 'blockedot') {
		myVertLbl = myLbl %>%
					filter(!isRogue) %>%
					select(lap, sec, inTraffic, inlap)
	}
	if (plotMode == 'fuelfitted') {
		myVertLbl = myLbl %>%
					filter(isGood30) %>%
					select(lap, sec, fuel, inlap, fuelLineOfBestFit)
	}
	if (plotMode == 'fuelcorrected') {
		myVertLbl = myLbl %>%
					filter(isGood30) %>%
					select(lap, sec, tyre, fuelAdjustedSec, inlap) %>%
					tidyr::gather(realAdjusted, sec, -c(lap, tyre, inlap))
	}

	return(myVertLbl)
}

.MakeLapLabel = function(plotMode, myVertLbl) {
	if (plotMode == 'raw') {
		myVertLbl = myVertLbl %>%
						mutate(lapLabel = case_when(
								(inlap | outlap) ~ 'inlap/outlap',
								isSafetyCar & (!inlap | outlap) ~ 'safety car',
								TRUE ~ 'yes'))
	}
	if (plotMode == 'blockedot') {
		myVertLbl = myVertLbl %>%
						mutate(lapLabel = case_when(
								!inTraffic ~'clear lap',
								inTraffic ~ 'blocked'))
	}
	if (plotMode == 'fuelfitted') {
		myVertLbl = myVertLbl %>%
						mutate(lapLabel = 'fitted fuel effect')
	}
	if (plotMode == 'fuelcorrected') {
		myVertLbl = myVertLbl %>%
						mutate(lapLabel = case_when(
								(realAdjusted == 'fuelAdjustedSec') ~ 'fuel corrected lap time',
								(realAdjusted == 'sec') ~ tyre))
	}
	return(myVertLbl)
}

.PlotLapTime = function(plotMode, myVertLbl, myDrivPlot, myShape, myFill, colourScheme, plotConstant) {
	if (plotMode == 'raw') {
		myDrivPlot = myDrivPlot +
						geom_point(data = myVertLbl,
									aes(x = lap, y = sec, fill = lapLabel),
										shape = myShape,
										col = colourScheme$outline,
										size = plotConstant$pointSize) +
						scale_fill_manual(values = myFill)
	}
	if (plotMode == 'blockedot') {
		myDrivPlot = myDrivPlot +
						geom_point(data = myVertLbl,
									aes(x = lap, y = sec, fill = lapLabel,
										shape = lapLabel),
										col = colourScheme$outline,
										size = plotConstant$pointSize) +
						scale_fill_manual(values = myFill) +
						scale_shape_manual(values = myShape)
	}
	if (plotMode == 'fuelfitted') {
		myDrivPlot = myDrivPlot +
						geom_point(data = myVertLbl,
									aes(x = lap, y = sec),
										fill = myFill,
										shape = myShape,
										col = colourScheme$outline,
										size = plotConstant$pointSize)
	}
	if (plotMode == 'fuelcorrected') {
		myDrivPlot = myDrivPlot +
						geom_point(data = myVertLbl,
									aes(x = lap, y = sec,
										fill = lapLabel,
										shape = lapLabel),
										col = colourScheme$outline,
										size = plotConstant$pointSize) +
						scale_fill_manual(values = myFill) +
						scale_shape_manual(values = myShape)
	}
	return(myDrivPlot)
}

.AddFuelLine = function(plotMode, myVertLbl, myDrivPlot, myLineType, myLineColour) {
	if (plotMode == 'fuelfitted') {
		myDrivPlot = myDrivPlot +
						geom_line(aes(x = lap, y = fuelLineOfBestFit, linetype = lapLabel),
								col = myLineColour,
								size = 1) +
					scale_linetype_manual(name = '',
											values = myLineType)
	}
	if (FALSE) {
	if (plotMode == 'fuelcorrected') {
		myDrivPlot = myDrivPlot +
					geom_hline(data = driv1CoefDF,
							aes(yintercept = driv1Coef, linetype = legendLabel),
								size = plotConstant$averageSecLineSize,
								col = myLineColour) +
							scale_linetype_manual(values = myLineType)
	}
	}
	return(myDrivPlot)
}

.AddPitStopLine = function(plotMode, inlapDF, myPitStopLineType, colourScheme, plotConstant, myDrivPlot) {
	### it's a bit cunning, only labels the pit stop line when there's no other line
	if (plotMode == 'blockedot') {
		myDrivPlot = myDrivPlot +
					geom_vline(data = inlapDF,
							aes(xintercept = endOfLap,
								linetype = leglabel),
								col = colourScheme$outline,
								size = plotConstant$pitStopLineSize)  +
					scale_linetype_manual(values = myPitStopLineType)
	}
	if (plotMode %in% c('fuelfitted', 'fuelcorrected')) {
		myDrivPlot = myDrivPlot +
						geom_vline(data = inlapDF,
									aes(xintercept = endOfLap),
								col = colourScheme$outline,
								linetype = plotConstant$pitStopLineType,
								size = plotConstant$pitStopLineSize)
	}
	
	return(myDrivPlot)
}

ModelExplanationPlot = function(myRace, myDriv1) {
	# gets a bit fiddly here, need to have blue shapes for actual times, but black somethings for adjusted times

	raceDF = f1data:::MakePrettyRaceLabel(raceDF)
	if (!exists('lblForDR')) {
		lblForDR = f1plot:::.DriverRaceInitialiseData(lbl)
	}
	
	tyreHardnessDF=ReadF1Data(paste0(USERPATH, 'f1admin/tyre.csv'), 'tyre')

	myLbl = lblForDR %>%
			filter(race == myRace &
					driver == myDriv1)
	
	plotConstant = f1plot:::.DriverRaceGetPlotConstant()

	plotTitle = f1plot:::.DriverRaceMakePlotTitle(myRace, myDriv1, myDriv2 = NULL, raceDF = raceDF)

	colourScheme = f1plot:::.DriverRaceMakeColourScheme(myRace, myDriv1)
	myTheme = f1plot:::.DriverRaceGetTheme(FALSE, colourScheme = colourScheme)
	myTheme$legend.title = element_blank()

	myXLim = c(1,with(raceDF,nlap[race == myRace]))

	myTyre = unique(myLbl$tyre)
	myTyreShape = f1plot:::.DriverRaceGetTyreShape(myLbl, tyreHardnessDF)

	inlapDF = f1plot:::.DriverRaceMakeInlapDF(myRace, myDriv1, myDriv2 = NULL)
	myPitStopLineType = c('pit stop' = plotConstant$pitStopLineType)
	
	dum = .FitFuelModel(myDriv1, myRace, lblForDR)
	myDriv1Coef = dum[1]
	fuelCoef = dum[2]
	myLbl$fuelAdjustedSec = with(myLbl, sec - fuelCoef * fuel)
	myLbl$fuelLineOfBestFit = with(myLbl, myDriv1Coef + fuelCoef * fuel)
	driv1CoefDF = tibble(driv1Coef = myDriv1Coef,
						legendLabel = plotConstant$averageAdjSecLabel)

	for (plotMode in c('raw', 'blockedot', 'fuelfitted', 'fuelcorrected')) {
		dum = .GetAesthetic(plotMode, colourScheme, plotConstant, myLbl, myTyreShape)
		myShape = dum$myShape
		myFill = dum$myFill
		myLineType = dum$myLineType
		myLineColour = dum$myLineColour
	
		myVertLbl = .MakeVertLbl(plotMode, myLbl)
		myVertLbl = .MakeLapLabel(plotMode, myVertLbl)

		myDrivPlot = ggplot(myVertLbl) +
					myTheme +
					plotTitle$maintitle +
					plotTitle$ylabel +
					xlim(myXLim)
		
		myDrivPlot = .PlotLapTime(plotMode, myVertLbl, myDrivPlot, myShape, myFill, colourScheme, plotConstant)
		
		# then add lines of best fit where appropriate
		if (plotMode == 'fuelfitted') {
			myDrivPlot = .AddFuelLine(plotMode, myVertLbl, myDrivPlot, myLineType, myLineColour)
		}
		
		# then add pit stop lines
		if (plotMode %in% c('blockedot', 'fuelfitted', 'fuelcorrected')) {
			myDrivPlot = .AddPitStopLine(plotMode, inlapDF,
											myPitStopLineType, colourScheme, plotConstant,
											myDrivPlot)
		}
		
		fileout = MakeRaceFile(myRace=myRace, myFile=paste0(myDriv1, '_', plotMode, '.png'))
		aafunct::NiceGgSave(myPlot = myDrivPlot, myFile = fileout, inchHeight = 7, inchWidth = 7)
	}
	
	# we also want the slightly different plot which is the fuel and tyre effect plot
	myPlot = PlotTyreFuelEffect(myRace, myDriv1)
	fileout = MakeRaceFile(myRace=myRace, myFile=paste0(myDriv1, '_fuelTyreEffect.png'))
	aafunct::NiceGgSave(myPlot, myFile = fileout, inchHeight = 7, inchWidth = 7)
}

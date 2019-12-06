
PlotTyreDeg = function(myRace, toFile = FALSE) {
	### first find out which tyres were actually used in the race

	myTyreDF = raceTyreDF %>%
				filter(race == myRace) %>%
				select(tyre, mod30TyreInt, mod30TyreSlope)
	fastestNewTyre = with(myTyreDF, tyre[near(mod30TyreInt, 0)])

	stintLength = 30
	tyreWearDF = left_join(expand.grid(tyre = myTyreDF$tyre,
										tyreLap = 0:(stintLength - 1),
										stringsAsFactors = FALSE),
							myTyreDF,
							'tyre')
	
	if (FALSE) {
		# this is done at modelling stage, no?
	## normalise so fastest new tyre is always zero
	fastestNewTyreInfo = tyreweardf %>%
						filter(tlap == 0) %>%
						filter(int == min(int))
	baselineIntercept = fastestNewTyreInfo$int
	fastestNewTyre = fastestNewTyreInfo$tyre
	
	tyreweardf$int = tyreweardf$int - baselineIntercept
	}
	
	tyreWearDF$value = with(tyreWearDF, mod30TyreInt + mod30TyreSlope*tyreLap)

	raceDF = f1data:::MakePrettyRaceLabel(raceDF)
	myPrettyRace = with(raceDF, prettyRace[race == myRace])

	myInit = ggplot(tyreWearDF, aes(x = tyreLap, y = value, col = tyre))
	mainTitle = paste0('Tyre wear, each compound, ', myPrettyRace)
	yLabel = paste('seconds lost (compare to new set of ', fastestNewTyre, ')')
	xLabel='laps on tyre'
	legList=list(colour='Tyre')
	panelColour='slategray1'
	plotBGColour='grey97'
	fontChoice='Trebuchet MS'

	myTheme = theme(text = element_text(family = fontChoice),
				plot.background = element_rect(fill = plotBGColour),
				panel.background = element_rect(fill = panelColour),
				legend.key = element_rect(fill = panelColour),
				plot.title = element_text(size = 13,
											lineheight = .8,
											vjust = 1))
	
	myPlot = myInit +
				geom_line(size = 1) +
				ylab(yLabel) +
				xlab(xLabel) +
				labs(legList) +
				ggtitle(mainTitle) +
				myTheme
	
	if (toFile) {
		fileOut=MakeRaceFile(myRace = myRace, myFile = 'tyredeg.png')
		#savePlot(fileout,type='png')
		aafunct:::NiceGgSave(myPlot, myFile = fileOut)
	}
	
	return(myPlot)
}

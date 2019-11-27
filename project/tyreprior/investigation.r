# got some really messy code to get the tyre coefs out now that we've reordered the factors, this should do it:

LoadAllData()

PROJECTPATH = paste(USERPATH, 'project/tyreprior/', sep = '')
source(paste(PROJECTPATH, 'tyreprior_data_setup.r', sep = ''))
source(paste(PROJECTPATH, 'tyreprior_funct.r', sep = ''))

raceDF[,c('tyreLapPrior1', 'tyreLapPrior2')] = NA
for (ri in which(raceDF$isValidRace30)) {
	dum = FitSingleRace(raceDF$race[ri], priorScale = list(driver = 0.001, tyre = 0.001, tyreLap = 0.001, fuel = 0.001), 1, 'cv')
	raceDF$tyreLapPrior1[ri] = dum$sampleMleList[[1]]$tyreLap$tyreLapCoef
	raceDF$tyreLapPrior2[ri] = dum$sampleMleList[[2]]$tyreLap$tyreLapCoef
}

ExtractTyreCoefFromMod = function(myMod) {
    tyrePhrase = paste("factor\\(tyre, levels.+\\)[a-z]+$",
            sep = "")
    tlapPhrase = paste("factor\\(tyre, levels.+\\)[a-z]+:tyreLap2$", sep = "")
    myTyre = myMod$xlevels[[2]]
    uglyTyreCoef = coef(myMod)[grep(tyrePhrase, names(coef(myMod)))]
	names(uglyTyreCoef) = gsub("^.+\\)", "", names(uglyTyreCoef))
	uglyTlapReferenceCoef = coef(myMod)[['tyreLap2']]
	uglyTlapCoef = coef(myMod)[grep(tlapPhrase, names(coef(myMod)))]
	names(uglyTlapCoef) = gsub("(factor\\(tyre[^\\)]*\\))([^:]+)(:.+$)",
		"\\2", names(uglyTlapCoef))
	tyreCoefInt = c(0, as.numeric(uglyTyreCoef))
	tyreCoefSlope = as.numeric(c(uglyTlapReferenceCoef, uglyTlapReferenceCoef +
		uglyTlapCoef))
	# but we don't want any -ve intercepts, so add the smallest one on
	# tyreCoefInt = tyreCoefInt - min(tyreCoefInt)

	tyreCoefDF = tibble(tyre = myTyre, int = tyreCoefInt, slo = tyreCoefSlope)

	return(tyreCoefDF)
}

FitLMModel = function(myRace, tyreCount, allTyreLapDF) {

	tyreLevel = with(tyreCount, tyre[order(-totalLap)])
	mod1 = lm(sec ~ factor(driver) + fuel2 + factor(tyre, levels = tyreLevel) * tyreLap2 - 1,
				data = lbl %>% filter(race == myRace & isGoodPreValidRace & inSample == 1))
	mod1TyreCoefDF = ExtractTyreCoefFromMod(mod1) %>% mutate(inSample = '1')
	mod2 = lm(sec ~ factor(driver) + fuel2 + factor(tyre, levels = tyreLevel) * tyreLap2 - 1,
				data = lbl %>% filter(race == myRace & isGoodPreValidRace & inSample == 2))
	mod2TyreCoefDF = ExtractTyreCoefFromMod(mod2) %>% mutate(inSample = '2')
	mod = lm(sec ~ factor(driver) + fuel2 + factor(tyre, levels = tyreLevel) * tyreLap2 - 1,
				data = lbl %>% filter(race == myRace & isGoodPreValidRace))
	modTyreCoefDF = ExtractTyreCoefFromMod(mod) %>% mutate(inSample = 'combined')

	combinedModTyreCoef = bind_rows(mod1TyreCoefDF,
									mod2TyreCoefDF,
									modTyreCoefDF)

	lmTyreLapDF = left_join(allTyreLapDF,
								combinedModTyreCoef,
								c('tyre', 'inSample'))
	lmTyreLapDF$effect = with(lmTyreLapDF, int + slo * tyreLap2)

	# but we want to join in the info about number of laps into the tyre label, we can do that here
	lmTyreLapDF = lazy_left_join(lmTyreLapDF, tyreCount, 'tyre')

	return(lmTyreLapDF)
}

FitPrioredModel = function(myRace, tyreCount, allTyreLapDF, tyreScale, tyreLapScale) {

	# then do our priored stuff
	dum = FitSingleRace(myRace, priorScale = list(driver = 0.0001, tyre = tyreScale,
													tyreLap = tyreLapScale, fuel = 0.0001), 2, 'cv')

	prioredMod1Coef= with(dum$sampleMleList[[1]], left_join(tyre, tyreLap, 'tyre')) %>%
					rename(int = tyreCoef, slo = tyreLapCoef) %>%
					mutate(inSample = '1')
					#mutate(int = int - min(int))
	prioredMod2Coef= with(dum$sampleMleList[[2]], left_join(tyre, tyreLap, 'tyre')) %>%
					rename(int = tyreCoef, slo = tyreLapCoef) %>%
					mutate(inSample = '2')
					#mutate(int = int - min(int))

	# be careful though, because the lm ones have ensured that the most common tyre has intercept 0. but we don't have that happening in priored, do make it so
	mostCommonTyre = with(tyreCount, tyre[which.max(totalLap)])
	prioredMod1Coef = prioredMod1Coef %>%
						mutate(int = int - int[tyre == mostCommonTyre])
	prioredMod2Coef = prioredMod2Coef %>%
						mutate(int = int - int[tyre == mostCommonTyre])

	combinedPrioredCoef = bind_rows(prioredMod1Coef, prioredMod2Coef)
	prioredTyreLapDF = left_join(allTyreLapDF,
								combinedPrioredCoef,
								c('tyre', 'inSample')) %>%
						mutate(effect = int + slo * tyreLap2)
	prioredTyreLapDF = lazy_left_join(prioredTyreLapDF, tyreCount, 'tyre')

	return(prioredTyreLapDF)
}

# so let's have a function that does the whole shebang for this race
ExamineRace = function(myRace, tyreScale, tyreLapScale) {
	# firstly do the naive lm for each half of the race, plus overall
	# make sure it's the most commonly used tyres that are by default used in the lm
	tyreCount = lbl %>%
					filter(race == myRace & isGoodPreValidRace) %>%
					count(tyre, inSample) %>%
					spread(key = inSample, value = n) %>%
					mutate(tyreCount = paste0(tyre, ' ', `1`, '/', `2`),
							totalLap = `1` + `2`)

	allTyreLapDF = expand.grid(tyre = tyreCount$tyre,
								tyreLap2 = 1:30,
								inSample = c('1', '2', 'combined'),
								stringsAsFactors = FALSE)


	lmTyreLapDF = FitLMModel(myRace, tyreCount, allTyreLapDF)
	prioredTyreLapDF = FitPrioredModel(myRace, tyreCount, allTyreLapDF, tyreScale, tyreLapScale)

	#### now we're ready to plot the bastards
	# make sure the combined one is solid
	myLineType = c('1' = 2, '2' = 3, 'combined' = 1)

	commonYLim = range(c(lmTyreLapDF$effect, prioredTyreLapDF$effect), na.rm = TRUE)

	lmPlot = ggplot(lmTyreLapDF %>%
						filter(!is.na(effect))) +
					geom_line(aes(x = tyreLap2, y = effect,
								col = tyreCount, linetype = inSample)) +
					scale_linetype_manual(values = myLineType) +
					ylim(commonYLim) +
					ggtitle(paste(myRace, '- Raw LM model'))

	prioredPlot = ggplot(prioredTyreLapDF %>%
						filter(inSample != 'combined')) +
					geom_line(aes(x = tyreLap2, y = effect,
								col = tyreCount, linetype = inSample)) +
					scale_linetype_manual(values = myLineType) +
					ylim(commonYLim) +
					ggtitle(paste(myRace, ' - priored model'))

	# and let's add in the line they are being priored to
	tyreLapPriorDF = raceDF %>%
						filter(race == myRace) %>%
						select(tyreLapPrior1, tyreLapPrior2)
	prioredPlot = prioredPlot +
					geom_abline(intercept = 0, slope = tyreLapPriorDF$tyreLapPrior1,
									linetype = 1) +
					geom_abline(intercept = 0, slope = tyreLapPriorDF$tyreLapPrior2,
									linetype = 1)

	### firstly, check it's behaving as expected, display the two plots together
	gridExtra::grid.arrange(lmPlot, prioredPlot, nrow = 2)
}

# ok, so it's doing exactly what it should be doing, no problem there. but now let's look into actual predictive ability instead
# it's obvious just looking at the graphs that there is no need for any kind of meaningful prior though. On every single one I look at, the tyre with the least data is better modelled by its other half than by either of the other tyres.
# at the heart of the problem is this though - generally a tyre is not used much because it is bad. and if it's bad, then it's unlikely to be like the other two tyres, so prioring it to them does not make sense
# so let's just go ahead with a model with very small priors just to ensure convergence
# and it doesn't really matter if one of the tyres has slightly wild coefs because it should have very little impact on the driver's estimate.
#### or should it....no, i think it might have a big impact on the driver's estimate because the estimate is just a straight average of the adjusted times...it's the standard error of the driver's coef that will go up.
# let's look into that in a separate file

# but can investigate the interesting races via this:

problemRace = raceTyreDF %>%
								lazy_left_join(raceDF, 'race', 'isValidRace30') %>%
								filter(year > 2015 & !isValidTyre30 & isValidRace30) %>%
								select(race, tyre)
for (myRace in unique(problemRace$race)) {
	ExamineRace(myRace, 0.001, 0.001)
	dum = askcond(F,T)
}

# so let's have a function that does the whole shebang for this race
ExamineRace = function(myRace, tyreScale, tyreLapScale) {
	# firstly do the naive lm for each half of the race, plus overall
	# make sure it's the most commonly used tyres that are by default used in the lm

	tyreCount = lbl %>%
					filter(race == myRace & isGoodPreValidRace) %>%
					count(tyre, inSample) %>%
					spread(key = inSample, value = n) %>%
					mutate(tyreCount = paste0(tyre, ' ', `1`, '/', `2`),
							totalLap = `1` + `2`)

	allTyreLapDF = expand.grid(tyre = tyreCount$tyre,
								tyreLap2 = 1:30,
								inSample = c('1', '2', 'combined'),
								stringsAsFactors = FALSE)


	noPriorTyreLapDF = FitPrioredModel(myRace, tyreCount, allTyreLapDF, 0.0001, 0.0001)
	suppliedTyreLapDF = FitPrioredModel(myRace, tyreCount, allTyreLapDF, tyreScale, tyreLapScale)

	myLineType = c('1' = 2, '2' = 3, 'combined' = 1)
	commonYLim = range(c(noPriorTyreLapDF$effect, suppliedTyreLapDF$effect), na.rm = TRUE)

	noPriorPlot = ggplot(noPriorTyreLapDF %>%
						filter(inSample != 'combined')) +
					geom_line(aes(x = tyreLap2, y = effect,
								col = tyreCount, linetype = inSample)) +
					scale_linetype_manual(values = myLineType) +
					ylim(commonYLim) +
					ggtitle('no prior model')
	suppliedPlot = ggplot(suppliedTyreLapDF %>%
						filter(inSample != 'combined')) +
					geom_line(aes(x = tyreLap2, y = effect,
								col = tyreCount, linetype = inSample)) +
					scale_linetype_manual(values = myLineType) +
					ylim(commonYLim) +
					ggtitle('supplied scale model')
	# and let's add in the line they are being priored to
	tyreLapPriorDF = raceDF %>%
						filter(race == myRace) %>%
						select(tyreLapPrior1, tyreLapPrior2)
	noPriorPlot = noPriorPlot +
					geom_abline(intercept = 0, slope = tyreLapPriorDF$tyreLapPrior1,
									linetype = 1) +
					geom_abline(intercept = 0, slope = tyreLapPriorDF$tyreLapPrior2,
									linetype = 1)
	suppliedPlot = suppliedPlot +
					geom_abline(intercept = 0, slope = tyreLapPriorDF$tyreLapPrior1,
									linetype = 1) +
					geom_abline(intercept = 0, slope = tyreLapPriorDF$tyreLapPrior2,
									linetype = 1)
	gridExtra::grid.arrange(noPriorPlot, suppliedPlot, nrow = 2)

	# it's always worht displaying the effective zero priored thing i think, that's the obvious reference
	dum = FitSingleRace(myRace, priorScale = list(driver = 0.0001, tyre = 0.0001,
													tyreLap = 0.0001, fuel = 0.0001),
													2, 'cv', returnPred = TRUE)

	# so we want to add up the sqdiff for each tyre within each sample
	myPredSecDF = dum$thisRaceLbl
	sqDiff = myPredSecDF %>%
				group_by(inSample, tyre) %>%
				summarise(sumSqDiff = sum(sqDiff))

	print('With very minor prior strengths:')
	print(sqDiff)

	message('Total sqDiff:')
	print(sum(myPredSecDF$sqDiff))

	dum = FitSingleRace(myRace, priorScale = list(driver = 0.0001, tyre = tyreScale,
													tyreLap = tyreLapScale, fuel = 0.0001),
													2, 'cv', returnPred = TRUE)

	# so we want to add up the sqdiff for each tyre within each sample
	myPredSecDF = dum$thisRaceLbl
	sqDiff = myPredSecDF %>%
				group_by(inSample, tyre) %>%
				summarise(sumSqDiff = sum(sqDiff))

	print('With supplied prior strengths:')
	print(sqDiff)

	message('Total sqDiff:')
	print(sum(myPredSecDF$sqDiff))

}

# even for races which have very small amounts of data for a tyre, e.g 25 laps in each sample, the two lines look pretty close to each other in general - much closer than the overall average.
# so I don't think there's a lot in this. I think we're better off introducing this just so that we can get rid of validTyre, because that's a pain in the arse. but let's not do that now, too much disruption, better things to be doing between now and start of season

### have been focussing on wrong problem i think , have been looking too much at situatinos like malaysia 2012, monaco 2016 etc. but they're lower priority, what is more pressing is the common 2016 situation of having tyres that were rarely used in a normal race

# which is more of a phase 2 problem surely

# let's look at a race like Europe 2016. here, the medium was barely used, so would expect its coef to be very sensitive

PROJECTPATH = paste(USERPATH, 'development/tyreprior jan2018/', sep = '')
source(paste(PROJECTPATH, 'tyreprior_data_setup.r', sep = ''))
source(paste(PROJECTPATH, 'tyreprior_funct.r', sep = ''))
# coefs, some appear not to be very good

myRacename = '2016europe'

## make most commonly used tyre the default in the lm
tyreUsageOrder = lbl %>%
				filter(racename == myRacename & isvalid) %>%
				count(tyre) %>%
				arrange(-n) %>%
				pull(tyre)

				
mod = lm(sec ~ factor(driver) + fuel + factor(tyre, levels = tyreUsageOrder) * tlap2,
			data = lbl %>% filter(racename == myRacename & isvalid))

## can see some large stndard errors there, let's use phaes 1 to get the tyre and fuel prior, only focus on seeing what driver prior should be

# so let's do that

race2016 = racedb %>%
			filter(isvalidrace30 & yr == 2016) %>%
			select(racename)

FitAll2016Race = function(theta) {
	driverPrior = exp(theta)
	priorScale = list(driver = driverPrior, fuel = 0.1, tyre = 0.1, tlap = 0.1)
	raceMeanSqDiff = rep(NA, nrow(race2016))
	for (ri in 1:nrow(race2016)) {
		raceMeanSqDiff[ri] = FitSingleRace(race2016$racename[ri], priorScale = priorScale, phase12 = 1, crossValidStatus = 'cv')$meanSqDiff
	}
	meanMeanSqDiff = mean(raceMeanSqDiff)
	return(meanMeanSqDiff)
}

# seems like it wants driver priors a bit: 0.29 is opt, so driverPrior = 1.33

# now, let's collect the fuel and combined tyre prior coefs from phase 1

driverPrior = exp(0.29)
priorScale = list(driver = driverPrior, fuel = 0.1, tyre = 0.1, tlap = 0.1)
race2016[, c('fuelPrior', 'tlapPrior')] = NA
for (ri in 1:nrow(race2016)) {
	dum = FitSingleRace(race2016$racename[ri], priorScale = priorScale, phase12 = 1, crossValidStatus = 'no')
	race2016$fuelPrior[ri] = dum$fuel
	race2016$tlapPrior[ri] = dum$tlap$tlapCoef
}

### awesome, now we set those as priors for phase 2
## although FitSingleRace isn't quite right for what we want

FitSingleRace2 = function(myRacename, priorScale, crossValidStatus) {

	mod30PriorDF = rddf %>% 
					filter(racename==myRacename & !is.na(startinggrid)) %>%
					select(driver, mod30mixqrsmdcoef)
					
	priorValue = with(race2016[race2016$racename == myRacename,], list(fuel = fuelPrior, tyre = 0, tlap = tlapPrior))

	thisRaceLbl = lbl %>%
					filter(racename == myRacename & isvalid) %>%
					select(driver, fuel2, tyre, tlap2, sec, insample) %>%
					mutate(interceptCoef = NA,
							driverCoef = NA,
							tyreCoef = NA,
							tlapCoef = NA,
							fuelCoef = NA)

	if (crossValidStatus == 'cv') {
		sampleMleList = NULL
		for (samplei in 1:2) {
			myLbl = thisRaceLbl %>%
					filter(insample == samplei)
					
			myMleDF = SolveRaceMatrix(myLbl, mod30PriorDF, priorValue, priorScale, phase12 = 2)
			myMleList = TidyMleDF(myMleDF, mod30PriorDF)
			sampleMleList[[samplei]] = myMleList
			
			# match up those coefs to half they need to predict:
			thisRaceLbl = JoinLblToCoef(thisRaceLbl, myMleList, samplei)
		}
				
		thisRaceLbl = thisRaceLbl %>%
					mutate(predSec = interceptCoef + driverCoef + fuelCoef * fuel2 + tyreCoef + tlapCoef * tlap2)
					
		thisRaceLbl$sqDiff = with(thisRaceLbl, (predSec - sec)^2)
		meanSqDiff = mean(thisRaceLbl$sqDiff)
		toReturn = list(meanSqDiff = meanSqDiff,
						sampleMleList = sampleMleList)
	}

	if (crossValidStatus == 'no') {
		myLbl = thisRaceLbl
					
		myMleDF = SolveRaceMatrix(myLbl, mod30PriorDF, priorValue, priorScale, phase12 = 2)
		myMleList = TidyMleDF(myMleDF, mod30PriorDF)

		toReturn = myMleList
	}
	
	return(toReturn)
}

## just try out this prior on a few races, see if it's doing what you'd think
myRacename = '2016mexico'
priorScale  = list(driver = driverPrior, fuel = 10000, tyre = 5, tlap = 10)
FitSingleRace2(myRacename, priorScale, crossValidStatus = 'no')

# optimise a single race:
OptimiseSingleRace = function(theta, myRacename) {
	priorScale = list(driver = 1, fuel = exp(theta[1]), tyre = exp(theta[2]), tlap = exp(theta[3]))
	meanSqDiff = FitSingleRace2(myRacename, priorScale, crossValidStatus = 'cv')$meanSqDiff
	print(paste(priorScale, collapse = ', '))
	print(meanSqDiff)
	return(meanSqDiff)
}

nlm(OptimiseSingleRace, p = c(log(1000), 0, log(1000)), myRacename = '2016russia')

# get wildly different results for just a single race, let's do the whole lot

FitAllRace = function(theta) {

	priorScale = list(driver = 1, fuel = exp(theta[1]), tyre = exp(theta[2]), tlap = exp(theta[3]))

	race2016$meanSqDiff = NA
	for (ri in 1:nrow(race2016)) {
		race2016$meanSqDiff[ri] = FitSingleRace2(race2016$racename[ri], priorScale, crossValidStatus = 'cv')$meanSqDiff
	}
	
	meanMeanSqDiff = mean(race2016$meanSqDiff)

	print(paste(priorScale, collapse = ', '))
	print(meanMeanSqDiff)
	return(meanMeanSqDiff)
}

maxInfo = nlm(FitAllRace, p = c(log(1000), 0, log(1000)))

# ah ffs, it doesn't want any of the tyre prior. how can this be. let's look at the really dodgy looking ones and see how it can not want them at all
# is it possible that in terms of overall fit it has little impact, but justmakes a handful of ones look stupid without doing that much damage.
# maybe sqdiff isn't a good measure, we need something else that measure how stupid a curve looks
# doing sqdiff on whole race obviously dilutes everything a lot. we want to just plot tyre prior against what data is saying

PlotTyreIssue = function(myRacename) {
	# let's have no priors, and display all fitted tyre coefs as well as the prior
	# so let's have an ultra weak prior
	print(myRacename)
	priorTlap = with(race2016, tlapPrior[race2016$racename == myRacename])
	tyreUsageStats = lbl %>%
				filter(racename == myRacename & isvalid) %>%
				group_by(tyre, insample) %>%
				summarise(numOb = n()) %>%
				ungroup() %>%
				group_by(tyre) %>%
				summarise(numObLabel = paste('(', paste(numOb, collapse = ','), ')', sep = ''))

	priorScale = list(driver = 0.1,
						fuel = 0.1,
						tyre = 0.1,
						tlap = 0.1)
	raceInfo = FitSingleRace2(myRacename, priorScale = priorScale, crossValidStatus = 'cv')

	# make nice array of coefs
	raceInfo$sampleMleList[[1]]$tyre$insample = 1
	raceInfo$sampleMleList[[2]]$tyre$insample = 2
	raceInfo$sampleMleList[[1]]$tlap$insample = 1
	raceInfo$sampleMleList[[2]]$tlap$insample = 2
	
	tyreSampleCoef = with(raceInfo, rbind(sampleMleList[[1]]$tyre,
											sampleMleList[[2]]$tyre))
	tlapSampleCoef = with(raceInfo, rbind(sampleMleList[[1]]$tlap,
											sampleMleList[[2]]$tlap))

	tyreTlapSampleCoef = left_join(tyreSampleCoef,
									tlapSampleCoef,
									by = c('tyre', 'insample'))

	tlapTyreCombo = expand.grid(tlap = 1:30,
								tyre = unique(tyreTlapSampleCoef$tyre),
								insample = 1:2,
								stringsAsFactors = FALSE)
	
	tlapTyreCombo = left_join(tlapTyreCombo,
								tyreTlapSampleCoef,
								by = c('tyre', 'insample'))
	
	tlapTyreCombo = tlapTyreCombo %>%
						mutate(predSec = tyreCoef + tlapCoef * tlap)
	
	tlapTyreCombo = left_join(tlapTyreCombo,
								tyreUsageStats,
								by = 'tyre') %>%
								mutate(tyreLabel = paste(tyre, numObLabel))
	
	minYLim = pmin(-2, min(tlapTyreCombo$predSec))
	maxYLim = pmax(6, max(tlapTyreCombo$predSec))
	
	myPlot = ggplot(tlapTyreCombo, 
					aes(x = tlap,
						y = predSec,
						col = tyreLabel,
						linetype = as.factor(insample))) +
				geom_line() +
				ggtitle(myRacename) +
				ylim(c(minYLim, maxYLim))
	
	# now add on the prior line

	myPlot = myPlot + geom_abline(intercept = 0, slope = priorTlap, size = 1)
	
	myPlot
	
	# but what this doesn't show is what the priors were coming up with as an alternative, really need to add those to know if they were doing what we wanted
	
	# also need to display which things actually happened, i.e. not really a problem if tyres are only used up to ten laps
}

PlotTyreIssue(sample(race2016$racename,1))

### my current thinking: the prior just seems to be bad, that's why it's not wanted. And if you think about it, the main situations when a tyre isn't used is when it is very different (ie worse) than the other two - the exact situation where the prior will be rubbish
### so i think a better solution might be to go with whatever prior slopes are suggested by the model, but think about highlighting (and possibly recalculating - that might be too complicated though) when a driver coef has been based on a big extrapolation


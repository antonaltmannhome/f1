### have decided that treating phase1 and phase2 at the same time is silly, they should be divided into separate jobs - once we have a fuel prior and a tlapprior for every race, that will make phase 2  feel much simpler

## so let's do just phase 1 for now

PROJECTPATH = paste(USERPATH, 'project/tyreprior jan2018/', sep = '')
source(paste(PROJECTPATH, 'tyreprior_data_setup.r', sep = ''))
source(paste(PROJECTPATH, 'tyreprior_funct.r', sep = ''))

# let's firstly investigate which races might actually be affected by having a strong fuel prior - does it almost always produce what the prior suggests in any case?

CheckRace = function(myRacename, fuelPriorScale, verbose = FALSE) {
	
	myrr = which(racedb$racename == myRacename)
	priorScale = list(driver = 0.1, tyre = 0.1, tlap = 0.1, fuel = fuelPriorScale)
	myRaceOutput = FitSingleRace(myRacename,
								priorScale = priorScale,
								phase12 = 1,
								crossValidStatus = 'cv')

	fuelSummary = c(racedb$fuelprior[myrr],
						myRaceOutput$sampleMleList[[1]]$fuel,
						myRaceOutput$sampleMleList[[2]]$fuel)

	meanSqDiff = myRaceOutput$meanSqDiff
						
	if (verbose) {
		cat('Fuel prior:', fuelSummary[1], '\n')
		cat('Fuel Mle, sample 1:', fuelSummary[2], '\n')
		cat('Fuel Mle, sample 2:', fuelSummary[3], '\n')
	}
	
	return(list(fuelSummary = fuelSummary, meanSqDiff = meanSqDiff))
}

SingleRaceOptimise = function(myRacename) {
	.GetMeanSqDiff = function(theta) {
		meanSqDiff = CheckRace(myRacename, fuelPriorScale = exp(theta))$meanSqDiff
		return(meanSqDiff)
	}
	maxInfo = optimise(.GetMeanSqDiff, interval = c(log(1), log(10000000)))
	optimalFuelPrior = exp(maxInfo$min)
	return(optimalFuelPrior)
}

# this is highly interesting, the prior seems to be either nothing or everything. There are some occasions that the prior just looks to be wrong - why would this be?

# it's not that big a job to bootstrap the fuel estimates, that would tell us when the fuel prior is categorically wrong
# don't need any of this prior stuff to do that in fact

BootstrapFuelTyreCoef = function(myRacename, numSample) {
	thisRaceLbl = lbl %>%
				filter(racename == myRacename & isvalid) %>%
				select(driver, fuel2, tyre, tlap2, sec)
	mod30PriorDF = rddf %>% 
					filter(racename==myRacename & !is.na(startinggrid)) %>%
					select(driver, mod30mixqrsmdcoef)
	# NB won't actually use this effectively
	myFuelPrior = with(racedb, fuelprior[racename == myRacename])

	priorValue = list(fuel = myFuelPrior,
						tyre = 0,
						tlap = 0.1)
	priorScale = list(driver = 0.1, fuel = 0.1, tyre = 0.1, tlap = 0.1)
	
	sampleFuelCoef = rep(NA, numSample)
	for (samplei in 1:numSample) {
		thisRaceLbl$useInModel = sample(c(TRUE, FALSE), nrow(thisRaceLbl), replace = TRUE)
		
		myMleDF = SolveRaceMatrix(thisRaceLbl %>% filter(useInModel),
									mod30PriorDF,
									priorValue,
									priorScale,
									phase12 = 1)
		
		sampleFuelCoef[samplei] = with(myMleDF, coefvalue[coefType == 'fuel'])
	}
	# now compare to the prior
	myXlim = range(c(myFuelPrior, sampleFuelCoef))
	hist(sampleFuelCoef, xlim = myXlim, main = myRacename)
	abline(v = myFuelPrior, col = 'red')
	
	return(sampleFuelCoef)
}

# very interesting, some of them are miles out. let's build up a list of them to see if there's any observable understandable pattern
sampleFuelCoefMat = matrix(NA, nrow = nrace, ncol = 100)
for (ri in which(racedb$isvalidrace30 == 1)) {
	sampleFuelCoefMat[ri,] = BootstrapFuelTyreCoef(racedb$racename[ri], 100)
}

FitAllRace = function(theta, phase12) {
	priorScale = list(driver = exp(theta[1]), tyre = 0.1, tlap = 0.1, fuel = exp(theta[2]))
		
	print('Prior scale:')
	print(paste(priorScale, collapse = ' '))

	if (any(exp(theta) > 10E7)) return(10E6)
	
	allRaceMeanSqDiff = rep(NA, length(allValidRace))
	for (j in 1:length(allValidRace)) {
		allRaceMeanSqDiff[j] = FitSingleRace(allValidRace[j], priorScale, phase12, crossValidStatus = 'cv')
	}

	meanMeanSqDiff = mean(allRaceMeanSqDiff)
	cat('Squared diff:', meanMeanSqDiff, '\n')
	return(meanMeanSqDiff)
}

maxInfo1 = nlm(FitAllRace, p = c(log(10),log(100)), phase12 = 1)

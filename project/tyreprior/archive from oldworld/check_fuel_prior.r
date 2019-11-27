### let's do the simpler version of getting mean and standard deviation of the fuel coefficient


PROJECTPATH = paste(USERPATH, 'development/tyreprior jan2018/', sep = '')
source(paste(PROJECTPATH, 'tyreprior_data_setup.r', sep = ''))
source(paste(PROJECTPATH, 'tyreprior_funct.r', sep = ''))
# coefs, some appear not to be very good

### now we can find out which ones are implausible and in what direction
raceDifferenceDF = with(racedb,
					tibble(racename = racename,
							circuit = circuit,
							yr = yr,
							fuelprior = fuelprior))

GetRaceFuelCoef = function(myRacename) {
	mod= lm(sec ~ factor(driver) + fuel2 + factor(tyre) * tlap2,
				data = lbl %>% filter(racename == myRacename & isvalid))
	fuelCoefIndex = which(rownames(summary(mod)$coef) == 'fuel2')
	fuelCoef = summary(mod)$coef[fuelCoefIndex, c('Estimate', 'Std. Error')]
	return(fuelCoef)
}
raceDifferenceDF[,c('fullLmMLE', 'fullLmStdDev')] = NA
for (ri in which(racedb$isvalidrace30 == 1)) {
	raceDifferenceDF[ri, c('fullLmMLE', 'fullLmStdDev')] = GetRaceFuelCoef(racedb$racename[ri])
}

## now i suppose what we're trying to do is predict the fuel coefficient each time. can we do a bayesian thing
## it could work like this: for every race, get hold of legal prior and past fuel estimates.
## compute the precision of past fuel estimates with variance formula
## then for race being looked at, take a bunch of training laps, and see how much it wants to weight the past estimates, the prior and the data to predict the test laps

# so let's do that

# let's just get familiar with what we'll be using

# let's pick out a single race, e.g bahrain 2013. Ignore the fact prior is cheaty for now, let's just get going with a simple example

### need to modify FitSingleRace to suit this particular experiment

trainTestRatio = 0.5
myRacename = '2013monaco'
myLbl = lbl %>%
		filter(racename == myRacename & isvalid) %>%
		rowwise() %>%
		mutate(insample = sample(1:2, 1, replace = TRUE, prob = c(trainTestRatio, 1 - trainTestRatio))) %>%
		select(driver, fuel2, tyre, tlap2, sec, insample) %>%
		mutate(tyre = 'combinedTyre')

GetPastRaceFuelPriorInfo = function(myRacename) {
	pastRaceName = with(racedb,
		racename[which(circuit == circuit[which(racename == myRacename)] &
						yr < yr[which(racename == myRacename)] &
						isvalidrace30)])
	pastFuelLmInfo = raceDifferenceDF %>%
						filter(racename %in% pastRaceName) %>%
						select(fullLmMLE, fullLmStdDev) %>%
						mutate(fullLmVariance = fullLmStdDev ^ 2,
								fullLmPrecision = 1 / fullLmVariance)
	pastFuelPrior = with(pastFuelLmInfo, weighted.mean(fullLmMLE, fullLmPrecision))
	pastFuelPrecision = sum(pastFuelLmInfo$fullLmPrecision)
	
	return(list(prior = pastFuelPrior,
				precision = pastFuelPrecision))
}

FitSingleRace = function(myRacename, myLbl, myPastRaceWgtMult) {

	mod30PriorDF = rddf %>% 
					filter(racename==myRacename & !is.na(startinggrid)) %>%
					select(driver, mod30mixqrsmdcoef)

	pastRaceFuelPriorInfo = GetPastRaceFuelPriorInfo(myRacename)

	priorValue = list(fuel = pastRaceFuelPriorInfo$prior,
						tyre = 0,
						tlap = 0.1)
	priorScale = list(driver = 1,
						fuel = pastRaceFuelPriorInfo$precision * myPastRaceWgtMult,
						tyre = 0.1,
						tlap = 0.1)
	
	samplei = 1
	myMleDF = SolveRaceMatrix(myLbl %>%
								filter(insample == samplei),
								mod30PriorDF,
								priorValue,
								priorScale,
								phase12 = 1)
	myMleList = TidyMleDF(myMleDF, mod30PriorDF)

	# right, how well has that fitted the test sample?
	myLbl = myLbl %>%
			mutate(interceptCoef = NA,
					driverCoef = NA,
					tyreCoef = NA,
					tlapCoef = NA,
					fuelCoef = NA)
	myLbl = JoinLblToCoef(myLbl, myMleList, samplei)

	myLbl = myLbl %>%
					mutate(predSec = interceptCoef + driverCoef + fuelCoef * fuel2 + tyreCoef + tlapCoef * tlap2)

	myLbl$sqDiff = with(myLbl, (predSec - sec)^2)
	meanSqDiff = with(myLbl, mean(sqDiff[insample == 2]))

	## display fitted fuel and tyre coefs just out of interest
	print(myMleList$fuel)
	print(myMleList$tlap)
	
	return(meanSqDiff)
}

### hmmm, think we might have a horrible problem to solve: that when we want to use the fuel prior, such as wet/drying tracks, then that's when it's least reliable, because the track is also evolving more quickly than usual

### also i don't think you can fix the fuel prior very hard but still let the tlap coef be weak, if you've only got a handful of laps the tlap coef will be all over the place

### so maybe we should use makeoveralldeg in conjunction with all of this
### but i'm a bit bored looking at this for now, let's take a break from it

### looking at wrong problem: we're trying to solve situations such as malaysia 2012 or singapore 2017, but even if you do everything suggested here, you've still got the horrible problem of track evolution which could be all over the place. just not the right priority, leave this

### check on the validity of the fuel prior 

PROJECTPATH = paste(USERPATH, 'development/tyreprior jan2018/', sep = '')
source(paste(PROJECTPATH, 'tyreprior_data_setup.r', sep = ''))
source(paste(PROJECTPATH, 'tyreprior_funct.r', sep = ''))
# coefs, some appear not to be very good


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

### now we can find out which ones are implausible and in what direction
raceDifferenceDF = with(racedb,
					tibble(racename = racename,
							circuit = circuit,
							yr = yr,
							fuelprior = fuelprior))
raceDifferenceDF = raceDifferenceDF %>%
					mutate(raceMean = apply(sampleFuelCoefMat, 1, mean),
							raceSD = apply(sampleFuelCoefMat, 1, sd),
							priorProb = pmin(pnorm(fuelprior, raceMean, raceSD),
											1 - pnorm(fuelprior, raceMean, raceSD)),
							priorSide = case_when(priorProb > 1E-03 ~ 'ok',
											priorProb < 1E-03 & fuelprior < raceMean ~ 'low',
											priorProb < 1E-03 & fuelprior > raceMean ~ 'high'))

### quick aside: am i just being silly here, could i have found all that out just using lm?

GetRaceFuelCoef = function(myRacename) {
	mod= lm(sec ~ factor(driver) + fuel2 + factor(tyre) * tlap2,
				data = lbl %>% filter(racename == myRacename & isvalid))
	fuelCoefIndex = which(rownames(summary(mod)$coef) == 'fuel2')
	fuelCoef = summary(mod)$coef[fuelCoefIndex, c('Estimate', 'Std. Error')]
	return(fuelCoef)
}
raceDifferenceDF[,c('fullLmEst', 'fullLmStdDev')] = NA
for (ri in which(racedb$isvalidrace30 == 1)) {
	raceDifferenceDF[ri, c('fullLmEst', 'fullLmStdDev')] = GetRaceFuelCoef(racedb$racename[ri])
}

## yes, almost the same, oops
	
ggplot(raceDifferenceDF, aes(x = yr, y = factor(circuit))) + geom_tile(aes(fill = priorSide))

# can do better
raceDifferenceDF = raceDifferenceDF %>%
					mutate(priorQuant = pnorm(fuelprior, raceMean, raceSD))

ggplot(raceDifferenceDF, aes(x = yr, y = factor(circuit))) + geom_tile(aes(fill = priorQuant))

### can't quite get that right, need to be able to tell the ridiculos ones from the just high ones, need better transformation than prob. but i'm getting a little bored, let's look at something else

## but just want to look at one quick thing, do we always underestimate the fuel effect at dry -> wet races? you would expect lap times to increase over and above fuelprior because of track evolution

wetToDry = GetChangeableRace() %>% filter(wetToDry)
wetToDry = left_join(wetToDry, raceDifferenceDF)
# no, it's not that clear at all. in fact, it's bollox because the tyres could just soak up the improvement as well

# just one more thing, look at average difference by circuit
aveDeltaByCircuit = racedb %>%
					filter(!is.na(fuelprior) & !is.na(mod30fuelcoef)) %>%
					mutate(post2014 = yr >=2014) %>%
					group_by(circuit, post2014) %>%
					summarise(meanPrior = mean(fuelprior),
								meanCoef = mean(mod30fuelcoef))
								
ggplot(aveDeltaByCircuit) +
	geom_point(aes(x = meanPrior, y = meanCoef)) +
	geom_text(label = paste(circuit, post2014))
### doesn't work. buyt anyway, i'm bored

### summary of situation: want to get tyre prior. but we can't yet be sure how good our tyre prior is because it relies on fuelprior being good, and it isn't always

### set up all the data needed for the tyre prior idea - this will eventually be done on database properly but let's just get the thing running for now

### and don't worry about prioring tyres coefs to be similar to each other, the phase1/phase2 approach seems to be a simpler way to do that
### does it make sense to optimise priors on phase1, store them, then do all of phae 2? much easier to optimise I'd have thought

dum = f1smoothing:::GetSmooth(qrToFit = 'qr', qrToPredict = 'r', fwbw = 'fwbw',
								modelChoice = 30, useStretch = TRUE)
rddf = lazy_left_join(rddf, dum$smoothDF, c('race', 'driver'), 'smoothDCoef')

lbl = f1validity:::MakeIsGoodPreValidRace(30, lbl, raceDF)

### so we need one vector for 'how many laps behind safety car in this race until this point, which we will use for fuel

lbl = lbl %>%
		group_by(race, driver) %>%
		mutate(cumSafetyCarLapInRace = cumsum(isSafetyCar)) %>%
		ungroup()
lbl = lbl %>%
		group_by(race, driver, stint) %>%
		mutate(cumSafetyCarLapInStint = cumsum(isSafetyCar)) %>%
		ungroup()

### values it comes up with are basically 1 for tyres, and -0.64 for fuel
lbl$fuel2 = lbl$fuel + 0.64 * lbl$cumSafetyCarLapInRace
lbl$tyreLap2 = lbl$tyreLap - lbl$cumSafetyCarLapInStint
message('Tyre lap adjustment looks suspicious, investigate it')

sax1013 = with(raceDF,which(year <= 2013))
sax14plus = with(raceDF,which(year >= 2014))
mod1013 = lm(raceDF$mod30FuelCoef[sax1013] ~ raceDF$perim[sax1013])
mod14plus = lm(raceDF$mod30FuelCoef[sax14plus] ~ raceDF$perim[sax14plus])
raceDF$fuelPrior = rep(NA,nrace)
raceDF$fuelPrior[sax1013] = predict(mod1013, data.frame(raceDF[sax1013,'perim']))
raceDF$fuelPrior[sax14plus] = predict(mod14plus, data.frame(raceDF[sax14plus,'perim']))

### what should these be? need to do some cross validation to find out

### let's have tyres in order of softness
untyre=c('hypersoft','ultrasoft','supersoft','soft','medium','hard')
untyreprior=0.2*(1:length(untyre)) - mean(0.2*(1:length(untyre)))
### but then want to rejig that by race to mean centre
dumf=function(myrr) {
	myuntyre = with(lbl,unique(tyre[rr == myrr & isGood30]))
	dum = rep(NA, length(untyre))
	dum[match(myuntyre, untyre)] = untyreprior[match(myuntyre,untyre)]
	dum = round(dum - mean(dum,na.rm = T), 2)
	return(dum)
}
tyrepriorarr = cbind(1:nrace,as.data.frame(t(sapply(1:nrace, dumf))))
names(tyrepriorarr) = c('rr',untyre)

lbl = lbl %>%
		mutate(inSample = sample(1:2, n(), replace = TRUE))

allValidRace = raceDF$race[raceDF$isValidRace30]

# i managed to get this running in old world:


LoadAllData()

PROJECTPATH = paste(USERPATH, 'project/tyreprior/', sep = '')
source(paste(PROJECTPATH, 'tyreprior_data_setup.r', sep = ''))
source(paste(PROJECTPATH, 'tyreprior_funct.r', sep = ''))

dum = FitSingleRace('2018bahrain', priorScale = list(driver = 10, tyre = 10, tlap = 10, fuel = 10), 2, 'cv')

# it looks promising, but maybe more complicated than what we need. also, we don't have lblupdate.preparedcoefmix(30) in new world

# but if you want to have only 1 tyre effectively, then you do this:
dum = FitSingleRace('2018bahrain', priorScale = list(driver = 10, tyre = 1000, tlap = 100000, fuel = 10), 2, 'no')
# while they're all free if you do this:
dum = FitSingleRace('2018bahrain', priorScale = list(driver = 10, tyre = 0.01, tlap = 0.01, fuel = 10), 2, 'no')
# here we have different tyre intercepts, but single slope:
dum = FitSingleRace('2018bahrain', priorScale = list(driver = 10, tyre = 0.01, tlap = 100000, fuel = 10), 2, 'no')

dum = FitAllRace(c(log(0.001), log(0.001), log(10), log(0.1)), 2)
[1] "Prior scale:"
[1] "driver: 0.001" "tyre: 0.001"   "tlap: 10"      "fuel: 0.1"    
Squared diff: 0.2803365 
dum = FitAllRace(c(log(0.001), log(0.001), log(100), log(0.1)), 2)
[1] "Prior scale:"
[1] "driver: 0.001" "tyre: 0.001"   "tlap: 100"     "fuel: 0.1"    
Squared diff: 0.2801456
dum = FitAllRace(c(log(0.001), log(0.001), log(1000), log(0.1)), 2)
[1] "Prior scale:"
[1] "driver: 0.001" "tyre: 0.001"   "tlap: 1000"    "fuel: 0.1"    
Squared diff: 0.28106 
# ok, this is not agreeing with what i was finding with new thing, this seems to want separate tyre intercept and slope. this feels less dodgy overall though because of the non-rank deficeint problem. would like to get rid of drier priors and fuel priors, except to help identify

# so this should jsut fit the same thing as lm, yes?
prioredLM = FitSingleRace('2018bahrain',
			priorScale = list(driver = 0.001, tyre = 0.001, tlap = 0.001, fuel = 0.001), 2, 'no')

			
thisRaceLbl = lbl %>%
				filter(racename == '2018bahrain' & isvalid) %>%
				select(driver, fuel2, tyre, tlap2, sec)
mod = lm(sec ~ factor(driver) + fuel2 + factor(tyre) * tlap2, data = thisRaceLbl)

# yes, that seems to be the case. this looks really good- let's make it work in new world
# weird looking line in FitSingleRace: 
if (phase12 == 2) {
		myTlapPrior = with(raceDF, fuelPrior[race == myRace])
}
# surely that should be tyreprior from stage 1?

### surely it should. but we don't have tyreprior to start with, have to get it via stage 1, so let's do that

raceDF[,c('tlapPrior1', 'tlapPrior2')] = NA
for (ri in which(raceDF$isValidRace30)) {
	dum = FitSingleRace(raceDF$race[ri], priorScale = list(driver = 0.001, tyre = 0.001, tlap = 0.001, fuel = 0.001), 1, 'cv')
	raceDF$tlapPrior1[ri] = dum$sampleMleList[[1]]$tlap$tlapCoef
	raceDF$tlapPrior2[ri] = dum$sampleMleList[[2]]$tlap$tlapCoef
}

dum = FitAllRace(c(log(0.1), log(0.1), log(0.1), log(0.1)), 2)

# rather than looking at all races at once, let's focus on a single rae that we think might be vulnerable
# eg. abu dhabi 2018


dum = FitSingleRace('2018abudhabi', priorScale = list(driver = 10, tyre = 10, tlap = 10, fuel = 10), 2, 'cv')
# so let's have a function that does the whole shebang for this race
ExamineRace = function(myRace) {
	# firstly do the naive lm for each half of the race, plus overall
	# make sure it's the most commonly used tyres that are by default used in the lm
	tyreCount = lbl %>%
					filter(race == myRace & isGoodPreValidRace) %>%
					count(tyre, inSample) %>%
					spread(key = inSample, value = n) %>%
					mutate(tyreCount = paste0(tyre, ' ', `1`, '/', `2`),
							totalLap = `1` + `2`)
	tyreLevel = with(tyreCount, tyre[order(-totalLap)])
	mod1 = lm(sec ~ factor(driver) + fuel2 + factor(tyre, levels = tyreLevel) * tyreLap2 - 1,
				data = lbl %>% filter(race == myRace & isGoodPreValidRace & inSample == 1))
	mod1TyreCoefDF = ExtractTyreCoefFromMod(mod1)
	mod2 = lm(sec ~ factor(driver) + fuel2 + factor(tyre, levels = tyreLevel) * tyreLap2 - 1,
				data = lbl %>% filter(race == myRace & isGoodPreValidRace & inSample == 2))
	mod2Coef = ExtractTyreCoefFromMod(mod2)
	mod = lm(sec ~ factor(driver) + fuel2 + factor(tyre, levels = tyreLevel) * tyreLap2 - 1,
				data = lbl %>% filter(race == myRace & isGoodPreValidRace))
	modCoef = ExtractTyreCoefFromMod(mod)
	
	combinedModTyreCoef = bind_rows(mod1Coef$tyreCoefDF %>% mutate(inSample = 1),
								mod2Coef$tyreCoefDF %>% mutate(inSample = 2),
								modCoef$tyreCoefDF %>% mutate(inSample = 3))
	myUnTyre = with(lbl, unique(tyre[race == myRace & isGoodPreValidRace]))
	allTyreLapDF = expand.grid(tyre = myUnTyre, tyreLap2 = 1:30, inSample = 1:3,
								stringsAsFactors = FALSE)
	allTyreLapDF = left_join(allTyreLapDF,
								combinedModTyreCoef,
								c('tyre', 'inSample'))
	allTyreLapDF$effect = with(allTyreLapDF, int + slo * tyreLap2)
	
	# but we want to join in the info about number of laps into the tyre label, we can do that here
	allTyreLapDF = lazy_left_join(allTyreLapDF, tyreCount, 'tyre')
	
	ggplot(allTyreLapDF) +
		geom_line(aes(x = tyreLap2, y = effect, col = tyreCount, linetype = as.factor(inSample)))
		
	
	myYlim = range(
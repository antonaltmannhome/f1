### let's take a look at the variance covariance matrix from doing lom. what if we have a race where there were two almost distinct groups of drivers using dcertain tyres?

## let's take an actual rae and tamper with the tyre chocies

LoadAllData()

dum = MakeIsGood(4, lbl, raceDF)
lbl = dum$lbl
raceDF = dum$raceDF

myLbl = lbl %>% filter(race == '2018japan')

undriv = unique(myLbl$driver)

ontyre = sample(c('jelly', 'glue'), length(undriv), replace = TRUE)

# so firstly, have no overlap , that should throw us an NA

myLbl$faketyre = ontyre[match(myLbl$driver, undriv)]

mod = lm(sec ~ factor(driver) + fuel + factor(faketyre) * tyreLap,
			data = myLbl %>% filter(isGood))

# indeed, an NA for tyre factor

## so let's have one driver using both
myLbl = myLbl %>%
			mutate_cond(driver == 'sperez',
							faketyre = 
							case_when(stint == 1 ~ 'jelly',
										stint == 2 ~ 'glue'))
										
mod = lm(sec ~ factor(driver) + fuel + factor(faketyre) * tyreLap - 1,
			data = myLbl %>% filter(isGood))
vcovmat = vcov(mod)
# let's turn that into a nice df
drivix = grep('driver', rownames(vcovmat))
vcovdf = data.frame(pairvcov = as.numeric(vcovmat[drivix, drivix]))
vcovdf$driver1 = rep(rownames(vcovmat)[drivix], rep(length(drivix), length(drivix)))
vcovdf$driver2 = rep(rownames(vcovmat)[drivix], length(drivix))
vcovdf$driver1 = gsub('^.+\\)', '', vcovdf$driver1)
vcovdf$driver2 = gsub('^.+\\)', '', vcovdf$driver2)

### ok, now let's check the vcovs of driver depending on which group they were in

viewdriver = function(mydriv = NULL) {
	if (is.null(mydriv)) {
		mydriv = sample(undriv, 1)
	}
	print(mydriv)
	### vcovs of driver using same tyre:
	myvcovdf = vcovdf %>% filter(driver1 == mydriv)
	thisdrivtyre = ontyre[undriv == mydriv]
	myvcovdf$sametyre = ontyre[match(myvcovdf$driver2, undriv)] == thisdrivtyre
	
	ggplot(myvcovdf %>% filter(driver2 != mydriv)) +
		geom_point(aes(x = driver2, y = pairvcov, col = sametyre)) +
		theme(axis.text.x = element_text(angle = 90))
}

### yes, it's pretty overwhelming, it's only perez whose are the wrong way around but that's because the code's not been prperly written for that.
### probably no need for MCMC then!
### we can presumably apply this to the series ranking thing

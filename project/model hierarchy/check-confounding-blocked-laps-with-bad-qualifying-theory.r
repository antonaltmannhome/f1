### so we're finding that we want to include blocked laps as well as clear laps in order to get the best model. let's dig deeper into that in this file rather than cluttering up sequential-model-run even further. we see crazy values in the numobwgt thing as well. maybe it's confounded with bad qualifying? lots of blocked laps => you qualified slower than your race pace => you will likely sturggle in future races to convert pace into finishing position. let's look into these things

source('project/model hierarchy/model-hierarchy-setup.r')

## let's set up the two models in question


lbl$isValidBOC  = with(lbl, !isRogue & !isOvertaking & isValidRace30)
rddf$predNValidBOC = MakePredNValidForLblValid('isValidBOC', rddf, lbl)

lbl$isValidClearLap = with(lbl, !isRogue & !inTraffic & isValidRace30)
rddf$predNValidClearLap = MakePredNValidForLblValid('isValidClearLap', rddf, lbl)

rddf = RunModelAllRace('fuelTyreTypeAndAge',
							'BOC',
							rddf, lbl)
rddf$dCoefBOC = rddf$dCoef
rddf$efpBOC = rddf$efp

rddf = RunModelAllRace('fuelTyreTypeAndAge',
							'clearLap',
							rddf, lbl)
rddf$dCoefClearLap = rddf$dCoef
rddf$efpClearLap = rddf$efp

### now, what we want to know is, is the BOC actually just finding drivers who've qualified badly? Is there a consistent relationship between drivers who are predicted to be better under BOC and qualifying better than you'd expect?

# so, firstly let's estimate qualifying position from model:

rddf = f1laptimelm:::MakeNormDriverCoef(rddf, raceDF, 30, 'race')
mod = lm(rddf$startingGrid ~ rddf$mod30DCoef)

rddf$predictedQualPos = predict(mod, rddf)
# that is rubbish.

# let's just look at some of the bigger differences
# so these are the ones with repeated big differences:

# A tibble: 11 x 3
    year driver         n
   <int> <chr>      <int>
 1  2010 fmassa        18
 2  2010 nheidfeld      4
 3  2012 bsenna        13
 4  2012 jvergne       13
 5  2012 pmaldonado     1
 6  2012 sperez        14
 7  2013 kraikkonen    15
 8  2016 dkvyat         7
 9  2017 bhartley       1
10  2017 dkvyat         3
11  2018 bhartley      16

# interesting, what could have caused that. let's look into some of the main ones
# there are definitely examples of drivers who raced better than they qualified in that list.


CheckDriver = function(myYear, myDriver) {
	myDF = rddf %>%
			filter(year == myYear & driver == myDriver) %>%
			select(race, predNValidClearLap, dCoefClearLap, efpClearLap, predNValidBOC, dCoefBOC, efpBOC, sqDiffFuelTyreTypeAndAgeClearLap, sqDiffFuelTyreTypeAndAgeBOC) %>%
			lazy_left_join(raceDF, 'race', 'daynum')
	
	# let's print the difference in sqdiff, it might just be interesting
	message('meanSqDiff for ClearLap: ', mean(myDF$sqDiffFuelTyreTypeAndAgeClearLap, na.rm=T))
	message('meanSqDiff for BOC: ', mean(myDF$sqDiffFuelTyreTypeAndAgeBOC, na.rm=T))
	
	myVertDF = myDF %>%
				select(daynum, dCoefClearLap, dCoefBOC) %>%
				gather(model, dCoef, -daynum)
	
	ggplot(myVertDF %>%
		filter(!is.na(dCoef))) +
		geom_point(aes(x = daynum, y = dCoef, fill = model), shape = 21, col = 'black', size = 3)
	
	return(myDF)
}

rddf = rddf %>%
		group_by(race) %>%
		mutate(paceRank = rank(mod30DCoef)) %>%
		ungroup()

paceRankByYear = rddf %>%
					group_by(year, driver) %>%
					summarise(medianRacePace = median(paceRank, na.rm = TRUE),
								medianQual = median(startingGrid, na.rm = TRUE))

rddf2 = left_join(rddf,
					paceRankByYear,
					c('year', 'driver'))

with(rddf2, calibplot(efpBOC - efpClearLap, medianQual - medianRacePace))
## VERY clear relationship. think we've cracked it.
# so maybe we should modify our predictions to be allowed to include the starting position? that is significantly more complicated, just leave on backburner for now
# but it could throw doubt on all the other results. probably not worth looking into the likelihood thing e.g. until we're on top of this

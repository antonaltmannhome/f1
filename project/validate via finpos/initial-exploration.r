### how do we convert paace into expected finishing position? betabinomial is nice but hard to explain, let's investigate simple ideas
LoadAllData()

dum = f1smoothing:::GetSmooth(method = 'downweight',
							qrtofit = 'r',
							qrtopredict = 'r',
							fwbw = 'fwbw',
							modelchoice = 30,
							usecurrentrace = FALSE,
							usestretch = TRUE,
							myRace = raceDF$race[nrace])

rddf[,c('smval', 'numobval')] = cbind(dum$smval, dum$numobval)

# get rid of 2010 data, so many outliers when they couldn't overtake

# need to factor in grid position, car problem, penalties, problems after start/safety cars
dbb=function(x,nn,aa,bb) choose(nn,x)*beta(x+aa,nn-x+bb)/beta(aa,bb)
with(rddf %>% filter(year > 2010),
	calibplot(smval, officialFinishingPosition,
				ylim = c(0, 24)))
# still looks weird on the left, is that outliers?

# think we might need to do outliers before we can trust any output

# there are several things to work through here
# draw up list of things that cause outliers
# code them up
# get likelihood for default smooth above using curve
# get likelihood for default smooth above using betabinomial
# tweak pace ratings so they're normed to drivers who 'finished' the race (ie not outliers)
# compare when you include qualifying
# compare when you add in the team mate stuff from yearly smooth

# things that cause outliers:
# unscheduled pit stop
# dropping to the back of the field on first lap or SC restart
# just losing lots of places on any lap?
# not setting time in a qualy session you were likely to have qualified well in
# getting major grid penalty
# slow pit stop
# slow lap
# significant car damage
# try those out, see how many outliers remain

# but you can have a one of the problems above but not be completely screwed...depends what proportion of data we're excluding, let's monitor that at each point

# shite, we don't have unscheduled pit stops in new world
interruptedPitStop = read.csv(file = paste0(USERPATH, 'project/validate via finpos/interrupted-stint.csv'), as.is = TRUE) %>%
	dplyr::rename(race = racename) %>%
	dplyr::rename(startLap = startlap)

# problem is, these might include safety cars, are we better off just detecting it from lsow injlaps and/or pit stops?
lbl = f1gaptrafficpitstop::AdjustLapTimeForPitStop(lbl)
lbl = f1laptimelm:::MakePredSec(lbl, 30)

# find drivers who retired in the race, then don't include their slow inlaps to the pits in the freak slow inlaps
rddf = rddf %>%
		mutate(isTime = grepl('[0-9]{1,2}\\.[0-9]{1,3}', classification),
				isLappedAtFinish = grepl('[0-9] Lap', classification),
				isRetirement = !(!is.na(officialFinishingPosition) &
									officialFinishingPosition == 1) &
									!isTime &
									!isLappedAtFinish)
retirementRddf = rddf %>%
				filter(isRetirement) %>%
				select(race, driver, maxLap) %>%
				rename(lap = maxLap)
lbl = lbl %>%
		indicate_overlapping_combination(
				retirementRddf,
				c('race', 'driver', 'lap'),
				'isRetirementLap')
lbl$isSlowInLap = with(lbl, inlap &
							impsec > mod30PredSec + 5 &
							(!isRetirementLap & !isSafetyCar & !isRed & !isWet))

# to view all examples from a given year:
lbl %>%
	filter(year == 2018 & isSlowInLap) %>%
	select(race, driver, lap, isSafetyCar, isRetirementLap, mod30PredSec, impsec)

### that seems sort of accurate.
### next, drivers who lose lots of places on lap 1 or on a safety car restart
lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE)

## here's a weird scenario: at azerbaijan 2018, raikkonen and perez pitted under SC at start and aatually gained a load of places. but we should be able to pick that up as long as it's at the start of the race, doing such an early stop under VSC is normally because of damage. unless it's a sauber e.g. so if a car is well placed and stops early under VSC, it's probably due to a problem
### in general it's hard for us to know that  a driver has a problem if they have a collision, cause a safety car, and pit and lose lots of time. need the simulations for that i'd have thought
## would help if we had an example of that happening
### tricky for safety cars, need to distinguish between VSCs and normal SCs

lbl = lbl %>%
		mutate(isFirstLapDisaster = lap == 1 &
									((startRank < 15 & inlap) |
									(endRank - startRank > 10)))
lbl = lbl %>%
		mutate(isFirstLapProblem = lap == 1 &
									(between(endRank - startRank, 6, 10)))

### this isn't brilliant, the midfeld's disasters aren't included, we can do much better
### but can we generalise to all laps, any lap where you lose loads of positions without pitting are surely a disaster

lbl = f1validity::MakeIsRogue(lbl)
lbl = lbl %>%
		mutate(isBadLap = !isRogue &
						(endRank - startRank > 5 & impsec - mod30PredSec > 7))

# mmm, we really need the simulations for this
rdlFile = paste0(USERPATH, 'project/validate via finpos/race-driver-lap-simulation.csv')
raceDriverLapSimulation = read.csv(rdlFile, as.is = TRUE) %>%
							rename(race = racename) %>%
							rename(meanFinPos = mod34meanfinpos)

lbl = left_join(lbl, raceDriverLapSimulation, c('race', 'driver', 'lap'))
### ok, now we might have more joy with that

dum = lbl %>%
		group_by(race, driver) %>%
		arrange(lap) %>%
		mutate(mfDelta = c(NA, diff(meanFinPos)))

lbl = lazy_left_join(lbl, dum, c('race','driver','lap'), 'mfDelta')

# that picks up a lot of needless rubbish like drivers developing car problems or retiring to pits, want to weed out those

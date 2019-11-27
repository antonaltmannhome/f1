### detecting messy races is dragging a little, let's crack on, ignoring messy races

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

source('project/validate via finpos/messy-race-funct.r')

dum = MakeRetirementLap(rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl

# need to factor in grid position, car problem, penalties, problems after start/safety cars
dbb=function(x,nn,aa,bb) choose(nn,x)*beta(x+aa,nn-x+bb)/beta(aa,bb)
with(rddf %>% filter(year > 2010 & !isRetirement),
	calibplot(smval, officialFinishingPosition,
				ylim = c(0, 24)))
# still looks weird on the left, is that outliers?
### hmm, this is swung by the presence of HRT etc. maybe time difference to fasterst driver is better

rddf = rddf %>%
		group_by(race) %>%
		mutate(smval2 = smval - min(smval, na.rm = TRUE)) %>%
		ungroup()
with(rddf %>% filter(year > 2010 & !isRetirement),
	calibplot(smval2, officialFinishingPosition,
				ylim = c(0, 24)))

### let's make the line of best fit

### initial exploration got a bit complicated, let's try again

LoadAllData()

lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE)
lbl = f1gaptrafficpitstop::AdjustLapTimeForPitStop(lbl)
lbl = f1laptimelm:::MakePredSec(lbl, 30)
lbl = f1validity::MakeIsRogue(lbl)

source('project/validate via finpos/messy-race-funct.r')

dum = MakeRetirementLap(rddf, lbl)
rddf = dum$rddf
lbl = dum$lbl
lbl = lazy_left_join(lbl, rddf, c('race', 'driver'), 'isRetirement')

rddf = DetectSlowPitStop(pitStopDF, rddf)
rddf = DetectVerySlowLap(rddf, lbl)
rddf = DetectFirstLapIssue(rddf, lbl)
rddf = AlignCarProblem(rddf, carProblemDF)
rddf = DetectSimulationIssue(rddf, lbl)

									
### there are a few that seem a bit out - e.g if a driver is blocked for a few laps, then does a fairly slow lap, it lurches. we need antoher run of the sims, one that just takes smval as its input - but we haven't got time for that, let's crack on with what we've got

# in fact, i'd say our own little rules are better than the sims generally

rddf$cleanRace = with(rddf, !(isRetirement | hadSlowPitStop | hadVerySlowLap |
								hadFirstLapDisaster | hadFirstLapProblem |
								hadCarProblem))

# edge cases found so far: Alonso & Hulk slowed right down at end of bahrain 2018, good sims would tell us that's not a problem though

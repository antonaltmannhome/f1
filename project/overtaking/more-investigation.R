
# here's an interesting example, look
lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE, includeIntermediateColumn = T)
lbl %>% filter(race=='2018australia' & lap == 10) %>% select(driver, startTimeElapsed, impsec, SOLGap, EOLGap,SOLGapToSOLDA,EOLGapToSOLDA,SOLGapToEOLDA,EOLGapToEOLDA) %>% arrange(startTimeElapsed)

# so in this example, we should ignore verstappen from the lap. grosjean was really limited by magnussen
# in what situation would you prefer to look at anything other than EOLGAPToEOLDA?

# this has all been thought about already. i think we just need to redefine startaheadoutlier, endaheadoutlier very broadly, then paste in the revisit-blocked-laps stuff

# what about, e.g mag +5s ric + 1s gros
# then mag makes mistake, end of lap it's ric+0.5s mag +0.5s gros.
# what is gros's limiting lap?

# ric does 80s so ignoring mag, it's 79
# mag does 85.5 so ignoring ric it's 79.5
# so 79.5 is the limit? yes clearly
# but again we're just looking at the end driver

# no, let's say we have this scenario:
# lap 5:
# driver   delta   sec
# bot       -       80
# ric      +2       83
# hul      +2.5     81
# so from hulk's point of view, he might have overtaken ric at last corner, so limit was 82.5
# but he might have overtaken him at first corner, tso it was 77.5
# so limit is naturally 82.5 ie. pmax as in the code
# but let's say instead that we had this scenario:
# lap 5:
# driver   delta   sec
# bot       -       80
# ric      +2       100
# hul      +2.5     81
# hulk clearly wasn't limited by 100s even though he might have been behind ricciardo until the last corner
# in this case we just can't tell. maybe we should just exclude it from analysis in this scenario?
# that would mean that if a driver loses e.g 10 places through a spin, all ten drivers have their laps ignored
# not really a problem? we're looking for very broad effects here so it doesn't matter
# so we're proposing to exclude any driver who overtook a driver who had an outliying lap? let's see how often that actually happens

# there are two different things we want:
# 1. the df of possible overtakings: the conversation above was about that, so ignore secLimit
# 2. the df for blocked laps, which is just a subset of lbl. we need secLimit for that
# for that one, these are the rules:
# so  no overtaking, driver ahead a long way ahead, not outlier: secLimit = SOLSec - SOLGap
#     no overtaking, driver ahead a long way ahead, yes outlier: secLimit = SOLSec - SOLGap (will be irrelevant anyway)
#     no overtaking, driver ahead a bit ahead, not outlier: secLimit = SOLSec - SOLGap
#     no overtaking, driver ahead a bit ahead, yes outlier: never happens
#     yes overtaking, driver ahead a long way ahead, not outlier: never happens
#     yes overtaking, driver ahead a long way ahead, yes outlier: ignore driver ahead
#     yes overtaking, driver ahead a bit ahead, not outlier: secLimit = pmax(SOLDASecLimit, EOLDASecLimit)
#     yes overtaking, driver head a bit ahead, yes outlier: don't ignore driver ahead becuase he might have blocked you until outlying
        # so, SOLDASecLimit = SOLDAPredSec + GOTOTOutlierCutoff
        # secLimit = pmax(SOLDASecLimit, EOLDASecLimit)
# why does all of this matter? We want to
  # calculate cost of doing an overtaking, but want to filter out rogue laps. We can be quite brutal filtering out outliers. do we even care about secLimit for that?
  # calculate likely lap time in the event of not overtaking. that is sensitive to outliers, so if drier ahead was an outlier, just get rid surely
  # calculate cost of getting overtaken. again, be brutla filtering out outliers
  # calculate overtaking probabilities. for that, we need to eliminate drivers who got overtaken but only because they were outliers, and drivers who didn't overtake but only because they were outliers

  # so validity::MakeGotOtIsGood is legitimate, it's needed for the

# so the process here is:
# we need the possibleOvertakingDF for the overtaking model. We need to indicate gotOt outliers and blocked outliers. So MakeGotOtIsGood is good, we also need MakeBlockedIsGood. Don't need MakeDidOtIsGood because if it wasn't the overtaking will be ruled out by MakeGotOtIsGood
# MakeBlockedOtISGood is much more complicated than MakeGotOtIsGood.

# we need to cover these scenarios: copied from above for clarity

# so  no overtaking, driver ahead a long way ahead, not outlier: secLimit = SOLSec - SOLGap
#     no overtaking, driver ahead a long way ahead, yes outlier: secLimit = SOLSec - SOLGap (will be irrelevant anyway)
#     no overtaking, driver ahead a bit ahead, not outlier: secLimit = SOLSec - SOLGap
#     no overtaking, driver ahead a bit ahead, yes outlier: never happens
#     no overtaking, driver ahead a bit ahead, pits: secLimit = SOLSec - SOLGap
# to summarise, EOLDA == SOLDA, secLimit = SOLDASec - SOLGapToSOLDriver
# but hang on, that's always equal to EOLDASec - SOLGapToEOLDriver, and i think that's what we almost always want
#     no overtaking, driver ahead a bit ahead, retires: secLimit = EOLDASec - SOLGapToEOLDA
#     no overtaking, drivers ahead are a bit ahead, swap positions: secLimit = EOLDASec - SOLGapTOEOLDA
# so we always want: EOLDASec - SOLGapTOEOLDA. which is normally the same as SOLDASec - SOLGapToSOLDA.
#     yes overtaking, driver ahead a long way ahead, not outlier: never happens
#     yes overtaking, driver ahead a long way ahead, yes outlier: ignore driver ahead
#     yes overtaking, driver ahead a bit ahead, not outlier: secLimit = pmax(SOLDASecLimit, EOLDASecLimit)
#     yes overtaking, driver head a bit ahead, yes outlier: don't ignore driver ahead becuase he might have blocked you until outlying
        # so, SOLDASecLimit = SOLDAPredSec + GOTOTOutlierCutoff
        # secLimit = pmax(SOLDASecLimit, EOLDASecLimit)

# so make secLimit accordingly. Then, if impsec > secLimit + blockedCutoff OR
#                                         impsec > mod30PredSec + blockedCutoff
# then it's a blocked outlier.
# but we don't want to fill the possibleOvertakingDF with irrelevant rubbish where the gaps are massive, so overlap with SOLDA should be <5. don't worry about overlap with EOLDA, that will be picked up by the other driver
# then something is admitted into possibleOvertaking if it satisfies isGoodGotOt AND isGoodBlocked

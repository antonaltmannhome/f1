# let's write the pittimedf to database in new world, and use interactive thing to update it from now on
oldpittimedf = read.csv('c:/research/f1/temporary transfer data/pittimedf.csv') %>%
        rename(race = racename,
            inlapLoss = illoss,
            outlapLoss = olloss,
            medianPitStopTime = medpstime,
            notPitTime = notpittime) %>%
        select(-c(rr))

oldpittimedf = oldpittimedf %>%
                mutate(inlapDelta = ifelse(inlapLoss > outlapLoss, inlapLoss - medianPitStopTime, inlapLoss),
                    outlapDelta = ifelse(outlapLoss > inlapLoss, outlapLoss - medianPitStopTime, outlapLoss),
                    numStandardInlap = nvalidstop,
                    numStandardOutlap = nvalidstop,
                    adjustInlapOrOutlapForPitTime = case_when(
                      inlapLoss > outlapLoss ~ 'inlap',
                      outlapLoss > inlapLoss ~ 'outlap')
                      )

raceDF = lazy_left_join(raceDF, oldpittimedf, 'race',
                      c('inlapDelta', 'outlapDelta', 'numStandardInlap', 'numStandardOutlap', 'adjustInlapOrOutlapForPitTime'))
raceDF$numStandardOutlap[is.na(raceDF$numStandardOutlap)] = 0
raceDF$numStandardInlap[is.na(raceDF$numStandardInlap)] = 0

raceDF$doneInlapOutlapDelta = TRUE
sqlLazyUpdate(raceDF, 'race', 'race', c('doneInlapOutlapDelta', 'numStandardInlap', 'numStandardOutlap'))
hasDelta = with(oldpittimedf, which(!is.na(inlapDelta)))
sqlLazyUpdate(raceDF[hasDelta,], 'race', 'race', c('inlapDelta', 'outlapDelta', 'adjustInlapOrOutlapForPitTime'))

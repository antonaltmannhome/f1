### quite a complicated process to check all the post-delta stuff, so let's have a nice file that steps through it all

# new world:
LoadAllData()

dum = MakeIsGood(4, lbl, raceDF)
raceDF = dum$raceDF
lbl = f1gaptrafficpitstop::GetGapAhead(lbl, isPreDelta = FALSE)

# old world:

write.csv(file = 'c:/research/f1/temporary transfer data/pittimedf.csv', pittimedf, row.names = F)
				
write.csv(file = 'c:/temp/temp.csv',lbl[,c('rr','driver','lap','impsec','starttelapse','endtelapse','starttgap','endtgap')],row.names= F)

### new world:

lbl2 = lazy_left_join(lbl, raceDF, 'race', 'isValidRace')
lbl2[which(!lbl2$isValidRace),c('impsec','startTimeElapsed','endTimeElapsed','startTimeGap','updateTimeGap')]=NA
b = read.csv('c:/temp/temp.csv')
lbl2 = left_join(lbl2, b, c('rr','driver','lap'))

plot(lbl2$impsec.x,lbl2$impsec.y,xlim = c(0,200), ylim = c(0, 200))

with(lbl2, which(abs(impsec.x - impsec.y) > 2))
## only edge case of oz 2016 red flag. all sorted
lbl2 %>% filter(is.na(impsec.x) & !is.na(impsec.y)) %>% count(race)
### jsut the shite races, all sorted

## let's look at startTimeElapsed now
plot(lbl2$startTimeElapsed,lbl2$starttelapse)
plot(lbl2$endTimeElapsed,lbl2$endtelapse)
lbl2 %>% filter(is.na(startTimeElapsed) & !is.na(starttelapse)) %>% count(race)
### same, all is fine
plot(lbl2$startTimeGap,lbl2$starttgap)
lbl2[with(lbl2, which(abs(startTimeGap - starttgap) > 1)),c('race','driver','lap')]
lbl2[with(lbl2, which(abs(updateTimeGap - endtgap) > 1)),c('race','driver','lap')]
# oz 2016

## have just 
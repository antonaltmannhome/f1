.F1Startup()
LoadAllData()
source('c:/research/f1/f1plot/R/driver-race.r')
myRace = '2018china'
myDriv1 = 'dricciardo'
myDriv2 = 'mverstappen'
debug(DriverRacePlot)
undebug(DriverRacePlot)
DriverRacePlot(myRace, myDriv1, myDriv2)

myRace = '2018brazil'
myRaceName = '2018brazil'
myDriv1 = 'mverstappen'
myDriv2 = NULL
includeAdjustedTime = TRUE
DriverRacePlot(myRace, myDriv1, myDriv2, includeAdjustedTime = includeAdjustedTime)

# check random ones:

dum = sample(raceDF$race[raceDF$isValidRace30], 1)
ddum = sample(rddf$driver[rddf$race==dum & rddf$mod30PredNValid > 5], 2)
dput(file='c:/temp/temp.dat', list(racename = dum, driv1 = ddum[1], driv2 = ddum[2]))
DriverRacePlot(dum, ddum[1], ddum[2])

dum = dget('c:/temp/temp.dat')
DriverRacePlot(dum$racename, dum$driv1, dum$driv2)

# isoutlier30 seems to harsh, we need a different criterian for it
# alguersuari abu dhabi 2010?


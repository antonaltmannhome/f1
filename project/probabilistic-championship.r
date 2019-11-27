### try to do probabilitic thing quickly

pointsSystem2010Plus = data.frame(position = 1:10, pointsScored = c(25,18,15,12,10,8,6,4,2,1))
rddf = left_join(rddf %>%
					mutate(position = officialFinishingPosition),
					pointsSystem2010Plus,
					'position')
rddf$pointsScored[is.na(rddf$pointsScored)] = 0

numRaceByDriverYear = rddf %>%
						group_by(year, driver) %>%
						summarise(numRace = n())
rddf = left_join(rddf, numRaceByDriverYear, c('year', 'driver'))

RandomPoint = function(x, numSim) {
	xmat = matrix(rep(x, numSim), nrow = numSim, byrow = TRUE)
	simPoint = apply(xmat, 1, function(x) sum(sample(x, length(x), replace = TRUE)))
	simDF = tibble(simNumber = 1:numSim, simPoint = simPoint)
	return(simDF)
}

numSim = 1000
simPointDF = rddf %>%
				group_by(year, driver) %>%
				do(RandomPoint(.$pointsScored, numSim)) %>%
				ungroup()

simWinner = simPointDF %>%
				group_by(year, simNumber) %>%
				summarise(winner = driver[which.max(simPoint)]) %>%
				ungroup()

winProb = simWinner %>%
			group_by(year, winner) %>%
			summarise(winProb = n()/numSim)

sumByDriver = winProb %>%
				group_by(winner) %>%
				summarise(sumWin = sum(winProb)) %>%
				arrange(sumWin)

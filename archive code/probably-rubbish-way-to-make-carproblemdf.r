.MakeDriverRaceCarProblemDF = function(isCarProblem) {
	rleOutput = rle(isCarProblem)
	rleLength = rleOutput$lengths
	fullCarProblemDF = tibble(startLap = c(1,lag(cumsum(rleLength))[-1]+1),
								endLap = cumsum(rleLength),
								carProblemInd = rleOutput$values)
	carProblemDF = fullCarProblemDF %>%
					filter(carProblemInd == 1) %>%
					select(-carProblemInd)
	return(carProblemDF)
}

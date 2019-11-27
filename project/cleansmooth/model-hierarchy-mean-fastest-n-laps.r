### let's tinker with how many laps to include when doing average. maybe taking the fastest lap, of the the fastest 5 laps is better?

### paste in first set of functions in model-hierarchy-v0


modelDF = lbl %>%
			filter(isValid0) %>%
			group_by(race, driver) %>%
			summarise(rawDCoef = min(sec)) %>%
			ungroup()
rddf$dCoef0 = NormaliseRawDCoef(modelDF)
rddf$predNValid0 = with(rddf, ifelse(nonRogueCount >=5, nonRogueCount, 0))

rddf = SmoothAndCalculateSqDiff(rddf, 'dCoef0', 'predNValid0', 'expectedFinPos0', 'sqDiff0')

CompareModel(rddf, 'expectedFinPos0', 'expectedFinPos30', 'sqDiff0', 'sqDiff30')
# A tibble: 1 x 2
    RMSE1   RMSE2
    <dbl>   <dbl>
1 2.20327 2.00034
[1] "(0.6489629, 1.0494153)"
## miles away

# what about fastest 5 laps

modelDF = lbl %>%
			filter(isValid0) %>%
			group_by(race, driver) %>%
			summarise(rawDCoef = mean(sort(sec)[1:5])) %>%
			ungroup()
rddf$dCoef0 = NormaliseRawDCoef(modelDF)
rddf$predNValid0 = with(rddf, ifelse(nonRogueCount >=5, nonRogueCount, 0))

rddf = SmoothAndCalculateSqDiff(rddf, 'dCoef0', 'predNValid0', 'expectedFinPos0', 'sqDiff0')

CompareModel(rddf, 'expectedFinPos0', 'expectedFinPos30', 'sqDiff0', 'sqDiff30')
# A tibble: 1 x 2
    RMSE1   RMSE2
    <dbl>   <dbl>
1 2.15377 2.00034
[1] "(0.4593973, 0.8062957)"

## reassuringly rubbish

numToInclude = 100
modelDF = lbl %>%
			filter(isValid0) %>%
			group_by(race, driver) %>%
			summarise(numValidLap = n(),
						numLapToUse = min(c(numValidLap, numToInclude)),
						rawDCoef = mean(sort(sec)[1:numLapToUse])) %>%
			ungroup()
rddf$dCoef0 = NormaliseRawDCoef(modelDF)
rddf$predNValid0 = with(rddf, ifelse(nonRogueCount >=5, nonRogueCount, 0))

rddf = SmoothAndCalculateSqDiff(rddf, 'dCoef0', 'predNValid0', 'expectedFinPos0', 'sqDiff0')

CompareModel(rddf, 'expectedFinPos0', 'expectedFinPos30', 'sqDiff0', 'sqDiff30')

### could put them into a graph. but the gist is, the more laps, the better

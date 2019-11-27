### have done some good work making messy-race-funct, let's do some validation of it, we can use our expected fin pos here

LoadAllData()

suppressWarnings(library(data.table))
source('project/cleansmooth/smoothing-admin-funct.r')
source('project/cleansmooth/smoothing-plus-finpos.r')
source('project/validate via finpos/messy-race-funct.r')

rddf = MakeCleanRace(rddf)

dum = GetSmooth(qrToFit = 'qr',
				qrToPredict = 'rfinpos',
				fwbw = 'fwbw',
				useStretch = TRUE,
				modelChoice = 30)

rddf = left_join(rddf, dum$smoothDF, c('race', 'driver'))

### all the various comparison scheck out:

rddf %>% group_by(hadVerySlowLap) %>% summarise(fpDelta = mean(officialFinishingPosition - expectedFinPos, na.rm=T))
  hadVerySlowLap    fpDelta
  <lgl>               <dbl>
1 FALSE          -0.0354214
2 TRUE            1.84763

rddf %>% group_by(simIndicatesDisaster) %>% summarise(fpDelta = mean(officialFinishingPosition - expectedFinPos, na.rm=T))
simIndicatesDisaster   fpDelta
  <lgl>                    <dbl>
1 FALSE                -0.205051
2 TRUE                  2.49961
rddf %>% group_by(hadFirstLapDisaster) %>% summarise(fpDelta = mean(officialFinishingPosition - expectedFinPos, na.rm=T))
# A tibble: 2 x 2
  hadFirstLapDisaster   fpDelta
  <lgl>                   <dbl>
1 FALSE               -0.106183
2 TRUE                 2.51647
rddf %>% group_by(hadFirstLapProblem) %>% summarise(fpDelta = mean(officialFinishingPosition - expectedFinPos, na.rm=T))
  hadFirstLapProblem    fpDelta
  <lgl>                   <dbl>
1 FALSE              -0.0533426
2 TRUE                1.58283
rddf %>% group_by(hadCarProblem) %>% summarise(fpDelta = mean(officialFinishingPosition - expectedFinPos, na.rm=T))
  hadCarProblem    fpDelta
  <lgl>              <dbl>
1 FALSE         -0.0940741
2 TRUE           2.54028
rddf %>% group_by(hadSlowPitStop) %>% summarise(fpDelta = mean(officialFinishingPosition - expectedFinPos, na.rm=T))
  hadSlowPitStop   fpDelta
  <lgl>              <dbl>
1 FALSE          -0.121217
2 TRUE            1.40384
rddf %>% group_by(qualDelta) %>% summarise(fpDelta = mean(officialFinishingPosition - expectedFinPos, na.rm=T), deltaCount = n())
 1       -16   -6.63310            1
 2       -15   -0.781929           1
 3       -13   -4.79588            3
 4       -12   -5.64016            3
 5       -11   -2.70581            5
 6       -10   -5.00098           12
 7        -9   -2.23772           12
 8        -8   -2.89208           29
 9        -7   -0.870971          56
10        -6   -1.45231           76
11        -5   -1.83125          122
12        -4   -0.471334         151
13        -3   -0.748027         290
14        -2   -0.248665         427
15        -1   -0.437616         566
16         0   -0.0386262        795
17         1    0.325190         406
18         2    0.651980         278
19         3    0.907299         168
20         4    1.23273          115
21         5    1.63111           81
22         6    1.11727           65
23         7    1.55138           45
24         8    2.41280           32
25         9    1.19280           26
26        10    0.899849          23
27        11    1.96031           21
28        12    2.12541           12
29        13    1.76279           11
30        14    2.62907            8
31        15    3.86687           11
32        16    1.56797           10
33        18    0.163203           5
34        19    0.893226           1
35        20    1.51721            2
36        21    2.94174            2
37        23    1.58734            2
38        NA  NaN                  2

# so let's have three categories: clean, half clean, messy
# retirement, carproblem, firstlapdisaster, simIndicatesDisaster, qualDelta < (-8): messy=0
# slow pit stop, veryslowlap, firstlapproblem, (qualDelta in (5, 7): mesy = 0.5
# everything else is fine

# need to do everything in this very long winded way because things can fall into more than one category
rddf = rddf %>%
		mutate(qualMessiness = case_when(
					qualDelta < 5 ~ 0,
					between(qualDelta, 5, 7) ~ 0.5,
					qualDelta > 7 ~ 1),
				slowPitStopMessiness = 0.5 * hadSlowPitStop,
				carProblemMessiness = 1 * hadCarProblem,
				verySlowLapMessiness = 0.5*hadVerySlowLap,
				simIndicatesDisasterMessiness = 1 * simIndicatesDisaster,
				firstLapMessiness = case_when(
									hadFirstLapDisaster ~ 1,
									hadFirstLapProblem ~ 0.5,
									TRUE ~ 0),
				retirementMessiness = 1 * isRetirement,
				invalidRaceMessiness = 1 * !isValidRace30) %>%
		rowwise() %>%
		mutate(messiness = max(c(qualMessiness, slowPitStopMessiness, carProblemMessiness,
								verySlowLapMessiness, simIndicatesDisasterMessiness,
								firstLapMessiness, retirementMessiness, invalidRaceMessiness)))

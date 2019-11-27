# these ones are not agreeing, why?

# start by running chunk at start of AdjustLapTimeForPitStop

lbl2 = left_join(lbl, read.csv('c:/temp/temp.csv'), c('rr','driver','lap'))

lbl2[with(lbl2, which(abs(impsec.x - impsec.y)>1)),c('race','driver', 'lap', 'inlap', 'outlap', 'impsec.x', 'impsec.y','penalty')] %>% arrange(race)

# just malaysia 2010, surely an edge cse of some sort

# meanwhile here's the table of weird looking ones:
worth checking on the penalty thing in calculate-pitstop-delta
# A tibble: 13 x 7
race          driver        lap inlap outlap   impsec mod4PredSec
<chr>         <chr>       <int> <lgl> <lgl>     <dbl>       <dbl>
1 2010europe    mschumacher    11 TRUE  FALSE   91.4260     103.072
2 2011malaysia  jbutton        23 TRUE  FALSE  100.855      104.056
3 2011malaysia  lhamilton      24 TRUE  FALSE  100.807      104.548
4 2011malaysia  svettel        25 TRUE  FALSE  100.923      104.183
5 2011singapore pmaldonado     26 FALSE TRUE   112.154      115.218
6 2011singapore pmaldonado     45 TRUE  FALSE  113.840      116.947
7 2013monaco    kraikkonen     71 FALSE TRUE    76.235       79.655
8 2013singapore pdiresta       42 TRUE  FALSE  111.062      114.146
9 2018monaco    dricciardo     17 TRUE  FALSE   74.2660      78.091
10 2018monaco    svettel        17 FALSE TRUE    74.983       78.476
11 2018singapore lhamilton      15 TRUE  FALSE  103.277      106.465

# schumacher in europe 2010 is the only one that's really definitely odd, the others are just what happens with so many race/driver/lap combos. think we can drop the warning. or at least only show new ones. b tut ath would invole setting upa  'checked are ok' table, can't be bothered right now

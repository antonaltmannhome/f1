crashdf = tibble(year = 2007:2019)
crashdf$vetcrash = c(2, 5, 6, 4, 3, 4, 0, 1, 5, 3, 5, 7, 3)
crashdf$hamcrash = c(2, 3, 4, 3, 11, 1, 1, 3, 2, 2, 1, 1, 0)
crashdf$maxcrash = 0
crashdf$maxcrash[crashdf$year >=2015] = c(2, 7, 2, 10, 0)
crashdf$vetnumrace = c(8, 18, 17, 19, 19, 20, 19, 19, 19, 21, 20, 21, 10)
crashdf$hamnumrace = c(17, 18, 17, 19, 19, 20, 19, 19, 19, 21, 20, 21, 10)
crashdf$maxnumrace = c(rep(0, 8), 19, 21, 20, 21, 10)

totalrace = with(crashdf, sum(vetnumrace) + sum(hamnumrace) + sum(maxnumrace))
totalcrash =  with(crashdf, sum(vetcrash) + sum(hamcrash) + sum(maxcrash))
crashprob = totalcrash / totalrace

dbinom(0:20, 20, crashprob)

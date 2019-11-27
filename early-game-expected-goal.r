### want to get expected goals scored and conceded at the start of the season. best way i can think of doing this is entering betfair odds manually, then converting them to lam and mu

earlygamedf = read.csv(paste0(DATAPATH, 'fixture_result/fixture1920_early_plus_odds.csv'),
                        stringsAsFactors = FALSE) %>%
              filter(!is.na(hwin))
earlygamedf = earlygamedf %>%
              rowwise %>%
              mutate(overround = sum(1/hwin + 1/draw + 1/awin),
                      hwinprob = 1/(hwin * overround),
                      drawprob = 1/(draw * overround),
                      awinprob = 1/(awin * overround))

# now the tricky bit, converting that to expected goals
suptottoprob = function(lam, mu) {
	scoreprobmat = dpois(0:10, lam) %*% t(dpois(0:10, mu))
	hwinprob = sum(scoreprobmat * lower.tri(scoreprobmat))
	drawprob = sum(diag(scoreprobmat))
	awinprob = sum(scoreprobmat * upper.tri(scoreprobmat))

	return(c(hwinprob, drawprob, awinprob))
}

tryouttheta = function(theta,truehda) {
	lam = exp(theta[1])
	mu = exp(theta[2])
	candidatehda = suptottoprob(lam, mu)
	sqdiff = sum( (candidatehda - truehda)^2)
	return(sqdiff)
}

getlammu = function(truehwin, truedraw, trueawin) {

	truehda = c(truehwin, truedraw, trueawin)
	maxinfo = nlm(tryouttheta, p = c(log(1.5), log(1)), truehda = truehda)
	lam = exp(maxinfo$est[1])
	mu = exp(maxinfo$est[2])
	return(data.frame(lam = lam, mu = mu))
}

dum = earlygamedf %>%
			rowwise() %>%
			do(getlammu(.$hwinprob, .$drawprob, .$awinprob))

earlygamedf = cbind(earlygamedf, dum)

vertgamedf = bind_rows(earlygamedf %>%
                        select(ht, lam, mu) %>%
                        rename(team = ht, escored = lam, econc = mu),
                        earlygamedf %>%
                        select(at, lam, mu) %>%
                        rename(team = at, escored = mu, econc = lam))

totalbyteam = vertgamedf %>%
              group_by(team) %>%
              summarise(totscored = sum(escored),
                        totconc = sum(econc)) %>%
              arrange(desc(totscored - totconc))

# so in 1920, liv > city > arse. but everton and bounemouth good. avoid spurs/chelsea/wolves/leicester/west ham

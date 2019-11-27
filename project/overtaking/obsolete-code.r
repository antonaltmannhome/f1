
## think this is all some investiagetive shite that isn't robust to the varying definitions of blocked

FitGamma = function(theta, mySecLoss) {
	print(theta)
	myprob = dgammaMuSigma(mySecLoss, exp(theta[1]),exp(theta[2]))
	loglik = mean(log(myprob))
	return(-loglik)
}

CheckInterval=function(mylagap, myuagap) {
	sax2=with(lbl, which(isGoodBlock & overlap > mylagap & overlap < myuagap))
	if (length(sax2)<10) {
		return(list(numob = length(sax2), mleparam = c(NA,NA)))
	}
	with(lbl[sax2,], hist( (impsec - secLimit),
							br=100, fre=FALSE, col='cyan',
							xlim = c(0, max(impsec - secLimit))))
	### and also overlay the mle version
	mySecLoss = (lbl$impsec - lbl$secLimit)[sax2]
	maxinfo = nlm2(FitGamma, p = c(log(mean(mySecLoss)), log(sd(mySecLoss))), mySecLoss)
	curve(dgamma(x,exp(maxinfo$est[1]),exp(maxinfo$est[2])),col='green',add=T)
	## let's have the mean and variance
	mymean=exp(maxinfo$est[1])/exp(maxinfo$est[2])
	myvar=exp(maxinfo$est[1])/exp(maxinfo$est[2])^2
	cat('Number of observations:',length(sax2),'Mean:',mymean,'Var:',myvar,'\n')
	return(list(numob=length(sax2),mleparam=c(exp(maxinfo$est[1]),exp(maxinfo$est[2]))))
}

myOverlapEdge=seq(-3,5,0.5)
myOverlapMean=0.5*(head(myOverlapEdge, -1) + tail(myOverlapEdge, -1))
meanParam = varParam = rep(NA, length(myOverlapMean))
for (j in 1:length(myOverlapMean)) {
	dum=checkinterval(myOverlapMean[j] - 0.25, myOverlapMean[j] + 0.25)
	meanparam[j]=dum$mleparam[1]/dum$mleparam[2]
	varparam[j]=dum$mleparam[1]/dum$mleparam[2]^2
}

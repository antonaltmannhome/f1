// Source for MyFunc.cpp

#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double RcppInvLogit(double x) {
	double invlogitedx;
	if (x < -10.0) invlogitedx = 0.0;
	if (x > 10.0) invlogitedx = 1.0;
	if ( (x > -10.0) & (x < 10.0) ) invlogitedx =  exp(x) / (1.0 + exp(x));

	return invlogitedx;
}

// [[Rcpp::export]]
double RcppGammaMuSigma(double mu, double sigma) {
	double alphaparam = pow(mu, 2.0)/pow(sigma, 2.0);
	double betaparam = pow(sigma, 2.0) / mu;
	NumericVector vectorSimVal = rgamma(1, alphaparam, betaparam);
	double simVal = vectorSimVal[0];
	// that occasionally simulates something ludicrous, cap it at mean+2sds
	double upperBound = mu + 2.0*sigma;
	if (simVal > upperBound) simVal = upperBound;
	return(simVal);
}

// [[Rcpp::export]]
double RcppSimulateBlockSec(double secLimit, double predSec,
							NumericVector followMeanCoef,
							NumericVector followVarCoef) {

	double overlap = predSec - secLimit;

	double meanSec = exp(followMeanCoef[0]) *
						log(exp(followMeanCoef[1]) +
						exp(exp(followMeanCoef[2]) * overlap));

	double stdDevSec = pow(exp(followVarCoef[0]) +
						exp(followVarCoef[1]) *
						RcppInvLogit(exp(followVarCoef[2]) * (overlap - followVarCoef[3])), 0.5);

	double secLoss = RcppGammaMuSigma(meanSec, stdDevSec);

	double simSec = secLimit + secLoss;

	return(simSec);
}

// [[Rcpp::export]]
double RcppSimulateGotOvertakeSec(double secLimit, double predSec) {

	double linMix = RcppInvLogit(10.0 * (predSec - secLimit));
	double meanSec = (1.0 - linMix) + linMix * (1.0 + (predSec - secLimit));
	double stdDevSec = 1.0;
	// Rcout << "meanSec:" << meanSec << std::endl;

	double secLoss = RcppGammaMuSigma(meanSec, stdDevSec);
	double simSec = secLimit + secLoss;

	return(simSec);
}

// [[Rcpp::export]]
double RcppSimulateDidOvertakeSec(double secLimit, double predSec, double didOtCost, bool isLeader,
							NumericVector followMeanCoef,
							NumericVector followVarCoef) {

	double adjPredSec = predSec + didOtCost;
	//	Rcout << "adjpredsec" << adjPredSec << std::endl;

	NumericVector vectorSimSec;
	double simSec;
	if (isLeader) {
		vectorSimSec = rnorm(1, adjPredSec, 1.0);
		simSec = vectorSimSec[0];
	}
	if (!isLeader) {
		simSec = RcppSimulateBlockSec(secLimit, adjPredSec, followMeanCoef, followVarCoef);
	}

	return(simSec);
}

// [[Rcpp::export]]
double RcppSimulateSec(int startRank, int endRank,
						double startTelapse, double prevEndTelapse, double predSec,
						int didOvertake, int gotOvertake,
						NumericVector followMeanCoef, NumericVector followVarCoef,
						double didOtCost,
						double secStandardError) {

	double secBuffer = -99.0;
	double secLimit = -99.0;
	if (endRank == 1) {
		secBuffer = -99.0;
	}
	if (endRank > 1) {
		secLimit = prevEndTelapse - startTelapse;
		secBuffer = predSec - secLimit;
	}

	bool isLeader = (endRank == 1);
	bool isClear = ( (startRank == 1) & (endRank == 1) ) |
					( (didOvertake == 0) & (gotOvertake == 0) & (secBuffer > 3.0) );
	bool isBlocked = ( (didOvertake == 0) & (gotOvertake == 0) &
						(secBuffer > -98.0) & (secBuffer < 3.0) );
	//Rcout << "secLimit:" << secLimit << std::endl;
	//Rcout << "secBuffer:" << secBuffer << std::endl;
	//Rcout << "isLeader:" << isLeader << std::endl;
	//Rcout << "isClear:" << isClear << std::endl;
	//Rcout << "isBlocked:" << isBlocked << std::endl;

	double simSec = -99.0;
	if (isClear) {
		NumericVector vectorSimSec = rnorm(1, predSec, secStandardError);
		simSec = vectorSimSec[0];
		// once in a blue moon, an illegal lap time will be simulated though
		if (simSec < secLimit) simSec = secLimit + 0.001;
	}
	if (isBlocked) {
		simSec = RcppSimulateBlockSec(secLimit, predSec, followMeanCoef, followVarCoef);
	}
	if (didOvertake > 0) {
		simSec = RcppSimulateDidOvertakeSec(secLimit, predSec, didOtCost, isLeader,
											followMeanCoef, followVarCoef);
	}
	if (gotOvertake > 0) {
		simSec = RcppSimulateGotOvertakeSec(secLimit, predSec);
	}

	return(simSec);
}

// [[Rcpp::export]]
NumericVector RcppSimulateLap(IntegerVector startRank, IntegerVector endRank,
								NumericVector startTelapse, NumericVector predSec,
								LogicalVector didOvertake, LogicalVector gotOvertake,
								NumericVector followMeanCoef, NumericVector followVarCoef,
								double didOtCost,
								double secStandardError) {

	int loopSize = startRank.size();
	NumericVector endTelapse(loopSize);

	for (int dsi = 0; dsi < loopSize; dsi++) {
		double prevEndTelapse = -99.0;
		if (endRank[dsi] == 1) {
			prevEndTelapse = -99.0;
		}
		if (endRank[dsi] > 1) {
			prevEndTelapse = endTelapse[dsi - 1];
		}

		double simSec = RcppSimulateSec(startRank[dsi], endRank[dsi],
										startTelapse[dsi], prevEndTelapse, predSec[dsi],
										didOvertake[dsi], gotOvertake[dsi],
										followMeanCoef, followVarCoef, didOtCost, secStandardError);
		endTelapse[dsi] = startTelapse[dsi] + simSec;
	}

	return(endTelapse);
}

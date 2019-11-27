// Source for MyFunc.cpp

#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]  
int RcppChoose(int n, int k) {
	double doubleChooseOutcome = Rf_choose(double(n), double(k));
	int intChooseOutcome = int(doubleChooseOutcome);
	return intChooseOutcome;
}

// [[Rcpp::export]]

double RcppInvLogit(double x) {
	double invlogitedx;
	if (x < -10.0) invlogitedx = 0.0;
	if (x > 10.0) invlogitedx = 1.0;
	if (x > -10.0 & x < 10.0) invlogitedx =  exp(x) / (1.0 + exp(x));
	
	return invlogitedx;
}

// [[Rcpp::export]]
bool RcppAllTrue(LogicalVector x) {
   // Note the use of is_true to return a bool type.
   return is_true(all(x == TRUE));
}

// [[Rcpp::export]]
IntegerMatrix RcppGetOvertakingSummary(NumericVector myPredEndTelapse,
							int numSim, int numDriver,
							double circuitOvertakingCoef, double overtakingGapCoef) {
  
	NumericVector currentSimPredEndTelapse(numDriver);
	int numberOfCombination = RcppChoose(numDriver, 2);
	// Rcout << "number of combinations" << numberOfCombination << std::endl;
		
	IntegerVector driver1(numberOfCombination);
	IntegerVector driver2(numberOfCombination);
	NumericVector overtakeProbability(numberOfCombination);

	IntegerVector driverRank(numDriver);
	IntegerVector numberDidOvertake(numDriver);
	IntegerVector numberGotOvertake(numDriver);
	LogicalVector driverInPositionCheck(numDriver);
	
	IntegerMatrix outputTibble(numDriver * numSim, 3);

	bool verbose = FALSE;
	
	// Rcout << "Have initialised everything" << std::endl;
	
	for (int simi = 0; simi < numSim; ++simi) {
		int startingIndex = simi * numDriver;
		for (int di = 0; di < numDriver; ++di) {
			currentSimPredEndTelapse[di] = myPredEndTelapse[startingIndex + di];
		}
											
		int count = 0;
		for (int d1 = 0; d1 < numDriver - 1; d1++) {
			for (int d2 = d1 + 1; d2 < numDriver; d2++) {
				driver1[count] = d1;
				driver2[count] = d2;
				count++;
			}
		}
	
		for (int ci = 0; ci < numberOfCombination; ci++) {
			double myDiffPredEndTelapse = currentSimPredEndTelapse[driver2[ci]] -
											currentSimPredEndTelapse[driver1[ci]];
			overtakeProbability[ci] = RcppInvLogit(
				circuitOvertakingCoef + myDiffPredEndTelapse * overtakingGapCoef
											);
			// Rcout << "The value is " << overtakeProbability[ci] << std::endl;
		}
		
		bool legal = FALSE;
		while(!legal) {
			NumericVector didOvertake(numberOfCombination);
			for (int ci = 0; ci < numberOfCombination; ci++) {
				NumericVector wtf = rbinom(1, 1, overtakeProbability[ci]);
				didOvertake[ci] = wtf[0];
				//Rcout << "Overtake prob: " << overtakeProbability[ci] << std::endl;
				//Rcout << "Did overtake: " << didOvertake[ci] << std::endl;
			}
			if (verbose) Rcout << "Have simulated overtakes" << std::endl;

			// initialise each driver in their starting rank
			for (int di =0; di < numDriver; di++) {
				driverRank[di] = di;
				numberDidOvertake[di] = 0;
				numberGotOvertake[di] = 0;
			}
			for (int ci =0; ci < numberOfCombination; ci++) {
				if (didOvertake[ci] == 1) {
					driverRank[driver1[ci]] += 1;
					driverRank[driver2[ci]] -= 1;
					numberDidOvertake[driver2[ci]] += 1;
					numberGotOvertake[driver1[ci]] += 1;
				}
			}
			if (verbose) Rcout << "Have procesed rankings" << std::endl;
			/// but is this legal? need to make sure there's a driver in each position
			for (int di = 0; di < numDriver; di++) {
				driverInPositionCheck[di] = FALSE;
				for (int ri = 0; ri < numDriver; ri++) {
					if (di == driverRank[ri]) {
						driverInPositionCheck[di] = TRUE;
					}
				}
			}
		
			legal = RcppAllTrue(driverInPositionCheck);
		}
		
		for (int di = 0; di < numDriver; di++) {
			outputTibble(startingIndex + di, 0) = driverRank[di];
			outputTibble(startingIndex + di, 1) = numberDidOvertake[di];
			outputTibble(startingIndex + di, 2) = numberGotOvertake[di];
		}
		if (verbose) Rcout << "Have filled out outputTibble" << std::endl;
		if (verbose) Rcout << "Simulation" << simi << "worked in seems" << std::endl;
	}
 
	return outputTibble;
}

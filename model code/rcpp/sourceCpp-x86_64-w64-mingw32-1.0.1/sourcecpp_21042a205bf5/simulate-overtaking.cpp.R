`.sourceCpp_1_DLLInfo` <- dyn.load('c:/research/f1/model code/rcpp/sourceCpp-x86_64-w64-mingw32-1.0.1/sourcecpp_21042a205bf5/sourceCpp_2.dll')

RcppChoose <- Rcpp:::sourceCppFunction(function(n, k) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_RcppChoose')
RcppInvLogit <- Rcpp:::sourceCppFunction(function(x) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_RcppInvLogit')
RcppAllTrue <- Rcpp:::sourceCppFunction(function(x) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_RcppAllTrue')
RcppGetOvertakingSummary <- Rcpp:::sourceCppFunction(function(myPredEndTelapse, numSim, numDriver, circuitOvertakingCoef, overtakingGapCoef) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_RcppGetOvertakingSummary')

rm(`.sourceCpp_1_DLLInfo`)

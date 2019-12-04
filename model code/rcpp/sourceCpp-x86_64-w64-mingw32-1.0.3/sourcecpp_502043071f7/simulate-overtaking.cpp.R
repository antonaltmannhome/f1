`.sourceCpp_1_DLLInfo` <- dyn.load('c:/git/f1/model code/rcpp/sourceCpp-x86_64-w64-mingw32-1.0.3/sourcecpp_502043071f7/sourceCpp_2.dll')

RcppChoose <- Rcpp:::sourceCppFunction(function(n, k) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_RcppChoose')
RcppInvLogit <- Rcpp:::sourceCppFunction(function(x) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_RcppInvLogit')
RcppAllTrue <- Rcpp:::sourceCppFunction(function(x) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_RcppAllTrue')
RcppGetOvertakingSummary <- Rcpp:::sourceCppFunction(function(myPredEndTelapse, numSim, numDriver, circuitOvertakingCoef, overtakingGapCoef) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_RcppGetOvertakingSummary')

rm(`.sourceCpp_1_DLLInfo`)

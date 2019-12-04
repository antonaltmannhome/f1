`.sourceCpp_3_DLLInfo` <- dyn.load('c:/git/f1/model code/rcpp/sourceCpp-x86_64-w64-mingw32-1.0.3/sourcecpp_5020292139af/sourceCpp_5.dll')

RcppInvLogit <- Rcpp:::sourceCppFunction(function(x) {}, FALSE, `.sourceCpp_3_DLLInfo`, 'sourceCpp_3_RcppInvLogit')
RcppGammaMuSigma <- Rcpp:::sourceCppFunction(function(mu, sigma) {}, FALSE, `.sourceCpp_3_DLLInfo`, 'sourceCpp_3_RcppGammaMuSigma')
RcppSimulateBlockSec <- Rcpp:::sourceCppFunction(function(secLimit, predSec, followMeanCoef, followVarCoef) {}, FALSE, `.sourceCpp_3_DLLInfo`, 'sourceCpp_3_RcppSimulateBlockSec')
RcppSimulateGotOvertakeSec <- Rcpp:::sourceCppFunction(function(secLimit, predSec) {}, FALSE, `.sourceCpp_3_DLLInfo`, 'sourceCpp_3_RcppSimulateGotOvertakeSec')
RcppSimulateDidOvertakeSec <- Rcpp:::sourceCppFunction(function(secLimit, predSec, didOtCost, isLeader, followMeanCoef, followVarCoef) {}, FALSE, `.sourceCpp_3_DLLInfo`, 'sourceCpp_3_RcppSimulateDidOvertakeSec')
RcppSimulateSec <- Rcpp:::sourceCppFunction(function(startRank, endRank, startTelapse, prevEndTelapse, predSec, didOvertake, gotOvertake, followMeanCoef, followVarCoef, didOtCost, secStandardError) {}, FALSE, `.sourceCpp_3_DLLInfo`, 'sourceCpp_3_RcppSimulateSec')
RcppSimulateLap <- Rcpp:::sourceCppFunction(function(startRank, endRank, startTelapse, predSec, didOvertake, gotOvertake, followMeanCoef, followVarCoef, didOtCost, secStandardError) {}, FALSE, `.sourceCpp_3_DLLInfo`, 'sourceCpp_3_RcppSimulateLap')

rm(`.sourceCpp_3_DLLInfo`)

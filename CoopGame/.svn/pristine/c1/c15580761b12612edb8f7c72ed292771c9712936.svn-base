#' @name getGapFunctionCoefficients
#' @title getGapFunctionCoefficients
#' @description getGapFunctionCoefficients
#' @aliases getGapFunctionCoefficients
#' @export getGapFunctionCoefficients
#' @template author/JS
#' @template cites/DRIESSEN_1998
#' @templateVar DRIESSEN_1998_P p. 57
#' @template param/A 
#' @return numeric vector containing the GapFunction coefficients for every coalition
#' @examples 
#' library(CoopGame)
#' getGapFunctionCoefficients(c(0,0,0,60,48,30,72))
#' #[1] 42 24 12  6  6  6  6
#' 

getGapFunctionCoefficients=function(A){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_getGapFunctionCoefficients(paramCheckResult, A)
  N=length(A);
  n<-getNumberOfPlayers(A)
  x <- getUtopiaPayoff(A)
  result <- -getExcessCoefficients(A,x)
  return(result);
}

initialParamCheck_getGapFunctionCoefficients=function(paramCheckResult,A){
  stopOnInvalidGameVectorA(paramCheckResult, A)
}

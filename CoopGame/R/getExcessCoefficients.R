#' @name getExcessCoefficients
#' @title getExcessCoefficients
#' @description getExcessCoefficients
#' @aliases getExcessCoefficients
#' @export getExcessCoefficients
#' @template author/JA
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 58
#' @template cites/DRIESSEN_1998
#' @templateVar DRIESSEN_1998_P p. 12
#' @template param/A 
#' @template param/x
#' @return numeric vector containing the excess coefficients for every coalition
#' @examples 
#' library(CoopGame)
#' getExcessCoefficients(c(0,0,0,60,48,30,72), c(24,24,24))
#' #[1] -24 -24 -24  12   0 -18   0
#' 

getExcessCoefficients=function(A,x){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_getExcessCoefficients(paramCheckResult, A,x)
  N=length(A);
  n<-getNumberOfPlayers(A)

  excessCoefficients=sapply(1:N,function(i){
    involvedPlayers=getPlayersFromIndex(n,i)
    return(A[i]-sum(x[involvedPlayers]))
  })
  
  return(excessCoefficients);

}

initialParamCheck_getExcessCoefficients=function(paramCheckResult,A,x){
  stopOnInvalidGameVectorA(paramCheckResult, A)
  stopOnInvalidAllocation(paramCheckResult,x)
}

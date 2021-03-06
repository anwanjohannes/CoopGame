#' @name getPerCapitaExcessCoefficients
#' @title getPerCapitaExcessCoefficients
#' @description getPerCapitaExcessCoefficients
#' @aliases getPerCapitaExcessCoefficients
#' @export getPerCapitaExcessCoefficients
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 58
#' @template param/A 
#' @template param/x
#' @return numeric vector containing the per capita excess coefficients for every coalition
#' @examples 
#' library(CoopGame)
#' getPerCapitaExcessCoefficients(c(0,0,0,60,48,30,72), c(24,24,24))
#' #[1] -24 -24 -24   6   0  -9   0
#'

getPerCapitaExcessCoefficients=function(A,x){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_getPerCapitaExcessCoefficients(paramCheckResult, A,x)
  N=length(A);
  n<-getNumberOfPlayers(A)

  excessCoefficients=sapply(1:N,function(i){
    involvedPlayers=getPlayersFromIndex(n,i)
    return(A[i]-sum(x[involvedPlayers]))
  })
  
  coeffMat<-createBitMatrix(n)
  cardS=sapply(1:N,function(ix){sum(coeffMat[ix,1:n])})
  pce = sapply(1:(N-1),function(i){
    excessCoefficients[i]/cardS[i]
  })
  return(c(pce[-N],0))
}

initialParamCheck_getPerCapitaExcessCoefficients=function(paramCheckResult,A,x){
  stopOnInvalidGameVectorA(paramCheckResult, A)
  stopOnInvalidAllocation(paramCheckResult,x)
}

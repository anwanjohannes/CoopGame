#  Version 1.0
#  Datum: 24.11.2015
#' @name cFuncApex
#' @title cFuncApex
#' @description function of apex game
#' @aliases cFuncApex
#' @export cFuncApex
#' @export initialParamCheck_cFuncApex
#' @author Alexandra Tiukkel
#' @param S numeric vector with coalition of players
#' @param numberOfPlayers number of players
#' @param apexPlayer number of the apex-player (per default 1)
#' @return Calculated payoff for coalition S
#' @examples
#' cFuncApex(c(1,2), numberOfPlayers = 3)

cFuncApex<-function(S, numberOfPlayers = 3, apexPlayer = 1){
  # validate parameter
  paramCheckResult <- getEmptyParamCheckResult()
  initialParamCheck_cFuncApex(paramCheckResult,S, numberOfPlayers)
  #sort to be secure
  S<-sort(S)
  #initialize payoff for S
  retVal<-0
  #check whether apexPlayer is element of S and |S| > 1
  if((apexPlayer %in% S) && (length(S) > 1)){
    return (1)
  }
  #initialize the grand coalition N
  #Bug at generateGameVector(cFuncApex, n=4,numberOfPlayers=4, apexPlayer=1)
  #N cannot be equal on later check as N was numeric and setWithoutApex is int
  #N<-as.numeric(factor(1:numberOfPlayers)) #commented out by JA
  N<-c(1:numberOfPlayers)   #proposal by JA
  #check whether S = N\{apexPlayer}
  setWithoutApex<-N[-(which(N == apexPlayer))]
  if(identical(as.numeric(S), as.numeric(setWithoutApex))){
    return (1)
  }
  return (retVal)
}

initialParamCheck_cFuncApex=function(paramCheckResult,S, numberOfPlayers){
  stopOnInvalidCoalitionS(paramCheckResult,S, n=numberOfPlayers)
}

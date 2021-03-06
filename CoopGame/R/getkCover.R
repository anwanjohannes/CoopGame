#' @name getkCover
#' @title getkCover
#' @description getkCover: returns k-cover for a given cooperative game (- this concept is most sensible for 1-convex games)
#' @aliases getkCover
#' @export getkCover
#' @template author/JS
#' @template cites/DRIESSEN_1998
#' @templateVar DRIESSEN_1998_P p. 173
#' @template param/A
#' @param k An integer specifying k in the k-cover 
#' @return numeric vector containing the k-cover of the given game 
#' @examples 
#' library(CoopGame)
#' #Example from textbook by Driessen, p. 175, with alpha = 0.6 and k = 2
#' alpha = 0.6
#' getkCover(c(0,0,0,alpha,alpha,0,1), k=2)
#' #[1] 0.0 0.0 0.0 0.6 0.6 0.0 1.0

getkCover=function(A, k){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_getkCover(paramCheckResult, A, k)
  N=length(A);
  n<-getNumberOfPlayers(A)
  x <- getUtopiaPayoff(A)
  result <- A
  for (i in 1:N)
  {
    playersInCurrCoal <- getPlayersFromIndex(n,i)
    if (length(playersInCurrCoal) < k)
    {
      result[i] <- A[i]
    }
    else
    {
      result[i] <- A[N] - sum(x[-playersInCurrCoal])  
    }
  }
  return(result);
}

initialParamCheck_getkCover=function(paramCheckResult,A, k){
  stopOnInvalidGameVectorA(paramCheckResult, A)
  stopOnInvalidNumber(paramCheckResult, k)
}

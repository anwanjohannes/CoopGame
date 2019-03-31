#' @name getMinimalRights
#' @title getMinimalRights
#' @description Calculates the minimal rights vector. \cr
#' What is the maximum player i would get (over all coalitions) if all the other players were to get their marginal contributions? \cr
#' Marginal contribution of player i in grand coalition: \deqn{M_{i} = v(N) - v(N \ {i})}
#' Minimum right of player i: \deqn{m_{i} = \max { v(S) - \sum M_{j} } }
#' @aliases getMinimalRights
#' @export getMinimalRights
#' @template author/JA
#' @template author/MM
#' @template author/JS
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 20
#' @template param/A
#' @return Vector of minimal right payoffs for each player
#' @examples
#' library(CoopGame)
#' A <- c(0,0,0,60,60,60,72)
#' getMinimalRights(A)
#' #[1] 48 48 48
#'
#' library(CoopGame)
#' B <- c(2,4,5,18,14,9,24) 
#' getMinimalRights(B)
#' #[1] 8 4 5
#' 
getMinimalRights<-function(A){
  n=getNumberOfPlayers(A)
  N=length(A)
  bm=createBitMatrix(n,A)
  M=getUtopiaPayoff(A)
  
  m=sapply(1:n, function(i){
    bmIndices=which(bm[,i]==1,1)
    max(
      apply(
        bm[bmIndices,,drop=FALSE],
        1,
        function(bmRow){
          jPlayers=getPlayersFromBMRow(bmRow)
          jPlayers=jPlayers[jPlayers!=i]
          bmRow["cVal"]-sum(M[jPlayers])
        }
      )
    )
  })
  return(m)
}
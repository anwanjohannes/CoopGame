#' @name getUtopiaPayoff
#' @title getUtopiaPayoff
#' @description Calculates the utopia payoff for each player. \cr
#' Marginal contribution of player i in grand coalition: \deqn{M_{i} = v(N) - v(N \ {i})}
#' Minimum right of player i: \deqn{m_{i} = \max { v(S) - \sum M_{j} } }
#' @aliases getUtopiaPayoff
#' @export getUtopiaPayoff
#' @template author/JA
#' @template author/MM
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 31
#' @template param/A
#' @return utopia payoffs for each player
#' @examples
#' library(CoopGame)
#' A <- c(0,0,0,60,60,60,72)
#' getUtopiaPayoff(A)
#' #[1] 12 12 12
#'
#' library(CoopGame)
#' B <- c(2,4,5,18,14,9,24) 
#' getUtopiaPayoff(B)
#' #[1] 15 10  6

getUtopiaPayoff<-function(A){
    n=getNumberOfPlayers(A)
    N=length(A)
    
    M=A[N]-A[(N-1):(N-n)]
    return(M)
}




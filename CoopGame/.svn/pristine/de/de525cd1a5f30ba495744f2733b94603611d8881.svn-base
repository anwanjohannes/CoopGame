#' @name isConstantSumGame
#' @title isConstantSumGame
#' @description Checks if a TU game for n players is constant-sum. \cr
#' In a constant-sum game for any coalition 
#' \code{S} the sums of the values of the coalition 
#' \code{S} and its complement equal the value of 
#' the grand coalition \code{N}. 
#' @aliases isConstantSumGame
#' @export isConstantSumGame
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 11
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 261 
#' @inheritParams CoopGameBaseParams
#' @return \code{TRUE} if the game is constant-sum, else \code{FALSE}.
#' @examples
#' #Example of a constant-sum game
#' library(CoopGame)
#' A=c(0,0,0,2,2,2,2) 
#' isConstantSumGame(A)
#' 
#' #Example of a game that is not constant-sum 
#' library(CoopGame)
#' A=c(0,0,0,40,30,130,100) 
#' isConstantSumGame(A)
#' 
#' #Another example of a constant-sum game
#' library(CoopGame)
#' A=c(1,1,1,2, 2,2,2,2,2,2, 2,3,3,3, 4)
#' isConstantSumGame(A)
#' 
isConstantSumGame<-function(A){
  isCS=GamePropertyConstantSum(A)
  return(determineProperty(isCS))
}

logicIsConstantSumGame<-function(A){

  #get number of players
  numberOfPlayers=getNumberOfPlayers(A)

  #result value
  retVal<-TRUE

  N = length(A)
  
  B = A[1:(N-1)]+rev(A[-N])
  
  tolerance <- 1e-12
  
  for (i in 1:(N-1))
  {
    if  (abs(B[i]-A[N])>tolerance)
    {
      retVal <- FALSE
    }
  }
  return(retVal)
}

#' @title GamePropertyConstantSum
#' @noRd
#' @include GameProperty.R
#' #@exportClass GamePropertyConstantSum

setClass(
  "GamePropertyConstantSum",
  contains = "GameProperty"
)

#' @title Constructor for GamePropertyConstantSum
#' @noRd
#' @template author/JA
#' @name GamePropertyConstantSum
#' #@export
GamePropertyConstantSum<-function(A){
  retGamePropertyConstantSum=methods::new("GamePropertyConstantSum",A)
  return(retGamePropertyConstantSum)
}

#' @rdname determineProperty-methods
#' @aliases determineProperty,GamePropertyConstantSum-method
setMethod(
  "determineProperty",
  signature="GamePropertyConstantSum",
  definition=function(.Object){
    A<-.Object@A
    result=logicIsConstantSumGame(A)
    return(result)
  }
)





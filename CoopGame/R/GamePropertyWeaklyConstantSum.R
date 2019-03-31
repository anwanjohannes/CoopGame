#' @name isWeaklyConstantSumGame
#' @title isWeaklyConstantSumGame
#' @description Checks if a TU game for n players is weakly constant-sum. \cr
#' In a weakly constant-sum game for any singleton coalition 
#' the sums of the values of that singleton coalition 
#' and its complement equal the value of 
#' the grand coalition \code{N}. 
#' @aliases isWeaklyConstantSumGame
#' @export isWeaklyConstantSumGame
#' @template author/JS
#' @inheritParams CoopGameBaseParams
#' @return \code{TRUE} if the game is weakly constant-sum, else \code{FALSE}.
#' @examples
#' #Example of a weakly constant-sum game
#' library(CoopGame)
#' A=c(0,0,0,2,2,2,2) 
#' isWeaklyConstantSumGame(A)
#' 
#' #Example of a game that is not weakly constant-sum 
#' library(CoopGame)
#' A=c(0,0,0,40,30,130,100) 
#' isWeaklyConstantSumGame(A)
#' 
#' #Another example of a weakly constant-sum game
#' library(CoopGame)
#' A=c(1,1,1,2, 7,7,7,7,7,7, 2,3,3,3, 4)
#' isWeaklyConstantSumGame(A)
#' 
isWeaklyConstantSumGame<-function(A){
  isWCS=GamePropertyWeaklyConstantSum(A)
  return(determineProperty(isWCS))
}

logicIsWeaklyConstantSumGame<-function(A){

  #get number of players
  numberOfPlayers=getNumberOfPlayers(A)

  #result value
  retVal<-TRUE

  N = length(A)
  
  B = A[1:(N-1)]+rev(A[-N])
  
  tolerance <- 1e-12
  
  for (i in 1:numberOfPlayers)
  {
    if  (abs(B[i]-A[N])>tolerance)
    {
      retVal <- FALSE
    }
  }
  return(retVal)
}

#' @title GamePropertyWeaklyConstantSum
#' @noRd
#' @include GameProperty.R
#' #@exportClass GamePropertyWeaklyConstantSum

setClass(
  "GamePropertyWeaklyConstantSum",
  contains = "GameProperty"
)

#' @title Constructor for GamePropertyWeaklyConstantSum
#' @noRd
#' @template author/JA
#' @name GamePropertyWeaklyConstantSum
#' #@export
GamePropertyWeaklyConstantSum<-function(A){
  retGamePropertyWeaklyConstantSum=methods::new("GamePropertyWeaklyConstantSum",A)
  return(retGamePropertyWeaklyConstantSum)
}

#' @rdname determineProperty-methods
#' @aliases determineProperty,GamePropertyWeaklyConstantSum-method
setMethod(
  "determineProperty",
  signature="GamePropertyWeaklyConstantSum",
  definition=function(.Object){
    A<-.Object@A
    result=logicIsWeaklyConstantSumGame(A)
    return(result)
  }
)





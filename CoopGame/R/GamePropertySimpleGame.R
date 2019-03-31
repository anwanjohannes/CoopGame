#' @name isSimpleGame
#' @title isSimpleGame - check if a TU game is a simple game.
#' @description isSimpleGame checks if a TU game is a simple game.
#' A TU game is a simple game in the sense of the book by 
#' Peleg and Sudhoelter (2007), p. 16 f., if and only if the 
#' game is monotonic and the values of all coalitions are 
#' either \code{0} or \code{1}. 
#' @aliases isSimpleGame
#' @export isSimpleGame
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 16 f. 
#' @inheritParams CoopGameBaseParams
#' @return \code{TRUE} if the game is essential, else \code{FALSE}.
#' @examples
#' #Example of a simple game
#' library(CoopGame) 
#' A<-c(0,0,0,0,1,1,1)
#' isSimpleGame(A)
#'
#' #Example of a game which not simple 
#' library(CoopGame)
#' A<-c(0,0,0,0,1,1,2)
#' isSimpleGame(A)
#' 
#' #Another example of a game which not simple 
#' #according to our definition
#' library(CoopGame) 
#' A<-c(1,0,0,0,1,1,1)
#' isSimpleGame(A)
#' 
isSimpleGame<-function(A){
  isSimple=GamePropertySimpleGame(A)
  return(determineProperty(isSimple))
}

logicIsSimpleGame=function(A){
  boolRetVal=TRUE
  n=getNumberOfPlayers(A)
  N=length(A)
  tolerance = 1e-15
  if (!isMonotonicGame(A))
  {
    boolRetVal = FALSE
    return(boolRetVal)
  }
  for (i in 1:N)
  {
    if ((abs(A[i]-0)>tolerance) && (abs(A[i]-1)>tolerance))
    {
      boolRetVal = FALSE
      return(boolRetVal)
    }
  }
  return(boolRetVal)
}

#' @title GamePropertySimpleGame
#' @noRd
#' @include GameProperty.R
#' #@exportClass GamePropertySimpleGame

setClass(
  "GamePropertySimpleGame",
  contains = "GameProperty"
)

#' @title Constructor for GamePropertySimpleGame
#' @noRd
#' @template author/JS
#' @name GamePropertySimpleGame
#' #@export
GamePropertySimpleGame<-function(A){
  retGamePropertySimpleGame=methods::new("GamePropertySimpleGame",A)
  return(retGamePropertySimpleGame)
}

#' @rdname determineProperty-methods
#' @aliases determineProperty,GamePropertySimpleGame-method
setMethod(
  "determineProperty",
  signature="GamePropertySimpleGame",
  definition=function(.Object){
    A<-.Object@A
    result=logicIsSimpleGame(A)
    return(result)
  }
)

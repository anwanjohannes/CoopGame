#' @name isNonnegativeGame
#' @title isNonnegativeGame - check if TU game is a nonnegative game.
#' @description isNonnegativeGame checks if a TU game is a nonnegative game.
#' A TU game is a nonnegative game if the game vector does not 
#' contain any negative entries.
#' @aliases isNonnegativeGame
#' @export isNonnegativeGame
#' @template author/JS
#' @inheritParams CoopGameBaseParams
#' @return \code{TRUE} if the game is nonnegative, else \code{FALSE}.
#' @examples
#' #Nonnegative game
#' library(CoopGame) 
#' A<-c(0,0,0,0,1,1,1)
#' isNonnegativeGame(A)
#'
#' #Example for game which not nonnegative 
#' library(CoopGame)
#' A<-c(0,0,0,0,-1.1,1,2)
#' isNonnegativeGame(A)
#' 
isNonnegativeGame<-function(A){
  isNonnegative=GamePropertyNonnegativeGame(A)
  return(determineProperty(isNonnegative))
}

logicIsNonnegativeGame=function(A){
  boolRetVal=TRUE
  n=getNumberOfPlayers(A)
  N=length(A)
  tolerance = -1e-15
  for (i in 1:N)
  {
    if (A[i]<tolerance)    
    {
      boolRetVal = FALSE
      return(boolRetVal)
    }
  }
  return(boolRetVal)
}

#' @title GamePropertyNonnegativeGame
#' @noRd
#' @include GameProperty.R
#' #@exportClass GamePropertyNonnegativeGame

setClass(
  "GamePropertyNonnegativeGame",
  contains = "GameProperty"
)

#' @title Constructor for GamePropertyNonnegativeGame
#' @noRd
#' @template author/JS
#' @name GamePropertyNonnegativeGame
#' #@export
GamePropertyNonnegativeGame<-function(A){
  retGamePropertyNonnegativeGame=methods::new("GamePropertyNonnegativeGame",A)
  return(retGamePropertyNonnegativeGame)
}

#' @rdname determineProperty-methods
#' @aliases determineProperty,GamePropertyNonnegativeGame-method
setMethod(
  "determineProperty",
  signature="GamePropertyNonnegativeGame",
  definition=function(.Object){
    A<-.Object@A
    result=logicIsNonnegativeGame(A)
    return(result)
  }
)

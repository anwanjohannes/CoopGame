#' @name isEssentialGame
#' @title isEssentialGame
#' @description Checks if a TU game for n players is essential.
#' A game is said to be essential, if the players generate more in the grand coalition   
#' than the sum of the values of the singleton coalitions.
#' A game is essential, if \deqn{v(N) > \sum v({i})}. \cr
#' For an essential game the imputation set is nonempty and consists of more than one point.
#' @aliases isEssentialGame
#' @export isEssentialGame
#' @template author/MM
#' @template author/JS
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 23
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P p. 408 
#' @inheritParams CoopGameBaseParams
#' @return \code{TRUE} if the game is essential, else \code{FALSE}. 
#' @examples
#' # Example of an essential game
#' library(CoopGame)
#' A <- c(0,0,0,60,60,60,72)
#' isEssentialGame(A)
#'
#' # Example of a game that is not essential  
#' library(CoopGame)
#' B <- c(30,30,15,60,60,60,72)
#' isEssentialGame(B)
#'
#' # Example of a game that is not essential 
#' library(CoopGame)
#' C <- c(20,20,32,60,60,60,72)
#' isEssentialGame(C)
#'
isEssentialGame<-function(A){
  isE=GamePropertyEssentiality(A)
  return(determineProperty(isE))
}

logicIsEssentialGame <- function(A) {
  numberOfPlayers <- getNumberOfPlayers(A)
  isEssential <- FALSE
  scVal <- sum(A[1:numberOfPlayers])
  gcVal <- A[length(A)]

  if (gcVal > scVal) {
    isEssential <- TRUE
  } else {
    isEssential <- FALSE
  }

  return(isEssential)
}

#' @title GamePropertyEssentiality
#' @noRd
#' @description Class for implementation of essentiality concept
#' @include GameProperty.R
#' #@exportClass GamePropertyEssentiality

setClass(
  "GamePropertyEssentiality",
  contains = "GameProperty"
)

#' @title Constructor for GamePropertyEssentiality
#' @noRd
#' @template author/JA
#' @name GamePropertyEssentiality
#' #@export
GamePropertyEssentiality<-function(A){
  retGamePropertyEssentiality=methods::new("GamePropertyEssentiality",A)
  return(retGamePropertyEssentiality)
}

#' @rdname determineProperty-methods
#' @aliases determineProperty,GamePropertyEssentiality-method
setMethod(
  "determineProperty",
  signature="GamePropertyEssentiality",
  definition=function(.Object){
    A<-.Object@A
    result=logicIsEssentialGame(A)
    return(result)
  }
)




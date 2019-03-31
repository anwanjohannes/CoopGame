#' @name is1ConvexGame
#' @title is1ConvexGame - check out if TU game is 1-Convex.
#' @description is1ConvexGame checks if a TU game is 1-convex.
#' A TU game is 1-convex if and only if the following condition holds true:
#' Let \code{S} be a nonempty coalition. Whenever all
#' players outside \code{S} receive their payoffs according to the
#' utopia payoff of the game, then the remaining part of the total savings 
#' is at least \code{v(S)}. 
#' @aliases is1ConvexGame
#' @export is1ConvexGame
#' @template author/JS
#' @template cites/DRIESSEN_1998
#' @templateVar DRIESSEN_1998_P p. 73
#' @inheritParams CoopGameBaseParams
#' @return \code{TRUE} if the game is 1-convex, else \code{FALSE}
#' @examples
#' #1-convex game (taken from book by T. Driessen, p. 75)
#' library(CoopGame)
#' A=c(0,0,0,9,9,15,18)
#' is1ConvexGame(A)
#'
#' #Example for game which not 1-convex 
#' library(CoopGame)
#' A=c(1:7)
#' is1ConvexGame(A)
#' 
is1ConvexGame<-function(A){
  is1C=GameProperty1Convexity(A)
  return(determineProperty(is1C))
}

logicIs1ConvexGame=function(A){
  N <- length(A)
  gapFunction <- getGapFunctionCoefficients(A)
  minGap <- min(gapFunction)
  gapGrandCoalition <- gapFunction[N]
  result <- FALSE
  result <- ((minGap >= 0) & (gapGrandCoalition <= minGap))
  return(result)
}

#' @title GameProperty1Convexity
#' @noRd
#' @include GameProperty.R
#' #@exportClass GameProperty1Convexity

setClass(
  "GameProperty1Convexity",
  contains = "GameProperty"
)

#' @title Constructor for GameProperty1Convexity
#' @noRd
#' @template author/JS
#' @name GameProperty1Convexity
#' #@export
GameProperty1Convexity<-function(A){
  retGameProperty1Convexity=methods::new("GameProperty1Convexity",A)
  return(retGameProperty1Convexity)
}

#' @rdname determineProperty-methods
#' @aliases determineProperty,GameProperty1Convexity-method
setMethod(
  "determineProperty",
  signature="GameProperty1Convexity",
  definition=function(.Object){
    A<-.Object@A
    result=logicIs1ConvexGame(A)
    return(result)
  }
)

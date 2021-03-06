#' @name isSemiConvexGame
#' @title isSemiConvexGame - check if TU game is semiconvex.
#' @description isSemiConvexGame checks if a TU game is semiconvex.
#' A TU game is semiconvex if and only if the following conditions hold true:
#' The gap function of any single player \code{i} is minimal among the gap 
#' function values of coalitions \code{S} containing player \code{i}. 
#' Also, the gap function itself is required to be nonnegative.
#' @aliases isSemiConvexGame
#' @export isSemiConvexGame
#' @template author/JS
#' @template cites/DRIESSEN_TIJS_1985
#' @templateVar DRIESSEN_TIJS_1985_P p. 229--247
#' @template cites/DRIESSEN_1998
#' @templateVar DRIESSEN_1998_P p. 76
#' @inheritParams CoopGameBaseParams
#' @return \code{TRUE} if the game is semiconvex, else \code{FALSE}. 
#' @examples
#' #Example of a semiconvex game 
#' library(CoopGame)
#' A<-c(3,4,5,9,10,11,18)
#' isSemiConvexGame(A)
#'
#' #Example of a game which not semiconvex 
#' library(CoopGame)
#' A=c(1:7)
#' isSemiConvexGame(A)
#' 
isSemiConvexGame<-function(A){
  isSemiC=GamePropertySemiConvexity(A)
  return(determineProperty(isSemiC))
}

logicIsSemiConvexGame=function(A){
  N <- length(A)
  n <- getNumberOfPlayers(A)
  bm <- createBitMatrix(n,A)
  result <- TRUE
  gapFunction <- getGapFunctionCoefficients(A)
  minGap <- min(gapFunction)
  if (minGap < 0){
    result <- FALSE
    return(result)
  }
  for (i in 1:n) {
    bmIndices=which(bm[,i]==1,1)
    gap_i = gapFunction[i]
    for (j in bmIndices){
      if (gapFunction[j] < gap_i){
        result <- FALSE
        return(result)
      }
    }
  }
  return(result)
}

#' @title GamePropertySemiConvexity
#' @noRd
#' @include GameProperty.R
#' #@exportClass GamePropertySemiConvexity

setClass(
  "GamePropertySemiConvexity",
  contains = "GameProperty"
)

#' @title Constructor for GamePropertySemiConvexity
#' @noRd
#' @template author/JS
#' @name GamePropertySemiConvexity
#' #@export
GamePropertySemiConvexity<-function(A){
  retGamePropertySemiConvexity=methods::new("GamePropertySemiConvexity",A)
  return(retGamePropertySemiConvexity)
}

#' @rdname determineProperty-methods
#' @aliases determineProperty,GamePropertySemiConvexity-method
setMethod(
  "determineProperty",
  signature="GamePropertySemiConvexity",
  definition=function(.Object){
    A<-.Object@A
    result=logicIsSemiConvexGame(A)
    return(result)
  }
)

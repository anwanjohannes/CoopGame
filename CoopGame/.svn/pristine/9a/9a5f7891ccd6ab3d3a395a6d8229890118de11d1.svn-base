#' @name isSymmetricGame
#' @title isSymmetricGame - check if a TU game is symmetric.
#' @description isSymmetricGame checks if a TU game is symmetric.
#' A TU game is symmetric if and only if the values of all  
#' coalitions containing the same number of players are identical.
#' @aliases isSymmetricGame
#' @export isSymmetricGame
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 12
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 26
#' @inheritParams CoopGameBaseParams
#' @return \code{TRUE} if the game is symmetric, else \code{FALSE}.
#' @examples
#' #Example of a symmetric game
#' library(CoopGame) 
#' A<-c(3,3,3,10,10,10,17)
#' isSymmetricGame(A)
#'
#' #Example of a game which is not symmetric 
#' library(CoopGame) 
#' A=c(1:7)
#' isSymmetricGame(A)
#' 
isSymmetricGame<-function(A){
  isSymm=GamePropertySymmetry(A)
  return(determineProperty(isSymm))
}

logicIsSymmetricGame=function(A){
  boolRetVal=TRUE
  n=getNumberOfPlayers(A)
  N=length(A)
  bm=as.data.frame(createBitMatrix(n,A))
  players=1:n
  tolerance <- 1e-12
  i <- 1
  numberOfCurrSetOld <- 1
  valueComp <- A[i]
  while (i < N){
    i <- i+1
    # Use structure of bitMatrix bm
    # with i increasing, the number of players involved in nondecreasing
    numberOfCurrSet<-sum(bm[i,1:n])
    if (numberOfCurrSet > numberOfCurrSetOld)
    {
      valueComp <- A[i]
      numberOfCurrSetOld <- numberOfCurrSet
    }
    else 
    {
      value <- bm[[i,"cVal"]]
      if (abs(value-valueComp)>tolerance)
      {
        boolRetVal <- FALSE
      }
    }
  }
  return(boolRetVal)
}

#' @title GamePropertySymmetry
#' @noRd
#' @include GameProperty.R
#' #@exportClass GamePropertySymmetry

setClass(
  "GamePropertySymmetry",
  contains = "GameProperty"
)

#' @title Constructor for GamePropertySymmetry
#' @noRd
#' @template author/JS
#' @name GamePropertySymmetry
#' #@export
GamePropertySymmetry<-function(A){
  retGamePropertySymmetry=methods::new("GamePropertySymmetry",A)
  return(retGamePropertySymmetry)
}

#' @rdname determineProperty-methods
#' @aliases determineProperty,GamePropertySymmetry-method
setMethod(
  "determineProperty",
  signature="GamePropertySymmetry",
  definition=function(.Object){
    A<-.Object@A
    result=logicIsSymmetricGame(A)
    return(result)
  }
)

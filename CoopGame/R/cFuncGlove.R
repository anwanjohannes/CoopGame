#' @name cFuncGloveValue
#' @title cFuncGloveValue
#' @description \strong{Coalition value for the glove game: }\cr
#' For further information see \link{cFuncGlove}
#' @aliases cFuncGloveValue
#' @export cFuncGloveValue
#' @template author/AT
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 155 ff.
#' @template param/S
#' @template param/L
#' @template param/R
#' @return Number of matched pairs of gloves for given coalition \code{S}
#' @examples
#' library(CoopGame)
#' cFuncGloveValue(S=c(1,2), L=c(1,2), R=c(3)) 
#' #[1] 0
#' 
cFuncGloveValue<-function(S,L,R){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S, n=length(union(L,R)))
  stopOnInvalidLeftRightCFuncGlove(paramCheckResult,L=L,R=R,N=union(L,R))
  logicCFuncGloveValue(S,L,R)
}

#' @name cFuncGloveVector
#' @title cFuncGloveVector
#' @description \strong{Game vector for the glove game: }\cr
#' For further information see \link{cFuncGlove}
#' @aliases cFuncGloveVector
#' @export cFuncGloveVector
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 155 ff. 
#' @template param/n
#' @template param/L
#' @template param/R
#' @return The return is a numeric vector of the specified glove game 
#' @examples 
#' #Example with three players, players 1 and 2 hold 
#' #a left-hand glove, player 3 holds a right-hand glove
#' library(CoopGame)
#' (A <- cFuncGloveVector(3, L=c(1,2), R=c(3)))
#' #An object of class "GameVector"
#' #[1]  0 0 0 0 1 1 1
#'
cFuncGloveVector<-function(n,L,R){
  gameVector <- cFuncGlove(n,L,R)@A
  return(gameVector)
}


logicCFuncGloveValue<-function(S, L, R){
  retVal <-0

  #initialize grand coalition N
  numberOfPlayers <-length(L)+length(R)
  N<-as.numeric(1:numberOfPlayers)
  
  compareVector<-c()
  #left side
  compareVector[1]<-length(intersect(S, L))
  #right side
  compareVector[2]<-length(intersect(S, R))
  #return min value of intersected sets' length
  retVal<-min(compareVector, na.rm = TRUE)

  return (retVal)
}

#' @title cFuncGlove - class of Glove game
#' @description Class for glove coalition function, 
#' inherits methods and slots from class \linkS4class{CoopGameCFunc}.
#' @include CoopGameCFunc.R
#' @exportClass cFuncGlove
#' @template slot/L
#' @template slot/R
setClass(
  "cFuncGlove",
  representation(L="numeric",R="numeric"),
  contains = "CoopGameCFunc",
  validity = function(object){
    paramCheckResult=getEmptyParamCheckResult()
    stopOnInvalidLeftRightCFuncGlove(paramCheckResult,L=object@L,R=object@R,N=union(object@L,object@R))
  }
)

setMethod(
  f="initialize",
  signature = "cFuncGlove",
  definition=function(.Object,...,L=L,R=R){
    .Object@L<-L
    .Object@R<-R
    .Object<-methods::callNextMethod(.Object, ...)
    return(.Object)
  }
)

#' @title Constructor of cFuncGlove
#' @description \strong{Constructor for a glove game:} \cr
#' We have a set of players \code{L} with left-hand gloves and 
#' a set of players \code{R} with right-hand gloves
#' The worth of a coalition \code{S} equals the number of 
#' pairs of gloves the members of \code{S} can make.
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 155 ff.
#' @template param/n
#' @template param/L
#' @template param/R
#' @name cFuncGlove
#' @export
#' @section Related Functions: 
#' \link{cFuncGloveValue}, \link{cFuncGloveVector}
#' @examples 
#' #Example with three players, players 1 and 2 hold 
#' #a left-hand glove, player 3 holds a right-hand glove
#' library(CoopGame)
#' (v<-cFuncGlove(n=3,L=c(1,2), R=c(3)))
#' #An object of class "cFuncGlove"
#' #Slot "L":
#' #[1] 1 2
#' #
#' #Slot "R":
#' #[1] 3
#' #
#' #Slot "A":
#' #An object of class "GameVector"
#' #[1] 0 0 0 0 1 1 1
#' #
#' #Slot "n":
#' # [1] 3
#' 
cFuncGlove<-function(n,L,R){
  retCGlove=methods::new("cFuncGlove",n=n,L=L,R=R)
  return(retCGlove)
}


#' @rdname getCoalitionValue-methods
#' @aliases getCoalitionValue,cFuncGlove-method
setMethod(
  "getCoalitionValue",
  signature="cFuncGlove",
  definition=function(.Object,S,...){
    return(logicCFuncGloveValue(S,L=.Object@L,R=.Object@R))
  }
)



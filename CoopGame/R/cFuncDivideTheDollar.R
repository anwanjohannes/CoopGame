#' @name cFuncDivideTheDollarValue
#' @title cFuncDivideTheDollarValue
#' @description \strong{Coalition value for a divide-the-dollar game:} \cr
#' For further information see \link{cFuncDivideTheDollar} 
#' @aliases cFuncDivideTheDollarValue
#' @export cFuncDivideTheDollarValue
#' @template author/MM
#' @template author/JS
#' @template cites/JACKSON_2008
#' @templateVar JACKSON_2008_P p. 413 ff.
#' @template param/S
#' @template param/n
#' @return value of coalition
#' @examples
#' library(CoopGame)
#' S <- c(1,2)
#' cFuncDivideTheDollarValue(S, n = 3)
#' #Output: 1
cFuncDivideTheDollarValue<-function(S,n){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S, n=n)
  stopOnInvalidNumberOfPlayers(paramCheckResult,n=n)
  logicCFuncDivideTheDollarValue(S,n)
}

#' @name cFuncDivideTheDollarVector
#' @title cFuncDivideTheDollarVector
#' @description \strong{Coalition vector for a divide-the-dollar game:} \cr
#' For further information see \link{cFuncDivideTheDollar} 
#' @aliases cFuncDivideTheDollarVector
#' @export cFuncDivideTheDollarVector
#' @template author/JA
#' @template author/JS
#' @template cites/JACKSON_2008
#' @templateVar JACKSON_2008_P p. 413 ff.
#' @template param/n
#' @return The return value is a numeric vector for the specified divide-the-dollar game 
#' @examples
#' library(CoopGame) 
#' (A <- cFuncDivideTheDollarVector(n=4))
#' #Output: 
#' #An object of class "GameVector"
#' # [1] 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1
cFuncDivideTheDollarVector<-function(n){
  gameVector <- cFuncDivideTheDollar(n)@A
  return(gameVector)
}


logicCFuncDivideTheDollarValue <- function(S, n) {
  result <- 0
  if (length(S) >= n / 2) {
    result <- 1
  }
  return(result)
}

#' @title cFuncDivideTheDollar - class for divide-the-dollar game
#' @description Class for divide the dollar coalition function, 
#' inherits methods and slots from class \linkS4class{CoopGameCFunc}.
#' @include CoopGameCFunc.R
#' @exportClass cFuncDivideTheDollar
setClass(
  "cFuncDivideTheDollar",
  contains = "CoopGameCFunc"
)

setMethod(
  f="initialize",
  signature = "cFuncDivideTheDollar",
  definition=function(.Object,...){
    .Object<-methods::callNextMethod(.Object, ...)
    return(.Object)
  }
)

#' @title Constructor for cFuncDivideTheDollar
#' @description  \strong{Constructor for a specified divide-the-dollar game:} \cr
#' Returns a divide-the-dollar game with \code{n} players: \cr
#' This sample majority game is taken from the book 'Social and Economic Networks' by Matthew O. Jackson (see p. 413 ff.). 
#' If coalition \code{S} has at least \code{n/2} members it generates a value of \code{1}, otherwise \code{0}. 
#' @template author/JA
#' @template author/JS
#' @template cites/JACKSON_2008
#' @templateVar JACKSON_2008_P p. 413 ff.
#' @template param/n
#' @return An S4 object representing the specified divide-the-dollar game
#' @name cFuncDivideTheDollar
#' @export
#' @section Related Functions: 
#' \link{cFuncDivideTheDollarValue}, \link{cFuncDivideTheDollarVector}
#' @examples 
#' #Example with four players
#' library(CoopGame)
#' (v<-cFuncDivideTheDollar(n=4))
#' #An object of class "cFuncDivideTheDollar"
#' #Slot "A":
#' #An object of class "GameVector"
#' # [1] 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1
#' #
#' #Slot "n":
#' # [1] 4
cFuncDivideTheDollar<-function(n){
  retCFuncDivideTheDollar=methods::new("cFuncDivideTheDollar",n=n)
  return(retCFuncDivideTheDollar)
}


#' @rdname getCoalitionValue-methods
#' @aliases getCoalitionValue,cFuncDivideTheDollar-method
setMethod(
  "getCoalitionValue",
  signature="cFuncDivideTheDollar",
  definition=function(.Object,S,...){
    return(logicCFuncDivideTheDollarValue(S,n=.Object@n))
  }
)



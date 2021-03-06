#' @name cFuncDictatorValue
#' @title cFuncDictatorValue
#' @description \strong{Coalition value for a dictator game:} \cr
#' For further information see \link{cFuncDictator} 
#' @aliases cFuncDictatorValue
#' @export cFuncDictatorValue
#' @template author/JA
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 295
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 764
#' @template param/S
#' @template param/dictator
#' @return \code{1} if dictator is involved, \code{0} otherwise.
#' @examples 
#' library(CoopGame)
#' cFuncDictatorValue(S=c(1,2,3),dictator=2)
#' #Output:
#' #[1] 1
cFuncDictatorValue<-function(S,dictator){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S)
  stopOnInvalidDictator(paramCheckResult,dictator)
  logicCFuncDictatorValue(S,dictator)
}


logicCFuncDictatorValue=function(S,dictator){
  retVal=logicCFuncUnanimityValue(S,T=dictator)
  return (retVal)
}

#' @name cFuncDictatorVector
#' @title cFuncDictatorVector
#' @description \strong{Coalition vector for an apex game:} \cr
#' For further information see \link{cFuncApex} 
#' @aliases cFuncDictatorVector
#' @export cFuncDictatorVector
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 295
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 764
#' @template param/n
#' @template param/dictator
#' @return  Game Vector where each element contains \code{1} if dictator is involved, \code{0} otherwise.
#' @examples 
#' library(CoopGame)
#' cFuncDictatorVector(n=3,dictator=2)
#' #Output:
#' #An object of class "GameVector"
#' #[1] 0 1 0 1 0 1 1
#' 
cFuncDictatorVector<-function(n,dictator){
  gameVector <- cFuncDictator(n,dictator)@A
  return(gameVector)
}


#' @title cFuncDictator - class for coalition function to dictator
#' @description Class for dictator coalition function, 
#' inherits methods and slots from class \linkS4class{cFuncUnanimity}.
#' Dictator game is a special case of an unanimity game, in which T one consists one player, the so called dictator.
#' Here a coalitions wins if and only if the dictator is part of the coalition.
#' @include cFuncUnanimity.R
#' @exportClass cFuncDictator

setClass(
  "cFuncDictator",
  contains = "cFuncUnanimity",
  validity = function(object){
    paramCheckResult=getEmptyParamCheckResult()
    stopOnInvalidDictator(paramCheckResult,dictator=object@T,n=object@n)
  }
)

setMethod(
  f="initialize",
  signature = "cFuncDictator",
  definition=function(.Object,n,dictator){
    .Object<-methods::callNextMethod(.Object, n,T=dictator)
    return(.Object)
  }
)

#' @title Constructor for cFuncDictator
#' @description \strong{Constructor for a dictator game:} \cr
#' Any coalitions including the dictator receive coalition 
#' value \code{1}. All the other coalitions, i.e. each and 
#' every coalition not containing the dictator, receives 
#' coalition value \code{0}.
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 295
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 764
#' @template author/JA
#' @template author/JS
#' @name cFuncDictator
#' @template param/n
#' @template param/dictator
#' @return An S4 object representing the specified dictator game
#' @export
#' @section Related Functions: 
#' \link{cFuncDictatorValue}, \link{cFuncDictatorVector}
#' @examples
#' library(CoopGame) 
#' cFuncDictator(n=3,dictator=2)
#' #Output:
#' #An object of class "cFuncDictator"
#' #Slot "T":
#' #[1] 2
#'
#' #Slot "A":
#' #An object of class "GameVector"
#' #[1] 0 1 0 1 0 1 1
#' 
#' #Slot "n":
#' #An object of class "NumberOfPlayers"
#' #[1] 3
#' 
cFuncDictator<-function(n,dictator){
  retCFuncDictator=methods::new("cFuncDictator",n=n,dictator=dictator)
  return(retCFuncDictator)
}
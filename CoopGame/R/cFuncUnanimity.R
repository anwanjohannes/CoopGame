#' @name cFuncUnanimityValue
#' @title cFuncUnanimityValue
#' @description \strong{Coalition value for a specified unanimity game:}\cr
#' For further information see \link{cFuncUnanimity}
#' @aliases cFuncUnanimityValue
#' @export cFuncUnanimityValue
#' @template author/JA
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 152
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 764
#' @template param/S
#' @param T represents coalition which is subset of grand coalition N and neccessary for generating value
#' @return  \code{1} if all players of coalition 'T' are included in 'S'
#' else \code{0}
#' @examples 
#' library(CoopGame)
#' cFuncUnanimityValue(S=c(1,2,3),T=c(2))
#' #Output:
#' #[1] 1
#' 
cFuncUnanimityValue<-function(S,T){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S)
  stopOnInvalidCoalitionS(paramCheckResult,S=T)
  logicCFuncUnanimityValue(S,T)
}


logicCFuncUnanimityValue=function(S,T){
  retVal=0
  playersTinS = intersect(S, T)
  checkTinS = setequal(playersTinS, T)

  if (checkTinS) {
    retVal=1
  }else{
    retVal=0
  }
  return (retVal)
}

#' @name cFuncUnanimityVector
#' @title cFuncUnanimityVector
#' @description \strong{Game Vector for a specified unanimity game:}\cr
#' For further information see \link{cFuncUnanimity}
#' @aliases cFuncUnanimityVector
#' @export cFuncUnanimityVector
#' @template author/JA
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 152
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 764
#' @template param/n
#' @param T represents coalition which is subset of grand coalition N and neccessary for generating value
#' @return  Game Vector where each element contains \code{1} if all players of coalition 'T' are included in 'S'
#' else \code{0}
#' @examples 
#' library(CoopGame)
#' cFuncUnanimityVector(n=3,T=c(2))
#' #Output:
#' #An object of class "GameVector"
#' #[1] 0 1 0 1 0 1 1

cFuncUnanimityVector<-function(n,T){
  gameVector <- cFuncUnanimity(n,T)@A
  return(gameVector)
}


#' @title cFuncUnanimity - class for coalition function to unanimity
#' @description Class for unanimity coalition function,
#' inherits methods and slots from class \linkS4class{CoopGameCFunc}.
#' @include CoopGameCFunc.R
#' @exportClass cFuncUnanimity
#' @template slot/T
setClass(
  "cFuncUnanimity",
  representation(T="numeric"),
  contains = "CoopGameCFunc",
  validity = function(object){
    paramCheckResult=getEmptyParamCheckResult()
    stopOnInvalidCoalitionS(paramCheckResult,S=object@T)
  }
)

setMethod(
  f="initialize",
  signature = "cFuncUnanimity",
  definition=function(.Object,...,T=T){
    .Object@T<-T
    .Object<-methods::callNextMethod(.Object, ...)
    return(.Object)
  }
)

#' @title Constructor for cFuncUnanimity
#' @description \strong{Constructor for a specified unanimity game:}\cr
#' @template author/JA
#' @template author/JS
#' @name cFuncUnanimity
#' @template param/n
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 152
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 764
#' @param T represents coalition which is subset of grand coalition N and neccessary for generating value
#' @return An S4 object representing the specified unanimity game
#' @export
#' @section Related Functions: 
#' \link{cFuncUnanimityValue}, \link{cFuncUnanimityVector}
#' @examples 
#' library(CoopGame)
#' cFuncUnanimity(n=3,T=c(2))
#' #Output
#' #An object of class "cFuncUnanimity"
#' #Slot "T":
#' #  [1] 2
#' #
#' #Slot "A":
#' #  An object of class "GameVector"
#' #[1] 0 1 0 1 0 1 1
#' #
#' #Slot "n":
#' #  [1] 3
#' 
cFuncUnanimity<-function(n,T){
  retCFuncUnanimity=methods::new("cFuncUnanimity",n=n,T=T)
  return(retCFuncUnanimity)
}

#' @rdname getCoalitionValue-methods
#' @aliases getCoalitionValue,cFuncUnanimity-method
setMethod(
  "getCoalitionValue",
  signature="cFuncUnanimity",
  definition=function(.Object,S,...){
    return(logicCFuncUnanimityValue(S,T=.Object@T))
  }
)

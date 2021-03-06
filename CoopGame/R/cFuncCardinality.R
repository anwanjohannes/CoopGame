#' @name cFuncCardinalityValue
#' @title cFuncCardinalityValue
#' @description \strong{Coalition value for a cardinality game:} \cr
#' For further information see \link{cFuncCardinality} 
#' @export cFuncCardinalityValue
#' @template author/JA
#' @template author/JS
#' @template param/S
#' @return The return value is the cardinality of S
#' @examples
#' library(CoopGame)
#' S=c(1,2,4,5)
#' cFuncCardinalityValue(S)
#' # Output:
#' # [1] 4

cFuncCardinalityValue<-function(S){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S)
  logicCFuncCardinalityValue(S)
}

#' @name cFuncCardinalityVector
#' @title cFuncCardinalityVector
#' @description \strong{Game vector for a cardinality game:} \cr
#' For further information see \link{cFuncCardinality}
#' @aliases cFuncCardinalityVector
#' @export cFuncCardinalityVector
#' @template author/JA
#' @template author/JS
#' @template param/n
#' @return The return is a numeric vector for the cardinality game
#' @examples 
#' library(CoopGame)
#' (A <- cFuncCardinalityVector(n=4))
#' #An object of class "GameVector"
#' #[1] 1 1 1 1 2 2 2 2 2 2 3 3 3 3 4
#'
cFuncCardinalityVector<-function(n){
  gameVector <- cFuncCardinality(n)@A
  return(gameVector)
}

logicCFuncCardinalityValue=function(S){
  return(length(S))
}

#' @title cFuncCardinality - class for cardinality functions
#' @description Coalition function returning number of coalition's members as value, 
#' inherits methods and slots from class \linkS4class{CoopGameCFunc}.
#' @include CoopGameCFunc.R
#' @exportClass cFuncCardinality

setClass(
  "cFuncCardinality",
  contains = "CoopGameCFunc"
)

setMethod(
  f="initialize",
  signature = "cFuncCardinality",
  definition=function(.Object,...){
    .Object<-methods::callNextMethod(.Object, ...)
    return(.Object)
  }
)

#' @title Constructor for cFuncCardinality
#' @description \strong{Constructor for specified cardinality game:} \cr
#' For a cardinality game the worth of each coalition 
#' is simply the number of the members of the coalition.
#' @template author/JA
#' @template author/JS
#' @template param/n
#' @name cFuncCardinality
#' @return An S4 object representing the specified cardinality game
#' @export
#' @section Related Functions: 
#' \link{cFuncCardinalityValue}, \link{cFuncCardinalityVector}
#' @examples 
#' library(CoopGame)
#' #Example: Cardinality function for four players
#' (v<-cFuncCardinality(n=4))
#' #An object of class "cFuncCardinality"
#' #Slot "A":
#' #An object of class "GameVector"
#' # [1] 1 1 1 1 2 2 2 2 2 2 3 3 3 3 4
#' #
#' #Slot "n":
#' # [1] 4
cFuncCardinality<-function(n){
  retCFuncCardinality=methods::new("cFuncCardinality",n=n)
  return(retCFuncCardinality)
}

#' @rdname getCoalitionValue-methods
#' @aliases getCoalitionValue,cFuncCardinality-method
setMethod(
  "getCoalitionValue",
  signature="cFuncCardinality",
  definition=function(.Object,S,...){
    return(logicCFuncCardinalityValue(S))
  }
)




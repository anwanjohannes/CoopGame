#' @name cFuncQuotaValue
#' @title cFuncQuotaValue
#' @description \strong{Coalition value for a specified quota game:} \cr
#' For further information see \link{cFuncQuota}
#' @aliases cFuncQuotaValue cFuncWeightedMajorityValue
#' @export cFuncQuotaValue
#' @template author/JA
#' @template author/MM
#' @template cites/PELEG_2002
#' @templateVar PELEG_2002_P pp. 195--201
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 17
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 825 ff.
#' @template param/S
#' @template param/w
#' @template param/q
#' @return \code{1} if the sum of the weights of coalition 'S' is greater or equal than quota
#' else \code{0}
#' @examples
#' library(CoopGame)
#' cFuncQuotaValue(S=c(1,2,3),w=c(1,2,3),q=4)
#' #Output:
#' #[1] 1
#' 
cFuncQuotaValue<-function(S,w,q){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S)
  stopOnInvalidWeightVector(paramCheckResult,n=length(w),w)
  logicCFuncQuotaValue(S,w,q)
}


logicCFuncQuotaValue=function(S, w, q) {

  result = 0

  for(p in S){
    result = result + w[p]
  }

  if (result >= q) {
    result = 1
  } else {
    result = 0
  }
  return(result)
}


#' @name cFuncQuotaVector
#' @title cFuncQuotaVector
#' @description \strong{Game vector for a specified quota game:} \cr
#' For further information see \link{cFuncQuota}
#' @aliases cFuncQuotaVector cFuncWeightedMajorityVector
#' @export cFuncQuotaVector
#' @template author/JA
#' @template author/MM
#' @template cites/PELEG_2002
#' @templateVar PELEG_2002_P pp. 195--201
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 17
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 825 ff.
#' @template param/n
#' @template param/w
#' @template param/q
#' @return Game Vector where each element contains \code{1} if the sum of the weights of coalition 'S' is greater or equal than quota
#' else \code{0}
#' @examples
#' library(CoopGame)
#' cFuncQuotaVector(n=3,w=c(1,2,3),q=4)
#' #Output
#' #An object of class "GameVector"
#' #[1] 0 0 0 0 1 1 1
#' 
cFuncQuotaVector<-function(n,w,q){
  gameVector <- cFuncQuota(n,w,q)@A
  return(gameVector)
}


#' @title cFuncQuota - Class of quota coalition function
#' @description Class for quota coalition function, 
#' inherits methods and slots from class \linkS4class{CoopGameCFunc}.
#' @include CoopGameCFunc.R
#' @exportClass cFuncQuota
#' @template slot/w
#' @template slot/q
  setClass(
    "cFuncQuota",
    representation(w="numeric",q="numeric"),
    contains = "CoopGameCFunc",
    validity = function(object){
      paramCheckResult=getEmptyParamCheckResult()
      stopOnInvalidQuota(paramCheckResult,object@q)
      stopOnInvalidWeightVector(paramCheckResult,object@n,object@w)    }
  )
  
  setMethod(
    f="initialize",
    signature = "cFuncQuota",
    definition=function(.Object,...,w,q){
      .Object@w<-w
      .Object@q<-q
      .Object<-methods::callNextMethod(.Object, ...)
      methods::validObject(.Object)
      return(.Object)
    }
  )
  
#' @title Constructor of cFuncQuota
#' @description \strong{Constructor for a specified quota game:}\cr
#' @aliases cFuncQuota cFuncWeightedMajority
#' @template author/JA
#' @name cFuncQuota
#' @template cites/PELEG_2002
#' @templateVar PELEG_2002_P pp. 195--201
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 17
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 825 ff.
#' @template param/n
#' @template param/w
#' @template param/q
#' @return An S4 object representing the specified quota game
#' @export
#' @section Related Functions: 
#' \link{cFuncQuotaValue}, \link{cFuncQuotaVector}
#' @examples 
#' library(CoopGame)
#' cFuncQuota(n=3,w=c(1,2,3),q=4)
#' #Output:
#' #An object of class "cFuncQuota"
#' #Slot "w":
#' #  [1] 1 2 3
#' #
#' #Slot "q":
#' #  [1] 4
#' #
#' #Slot "A":
#' #  An object of class "GameVector"
#' #[1] 0 0 0 0 1 1 1
#' #
#' #Slot "n":
#' #  [1] 3
#' 
  cFuncQuota<-function(n,w,q){
    retCFuncQuota=methods::new("cFuncQuota",n=n,w=w,q=q)
    return(retCFuncQuota)
  }
  
#' @rdname getCoalitionValue-methods
#' @aliases getCoalitionValue,cFuncQuota-method
setMethod(
  "getCoalitionValue",
  signature="cFuncQuota",
  definition=function(.Object,S,...){
    return(logicCFuncQuotaValue(S,w=.Object@w,q=.Object@q))
  }
)
  
  
  


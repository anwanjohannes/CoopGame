#' @name cFuncBankruptcyValue
#' @title cFuncBankruptcyValue
#' @description \strong{Coalition value for a specified Bankruptcy game:} \cr
#' For further information see \link{cFuncBankruptcy}
#' @export cFuncBankruptcyValue
#' @template author/JS
#' @template cites/ONEILL_1982
#' @templateVar ONEILL_1982_P pp. 345 -- 371
#' @template cites/AUMANN_ET_MASCHLER_1985
#' @templateVar AUMANN_ET_MASCHLER_1985_P pp. 195 -- 213
#' @template cites/AUMANN_2002
#' @template cites/GURA_ET_MASCHLER_2008
#' @templateVar GURA_ET_MASCHLER_2008_P pp. 166 ff.
#' @template param/S
#' @template param/d
#' @template param/E
#' @return A positive value if the sum of the claims outside of coalition 'S' is less than \code{E}
#' else \code{0}
#' @examples
#' library(CoopGame)
#' cFuncBankruptcyValue(S=c(1,2),d=c(1,2,3),E=4)
#' #Output
#' #[1] 1

cFuncBankruptcyValue<-function(S,d,E){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S, n=length(d))
  stopOnInvalidClaimsVector(paramCheckResult,n=length(d),d)
  logicCFuncBankruptcyValue(S,d,E)
}


logicCFuncBankruptcyValue=function(S, d, E) {

  result = 0
  sumOtherClaims <- 0
  players <- 1:length(d)
  uninvolvedPlayers=players[-S]

  for(p in uninvolvedPlayers){
    sumOtherClaims = sumOtherClaims + d[p]
  }

  if (sumOtherClaims >= E) {
    result = 0
  } else {
    result = E - sumOtherClaims
  }
  return(result)
}


#' @name cFuncBankruptcyVector
#' @title cFuncBankruptcyVector
#' @description \strong{Game vector for a specified Bankruptcy game:} \cr
#' For further information see \link{cFuncBankruptcy}
#' @export cFuncBankruptcyVector
#' @template author/JS
#' @template cites/ONEILL_1982
#' @templateVar ONEILL_1982_P pp. 345 -- 371
#' @template cites/AUMANN_ET_MASCHLER_1985
#' @templateVar AUMANN_ET_MASCHLER_1985_P pp. 195 -- 213
#' @template cites/AUMANN_2002
#' @template param/n
#' @template param/d
#' @template param/E
#' @return Game Vector where each element contains a positive value if the sum of the claims outside of coalition 'S' is less than \code{E}
#' else \code{0}
#' @examples
#' library(CoopGame)
#' cFuncBankruptcyVector(n=3,d=c(1,2,3),E=4)
#' #Output
#' #An object of class "GameVector"
#' #[1] 0 0 1 1 2 3 4
#' 
cFuncBankruptcyVector<-function(n,d,E){
  gameVector <- cFuncBankruptcy(n,d,E)@A
  return(gameVector)
}


#' @title cFuncBankruptcy - Class of Bankruptcy coalition function
#' @description Class for Bankruptcy coalition function, 
#' inherits methods and slots from class \linkS4class{CoopGameCFunc}.
#' @include CoopGameCFunc.R
#' @exportClass cFuncBankruptcy
#' @template slot/d
#' @template slot/E
  setClass(
    "cFuncBankruptcy",
    representation(d="numeric",E="numeric"),
    contains = "CoopGameCFunc",
    validity = function(object){
      paramCheckResult=getEmptyParamCheckResult()
      stopOnInvalidEstate(paramCheckResult,object@E)
      stopOnInvalidWeightVector(paramCheckResult,object@n,object@d)    }
  )
  
  setMethod(
    f="initialize",
    signature = "cFuncBankruptcy",
    definition=function(.Object,...,d,E){
      .Object@d<-d
      .Object@E<-E
      .Object<-methods::callNextMethod(.Object, ...)
      methods::validObject(.Object)
      return(.Object)
    }
  )
  
#' @title Constructor of cFuncBankruptcy
#' @description \strong{Constructor for a specified Bankruptcy game:}\cr
#' @template author/JS
#' @name cFuncBankruptcy
#' @template cites/ONEILL_1982
#' @templateVar ONEILL_1982_P pp. 345 -- 371
#' @template cites/AUMANN_ET_MASCHLER_1985
#' @templateVar AUMANN_ET_MASCHLER_1985_P pp. 195 -- 213
#' @template cites/AUMANN_2002
#' @template param/n
#' @template param/d
#' @template param/E
#' @return An S4 object representing the specified Bankruptcy game
#' @export
#' @section Related Functions: 
#' \link{cFuncBankruptcyValue}, \link{cFuncBankruptcyVector}
#' @examples 
#' library(CoopGame)
#' cFuncBankruptcy(n=3,d=c(1,2,3),E=4)
#' #Output
#' #An object of class "cFuncBankruptcy"
#' #Slot "d":
#' #  [1] 1 2 3
#' #
#' #Slot "E":
#' #  [1] 4
#' #
#' #Slot "A":
#' #  An object of class "GameVector"
#' #[1] 0 0 1 1 2 3 4
#' #
#' #Slot "n":
#' #  [1] 3

  cFuncBankruptcy<-function(n,d,E){
    retCFuncBankruptcy=methods::new("cFuncBankruptcy",n=n,d=d,E=E)
    return(retCFuncBankruptcy)
  }
  
#' @rdname getCoalitionValue-methods
#' @aliases getCoalitionValue,cFuncBankruptcy-method
setMethod(
  "getCoalitionValue",
  signature="cFuncBankruptcy",
  definition=function(.Object,S,...){
    return(logicCFuncBankruptcyValue(S,d=.Object@d,E=.Object@E))
  }
)
  
  
  


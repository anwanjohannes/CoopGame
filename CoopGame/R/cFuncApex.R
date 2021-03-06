#' @title cFuncApexValue
#' @name cFuncApexValue
#' @description \strong{Coalition value for an apex game:} \cr
#' For further information see \link{cFuncApex} 
#' @aliases cFuncApexValue
#' @export cFuncApexValue
#' @template author/AT
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 164 f.
#' @template param/S
#' @template param/n
#' @template param/apexPlayer
#' @return Calculated payoff of coalition S
#' @examples
#' library(CoopGame)
#' cFuncApexValue(c(1,2,3,4),4,3)
#' # Output:
#' # [1] 1
cFuncApexValue<-function(S,n,apexPlayer){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S, n=n)
  stopOnInvalidNumberOfPlayers(paramCheckResult,n)
  stopOnInvalidNumber(paramCheckResult,apexPlayer)
  logicCFuncApexValue(S,n,apexPlayer)
}

#' @title cFuncApexVector
#' @name cFuncApexVector
#' @description \strong{Game vector for an apex game:} \cr
#' For further information see \link{cFuncApex} 
#' @aliases cFuncApexVector
#' @export cFuncApexVector
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 164 f.
#' @template param/n
#' @template param/apexPlayer
#' @return The return is a numeric vector for the apex game
#' @examples 
#' library(CoopGame)
#' (A <- cFuncApexVector(n=4,apexPlayer=3))
#' #An object of class "GameVector"
#' #[1] 0 0 0 0 0 1 0 1 0 1 1 1 1 1 1
#'
cFuncApexVector<-function(n,apexPlayer){
  gameVector <- cFuncApex(n,apexPlayer)@A
  return(gameVector)
}


logicCFuncApexValue<-function(S,n,apexPlayer){
  #sort to be secure
  S<-sort(S)
  #initialize payoff for S
  retVal<-0
  #check whether apexPlayer is element of S and |S| > 1
  if((apexPlayer %in% S) && (length(S) > 1)){
    return (1)
  }
  #initialize the grand coalition N
  N<-c(1:n)   #proposal by JA
  #check whether S = N\{apexPlayer}
  setWithoutApex<-N[-(which(N == apexPlayer))]
  if(identical(as.numeric(S), as.numeric(setWithoutApex))){
    return (1)
  }
  return (retVal)
}

#' @title Class cFuncApex 
#' @description Class for apex coalition function,
#' inherits methods and slots from class \linkS4class{CoopGameCFunc}.
#' @include CoopGameCFunc.R
#' @template author/JA
#' @template author/JS
#' @template slot/apexPlayer
#' @exportClass cFuncApex

setClass(
  "cFuncApex",
  representation(apexPlayer="numeric"),
  contains = "CoopGameCFunc"
)

setMethod(
  f="initialize",
  signature = "cFuncApex",
  definition=function(.Object,...,apexPlayer){
    .Object@apexPlayer=apexPlayer
    .Object<-methods::callNextMethod(.Object, ...)
    return(.Object)
  }
)

#' @title Constructor for cFuncApex
#' @description \strong{Constructor for a specified apex game:} \cr
#' A coalition can only win (and hence obtain the value \code{1}) 
#' if it \cr
#' a) contains both the apex player and one additional player \cr
#' or \cr
#' b) contains all players except for the apex player. \cr
#' Any non-winning coalitions obtain the value \code{0}.
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 164 f.
#' @template param/n
#' @template param/apexPlayer
#' @return An S4 object representing the specified apex game
#' @name cFuncApex
#' @export
#' @section Related Functions: 
#' \link{cFuncApexValue}, \link{cFuncApexVector}
#' @examples 
#' library(CoopGame)
#' #Example with four players, apex player is number 3
#' (v<-cFuncApex(n=4,apexPlayer=3))
#' #An object of class "cFuncApex"
#' #Slot "apexPlayer":
#' #[1] 3
#' 
#' #Slot "A":
#' #An object of class "GameVector"
#' # [1] 0 0 0 0 0 1 0 1 0 1 1 1 1 1 1
#' #
#' #Slot "n":
#' # [1] 4
cFuncApex<-function(n,apexPlayer){
  retCFuncApex=methods::new("cFuncApex",n=n,apexPlayer=apexPlayer)
  return(retCFuncApex)
}


#' @rdname getCoalitionValue-methods
#' @aliases getCoalitionValue,cFuncApex-method
setMethod(
  "getCoalitionValue",
  signature="cFuncApex",
  definition=function(.Object,S,...){
    return(logicCFuncApexValue(S,n=.Object@n,apexPlayer=.Object@apexPlayer))
  }
)



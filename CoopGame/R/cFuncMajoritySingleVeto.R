#' @name cFuncMajoritySingleVetoValue
#' @title cFuncMajoritySingleVetoValue
#' @description \strong{Coalition value for an majority game with a single veto player:} \cr
#' For further information see \link{cFuncMajoritySingleVeto}
#' @aliases cFuncMajoritySingleVetoValue
#' @export cFuncMajoritySingleVetoValue
#' @template author/MM
#' @template cites/JACKSON_2008
#' @templateVar JACKSON_2008_P p. 415
#' @template param/S
#' @template param/vetoPlayer
#' @return \code{1} if vetoPlayer is included in 'S' and 'S' is not a single coalition, \code{0} otherwise
#' @examples
#' library(CoopGame)
#' cFuncMajoritySingleVetoValue(S=c(1,2), vetoPlayer=1)
#' #Output:
#' #[1] 1
#' 
cFuncMajoritySingleVetoValue<-function(S,vetoPlayer){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S)
  stopOnInvalidVetoPlayer(paramCheckResult,vetoPlayer)
  logicCFuncMajoritySingleVetoValue(S,vetoPlayer)
}

logicCFuncMajoritySingleVetoValue=function(S, vetoPlayer) {
  result=0
  if ( (length(S) >= 2) && (vetoPlayer %in% S) ) {
    result=1
  }

  return(result)
}


#' @name cFuncMajoritySingleVetoVector
#' @title cFuncMajoritySingleVetoVector
#' @description \strong{Game vector for an majority game with a single veto player:} \cr
#' For further information see \link{cFuncMajoritySingleVeto}#' @aliases cFuncMajoritySingleVetoVector
#' @export cFuncMajoritySingleVetoVector
#' @template author/MM
#' @template cites/JACKSON_2008
#' @templateVar JACKSON_2008_P p. 415
#' @template param/n
#' @template param/vetoPlayer
#' @return Game Vector where each elements contains \code{1} if vetoPlayer is included in 'S' and 'S' is not a single coalition, \code{0} otherwise
#' @examples
#' library(CoopGame)
#' cFuncMajoritySingleVetoVector(n=3, vetoPlayer=1)
#' #Output:
#' #An object of class "GameVector"
#' #[1] 0 0 0 1 1 0 1
#' 
cFuncMajoritySingleVetoVector<-function(n,vetoPlayer){
  msvg=cFuncMajoritySingleVeto(n,vetoPlayer)
  return(msvg@A)
}

#' @title cFuncMajoritySingleVeto - class of majority game with single veto player
#' @description Class for of majority game with single veto player, 
#' inherits methods and slots from class \linkS4class{CoopGameCFunc}.
#' @include CoopGameCFunc.R
#' @exportClass cFuncMajoritySingleVeto
#' @template slot/vetoPlayer

setClass(
  "cFuncMajoritySingleVeto",
  representation(vetoPlayer="numeric"),
  contains = "CoopGameCFunc",
  validity = function(object){
    paramCheckResult=getEmptyParamCheckResult()
    stopOnInvalidVetoPlayer(paramCheckResult,object@vetoPlayer)
  }
)

setMethod(
  f="initialize",
  signature = "cFuncMajoritySingleVeto",
  definition=function(.Object,...,vetoPlayer=vetoPlayer){
    .Object@vetoPlayer<-vetoPlayer
    .Object<-methods::callNextMethod(.Object, ...)
    methods::validObject(.Object)
    return(.Object)
  }
)

#' @title Constructor of cFuncMajoritySingleVeto
#' @description \strong{Constructor for a specified majority game with a single veto player}\cr
#' If coalition 'S' has at least 2 members and if the veto player is part of the\cr
#' coalition it generates a value of \code{1}, otherwise \code{0}
#' @template author/JA
#' @name cFuncMajoritySingleVeto
#' @template cites/JACKSON_2008
#' @templateVar JACKSON_2008_P p. 415
#' @template param/n
#' @template param/vetoPlayer
#' @return An S4 object representing the specified majority game with a single veto player
#' @export
#' @section Related Functions: 
#' \link{cFuncMajoritySingleVetoValue}, \link{cFuncMajoritySingleVetoVector}
#' @examples
#' library(CoopGame) 
#' cFuncMajoritySingleVeto(n=3, vetoPlayer=1)
#' #Output:
#' #An object of class "cFuncMajoritySingleVeto"
#' #Slot "vetoPlayer":
#' #  [1] 1
#' #
#' #Slot "A":
#' #  An object of class "GameVector"
#' #[1] 0 0 0 1 1 0 1
#' #
#' #Slot "n":
#' #  [1] 3
#' 
cFuncMajoritySingleVeto<-function(n,vetoPlayer){
  retCFuncMajoritySingleVeto=methods::new("cFuncMajoritySingleVeto",n=n,vetoPlayer=vetoPlayer)
  return(retCFuncMajoritySingleVeto)
}

#' @rdname getCoalitionValue-methods
#' @aliases getCoalitionValue,cFuncMajoritySingleVeto-method
setMethod(
  "getCoalitionValue",
  signature="cFuncMajoritySingleVeto",
  definition=function(.Object,S,...){
    return(logicCFuncMajoritySingleVetoValue(S,vetoPlayer=.Object@vetoPlayer))
  }
)



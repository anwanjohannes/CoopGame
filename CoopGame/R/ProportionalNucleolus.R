#' @name proportionalNucleolus
#' @title proportionalNucleolus for n Players
#' @description Calculates the Proportional Nucleolus for a game specified by game vector A.
#' @aliases proportionalNucleolus
#' @export proportionalNucleolus
#' @template author/JA
#' @template author/JS
#' @template cites/YOUNG_ET_AL_1982
#' @templateVar YOUNG_ET_AL_1982_P pp. 463 -- 475
#' @inheritParams CoopGameBaseParams
#' @template param/enableTermOutLP
#' @return the Proportional Nucleolus
#' @export proportionalNucleolus
#' @examples
#' library(CoopGame)
#' A<-c(0,0,0,48,60,72,140)
#' proportionalNucleolus(A)
#' #[1] 28.00000 46.66667 65.33333
proportionalNucleolus <- function(A,enableTermOutLP = FALSE){
  retValue = NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Therefore we do not compute the proportional nucleolus.")
  }
  else
  {
    pcn=ProportionalNucleolus(A=A)
    retVal = calculateNucleolus(pcn,enableTermOutLP=enableTermOutLP)
  }
  return(retVal)
} 


#' @title Constructor for ProportionalNucleolus
#' @noRd
#' @template author/JA
#' @name ProportionalNucleolus
#' #@export
ProportionalNucleolus<-function(A){
  retProportionalNucleolus=methods::new("ProportionalNucleolus",A=A)
  return(retProportionalNucleolus)
}

#' @title ProportionalNucleolus - S4 class for Nucleolus derivatives
#' @noRd
#' @template author/JA
#' @description An S4 class for the Proportional Nucleolus.
#' @include NucleolusBase.R
#'# @exportClass ProportionalNucleolus
setClass(
  "ProportionalNucleolus",
  contains="NucleolusBase"
)

#' @rdname determineExcessCoefficients-methods
#' @aliases determineExcessCoefficients,ProportionalNucleolus-method
setMethod(
  "determineExcessCoefficients",
  signature="ProportionalNucleolus",
  definition=function(.Object){
    N=length(.Object@A)
    return(c(.Object@A[-N],0))
  }
)

#' @rdname initLPBndsObjCoefs-methods
#' @aliases initLPBndsObjCoefs,ProportionalNucleolus-method
setMethod(
  "initLPBndsObjCoefs",
  signature="ProportionalNucleolus",
  definition=function(.Object){
    lpCoopGameUtils<-.Object@LPCoopGameUtils
    n=getNumberOfPlayers(.Object@A)
    setClb(lpCoopGameUtils)<- rep(-Inf, n + 1)
    setCub(lpCoopGameUtils)<- rep(Inf, n + 1)
    setCtype(lpCoopGameUtils)<- rep(GLP_FR, n + 1)
    setObj(lpCoopGameUtils)<- c(rep(0, n), 1)
    eval.parent(substitute(.Object@LPCoopGameUtils<-lpCoopGameUtils))
  }
)

#' @name drawProportionalNucleolus
#' @title drawProportionalNucleolus for n players
#' @description drawProportionalNucleolus draws the proportional nucleolus for 3 or 4 players.
#' @aliases drawProportionalNucleolus
#' @export drawProportionalNucleolus
#' @template author/JA
#' @template cites/YOUNG_ET_AL_1982
#' @templateVar YOUNG_ET_AL_1982_P pp. 463 -- 475
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' library(CoopGame)
#' A<-c(0,0,0,48,60,72,140)
#' drawProportionalNucleolus(A)
drawProportionalNucleolus<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Proportional Nucleolus"){
  A=GameVector(A)
  pgv=proportionalNucleolus(A);
  visualize(A, pointsToDraw=pgv, holdOn=holdOn, colour = colour , label=label, name = name)
}
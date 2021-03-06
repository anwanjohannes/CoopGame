#' @name prenucleolus
#' @title prenucleolus for n-player TU games
#' @description Computes the prenucleolus of a TU game with n players.
#' @aliases prenucleolus
#' @export prenucleolus
#' @template author/DG
#' @template author/JA
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 107 ff.
#' @inheritParams CoopGameBaseParams
#' @template param/enableTermOutLP
#' @return Numeric vector of length n representing the prenucleolus.
#' @examples
#' #Example 5.5.12 from Peleg/Sudhoelter, p. 96
#' prenucleolus(c(0,0,0,10,0,0,2))
#' #Output
#' #[1]  3  3 -4
#' prenucleolus(c(1, 1, 1, 2, 3, 4, 5))
#' prenucleolus(c(0, 0, 0, 0, 5, 5, 8, 9, 10, 8, 13, 15, 16, 17, 21), FALSE)

prenucleolus <- function(A, enableTermOutLP = FALSE){
  pcn=PreNucleolus(A=A)
  return(calculateNucleolus(pcn,enableTermOutLP=enableTermOutLP))
}


#' @title Constructor for PreNucleolus
#' @noRd
#' @template author/DG
#' @name PreNucleolus
#' #@export
PreNucleolus<-function(A){
  retPreNucleolus=methods::new("PreNucleolus",A)
  return(retPreNucleolus)
}

#' @title PreNucleolus - S4 class for Prenucleolus Concept
#' @description An S4 class for Prenucleolus Concept
#' @noRd
#' @template author/DG
#' @include NucleolusBase.R
# @exportClass PreNucleolus
setClass(
  "PreNucleolus",
  contains="NucleolusBase"
)

#' @rdname initLPBndsObjCoefs-methods
#' @aliases initLPBndsObjCoefs,PreNucleolus-method
setMethod(
  "initLPBndsObjCoefs",
  signature="PreNucleolus",
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

#' @rdname checkGamePreconditions-methods
#' @aliases checkGamePreconditions,PreNucleolus-method
setMethod(
  "checkGamePreconditions",
  signature="PreNucleolus",
  definition=function(.Object){
    if(!all(.Object@A>=0)){
      stop("Stopped as at least on coalition value smaller than 0.") 
    }
  }
)

#' @name drawPrenucleolus
#' @title drawPrenucleolus for 3 or 4 players
#' @description drawPrenucleolus draws the prenucleolus for 3 or 4 players.
#' @aliases drawPrenucleolus
#' @export drawPrenucleolus
#' @template author/JA
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 107 ff.
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' A=c(1, 1, 1, 2, 3, 4, 5)
#' drawPrenucleolus(A)
drawPrenucleolus<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Prenucleolus"){
  A=GameVector(A)
  pcn=prenucleolus(A);
  visualize(A, pointsToDraw=pcn, holdOn=holdOn, colour = colour , label=label, name = name)
}

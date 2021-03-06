#' @name disruptionNucleolus
#' @title disruptionNucleolus for n-player TU games
#' @description Computes the disruptionNucleolus of a balanced TU game with n players.
#' @aliases disruptionNucleolus
#' @export disruptionNucleolus
#' @template author/JA
#' @template cites/LITTLECHILD_ET_VAIDYA_1976
#' @templateVar LITTLECHILD_ET_VAIDYA_1976_P pp. 151 -- 161
#' @template param/A
#' @template param/enableTermOutLP
#' @return Numeric vector of length n representing the modified nucleolus
#' @examples
#' 
#' A1<-c(0, 0, 0, 1, 1, 0, 1)
#' disruptionNucleolus(A1)
#' # [1] 1 0 0
#' 
#' A2<-c(0,0,0,0,2,3,4,1,3,2,8,11,6.5,9.5,14)
#' disruptionNucleolus(A2)
#' #[1] 3.193548 4.754839 2.129032 3.922581



disruptionNucleolus <- function(A, enableTermOutLP = FALSE){
  mn=DisruptionNucleolus(A=A)
  retVal=NULL
  if(isBalancedGame(mn@A)){
    retVal=calculateNucleolus(mn,enableTermOutLP=enableTermOutLP)
  }else{
    print("Strict core is empty for specified game therefore no unique solution can be retrieved.")
  }
  return(retVal)
}


#' @title Constructor for Disruption Nucleolus
#' @noRd
#' @template author/JA
#' @name DisruptionNucleolus
#' #@export
DisruptionNucleolus<-function(A, enableTermOutLP = FALSE){
  retDisruptionNucleolus=methods::new("DisruptionNucleolus",A=A)
  return(retDisruptionNucleolus)
}

#' @title DisruptionNucleolus - class for Disruption Nucleolus concept
#' @noRd
#' @template author/JA
#' @include NucleolusBase.R
#' @name DisruptionNucleolus
#' #@exportClass DisruptionNucleolus



setClass(
  "DisruptionNucleolus",
  contains="NucleolusBase"
)

#' @rdname determineExcessCoefficients-methods
#' @aliases determineExcessCoefficients,DisruptionNucleolus-method
setMethod(
  "determineExcessCoefficients",
  signature="DisruptionNucleolus",
  definition=function(.Object){
    A=.Object@A
    N=length(A)
    tfac=sapply(1:((N-1)/2),function(ix){A[N]-A[ix]-A[N-ix]})
    tfac=c(tfac,rev(tfac),0)
    return(tfac)
  }
)

#' @rdname initLPBndsObjCoefs-methods
#' @aliases initLPBndsObjCoefs,DisruptionNucleolus-method
setMethod(
  "initLPBndsObjCoefs",
  signature="DisruptionNucleolus",
  definition=function(.Object){
    lpCoopGameUtils<-.Object@LPCoopGameUtils
    n=getNumberOfPlayers(.Object@A)
    setClb(lpCoopGameUtils)<- c(rep(0, n), -1+1e-16)
    setCub(lpCoopGameUtils)<- c(rep(Inf, n),-1e-16)
    setCtype(lpCoopGameUtils)<- c(rep(GLP_DB, n),GLP_DB)
    setObj(lpCoopGameUtils)<- c(rep(0, n), 1)
    eval.parent(substitute(.Object@LPCoopGameUtils<-lpCoopGameUtils))
  }
)

#' @name drawDisruptionNucleolus
#' @title drawDisruptionNucleolus for n players
#' @description drawDisruptionNucleolus draws the disruption nucleolus for n players.
#' @aliases drawDisruptionNucleolus
#' @export drawDisruptionNucleolus
#' @template author/JA
#' @template cites/LITTLECHILD_ET_VAIDYA_1976
#' @templateVar LITTLECHILD_ET_VAIDYA_1976_P pp. 151 -- 161
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' \dontrun{
#' A<-c(0,0,0,0,2,3,4,1,3,2,8,11,6.5,9.5,14)
#' drawDisruptionNucleolus(A)
#' }
drawDisruptionNucleolus<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Disruption Nucleolus"){
  A=GameVector(A)
  dn=disruptionNucleolus(A);
  visualize(A, pointsToDraw=dn, holdOn=holdOn, colour = colour , label=label, name = name)
}



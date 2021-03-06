#' @name colemanPreventivePowerIndex
#' @title Coleman Preventive Power Index
#' @description Calculates the Coleman Preventive Power Index for a specified simple TU game.
#' Note that in general the Coleman Preventive Power Index is not an efficient vector, 
#' i.e. the sum of its entries is not always 1.
#' Hence no drawing routine for the Coleman Preventive Power Index is provided.
#' @aliases colemanPreventivePowerIndex
#' @export colemanPreventivePowerIndex
#' @template author/JS
#' @template cites/COLEMAN_1971
#' @templateVar COLEMAN_1971_P pp. 269 -- 300
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 120 -- 123
#' @template cites/DE_KEIJZER_2008
#' @templateVar DE_KEIJZER_2008_P p. 18
#' @inheritParams CoopGameBaseParams
#' @return Coleman Preventive Power Index for specified simple game 
#' @examples 
#' A=c(0,0,0,1,1,0,1)
#' colemanPreventivePowerIndex(A) 
#' #result: [1] 1.0000000 0.3333333 0.3333333
#' 
colemanPreventivePowerIndex<-function(A){
  colemanPreventivePower=ColemanPreventivePowerConcept(A)
  return(calculatePointSolution(colemanPreventivePower))
}



logicColemanPreventivePowerIndex=function(A){
  retVal=NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Coleman Preventive Power Index can be retrieved.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    numerator = numeric(n)
    numerator = rawBanzhafIndex(A)
    #the winning coalitions
    wcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    colemanPreventivePower =sapply(c(1:n),function(i){numerator[i]/as.numeric(nrow(wcs))})
    retVal=colemanPreventivePower
  }
  return(retVal)
}

#' @title ColemanPreventivePowerConcept
#' @noRd
#' @description ColemanPreventivePowerConcept
#' @include PointSolutionConcept.R
# @exportClass ColemanPreventivePowerConcept

setClass(
  "ColemanPreventivePowerConcept",
  contains = "PointSolutionConcept"
)

#' @rdname calculatePowerIndex-methods
#' @aliases calculatePowerIndex,ColemanPreventivePowerConcept-method
setMethod(
  "calculatePointSolution",
  signature="ColemanPreventivePowerConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicColemanPreventivePowerIndex(A))
  }
)

#' @title Constructor for ColemanPreventivePowerConcept
#' @noRd
#' @template author/JS
#' @name ColemanPreventivePowerConcept
#' #@export
ColemanPreventivePowerConcept<-function(A){
  retcolemanPreventivePowerIndex=methods::new("ColemanPreventivePowerConcept",A)
  return(retcolemanPreventivePowerIndex)
}





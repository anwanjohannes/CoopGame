#' @name colemanInitiativePowerIndex
#' @title Coleman Initiative Power Index
#' @description Calculates the Coleman Initiative Power Index for a specified simple TU game.
#' Note that in general the Coleman Initiative Power Index is not an efficient vector, 
#' i.e. the sum of its entries is not always 1.
#' Hence no drawing routine for the Coleman Initiative Power Index is provided.
#' @aliases colemanInitiativePowerIndex
#' @export colemanInitiativePowerIndex
#' @template author/JS
#' @template cites/COLEMAN_1971
#' @templateVar COLEMAN_1971_P pp. 269 -- 300
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 120 -- 123
#' @template cites/DE_KEIJZER_2008
#' @templateVar DE_KEIJZER_2008_P p. 18
#' @inheritParams CoopGameBaseParams
#' @return Coleman Initiative Power Index for specified simple game 
#' @examples 
#' A=c(0,0,0,1,1,0,1)
#' colemanInitiativePowerIndex(A) 
#' #result: [1] 0.6 0.2 0.2
#' 
colemanInitiativePowerIndex<-function(A){
  colemanInitiativePower=ColemanInitiativePowerConcept(A)
  return(calculatePointSolution(colemanInitiativePower))
}



logicColemanInitiativePowerIndex=function(A){
  retVal=NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Coleman Initiative Power Index can be retrieved.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    numerator = numeric(n)
    numerator = rawBanzhafIndex(A)
    #the winning coalitions
    wcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    commonDenominator = as.numeric(2^n - nrow(wcs))
    colemanInitiativePower =sapply(c(1:n),function(i){numerator[i]/commonDenominator})
    retVal=colemanInitiativePower
  }
  return(retVal)
}

#' @title ColemanInitiativePowerConcept
#' @noRd
#' @description ColemanInitiativePowerConcept
#' @include PointSolutionConcept.R
# @exportClass ColemanInitiativePowerConcept

setClass(
  "ColemanInitiativePowerConcept",
  contains = "PointSolutionConcept"
)

#' @rdname calculatePowerIndex-methods
#' @aliases calculatePowerIndex,ColemanInitiativePowerConcept-method
setMethod(
  "calculatePointSolution",
  signature="ColemanInitiativePowerConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicColemanInitiativePowerIndex(A))
  }
)

#' @title Constructor for ColemanInitiativePowerConcept
#' @noRd
#' @template author/JS
#' @name ColemanInitiativePowerConcept
#' #@export
ColemanInitiativePowerConcept<-function(A){
  retcolemanInitiativePowerIndex=methods::new("ColemanInitiativePowerConcept",A)
  return(retcolemanInitiativePowerIndex)
}





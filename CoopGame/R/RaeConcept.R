#' @name raeIndex
#' @title raeIndex
#' @description Calculates the Rae Index for a specified simple TU game. 
#' Note that in general the Rae Index is not an efficient vector, 
#' i.e. the sum of its entries is not always 1.
#' Hence no drawing routine for the Rae Index is provided.
#' @aliases raeIndex
#' @export raeIndex
#' @template author/JS
#' @template cites/RAE_1969
#' @templateVar RAE_1969_P pp. 40 -- 56
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 119 -- 120
#' @inheritParams CoopGameBaseParams
#' @return Rae Index for specified simple game 
#' @examples 
#' A=c(0,0,0,1,1,0,1)
#' raeIndex(A) 
#' #result: [1] 0.8 0.6 0.6
#' 
raeIndex<-function(A){
  rae=RaeConcept(A)
  return(calculatePointSolution(rae))
}


logicRaeIndex=function(A){
  retval = NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Rae Index can be retrieved.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    banzhafV = normalizedBanzhafIndex(A)
    rae = 0.5 + 0.5 * banzhafV
    retVal = rae
  }
  return(retVal)
}

#' @title RaeConcept
#' @noRd
#' @description RaeConcept
#' @include PointSolutionConcept.R
# @exportClass RaeConcept

setClass(
  "RaeConcept",
  contains = "PointSolutionConcept"
)

#' @rdname calculatePowerIndex-methods
#' @aliases calculatePowerIndex,RaeConcept-method
setMethod(
  "calculatePointSolution",
  signature="RaeConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicRaeIndex(A))
  }
)

#' @title Constructor for RaeConcept
#' @noRd
#' @template author/JS
#' @name RaeConcept
#' #@export
RaeConcept<-function(A){
  retraeIndex=methods::new("RaeConcept",A)
  return(retraeIndex)
}

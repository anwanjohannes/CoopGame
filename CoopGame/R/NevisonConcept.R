#' @name nevisonIndex
#' @title nevisonIndex
#' @description Calculates the Nevison Index for a specified simple TU game.
#' Note that in general the Nevison Index is not an efficient vector, 
#' i.e. the sum of its entries is not always 1.
#' Hence no drawing routine for the Nevison Index is provided.
#' @aliases nevisonIndex
#' @export nevisonIndex
#' @template author/JS
#' @template cites/NEVISON_1979
#' @templateVar NEVISON_1979_P pp. 39 -- 57
#' @inheritParams CoopGameBaseParams
#' @return Nevison Index for a specified simple game 
#' @examples 
#' A=c(0,0,0,1,1,0,1)
#' nevisonIndex(A) 
#' #result: [1] 0.75 0.50 0.50
#'
nevisonIndex<-function(A){
  nevison=NevisonConcept(A)
  return(calculatePointSolution(nevison))
}




logicNevisonIndex=function(A){
  retVal = NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Nevison Index can be retrieved.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    #the winning coalitions
    wcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    temp=sapply(c(1:n),function(i){sum(wcs[wcs[,i]==1,"cVal"])})
    nevison =sapply(c(1:n),function(i){temp[i]/2^(n-1)})
    retVal = nevison
  }
  return(retVal)
}

#' @title NevisonConcept
#' @noRd
#' @description NevisonConcept
#' @include PointSolutionConcept.R
# @exportClass NevisonConcept

setClass(
  "NevisonConcept",
  contains = "PointSolutionConcept"
)

#' @rdname calculatePowerIndex-methods
#' @aliases calculatePowerIndex,NevisonConcept-method
setMethod(
  "calculatePointSolution",
  signature="NevisonConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicNevisonIndex(A))
  }
)

#' @title Constructor for NevisonConcept
#' @noRd
#' @template author/JS
#' @name NevisonConcept
#' #@export
NevisonConcept<-function(A){
  retnevisonIndex=methods::new("NevisonConcept",A)
  return(retnevisonIndex)
}





#' @name centroidWeberSet
#' @title centroidWeberSet
#' @description Calculates the centroid of the Weber set for specified game.
#' @aliases centroidWeberSet
#' @export centroidWeberSet
#' @template author/JS
#' @template cites/WEBER_1988
#' @templateVar WEBER_1988_P pp. 101--119
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 327 ff.
#' @inheritParams CoopGameBaseParams
#' @return Calculates the centroid of the Weber set for a game specified by a game vector A.
#' @examples
#' #Example for centroid of the Weber set
#' A=c(0,0,0,2,2,3,5)
#' centroidWeberSet(A) 
#' #[1] 1.333333 2.666667 2.666667
#' 
centroidWeberSet<-function(A){
  tv=CentroidWeberSetConcept(A)
  return(calculatePointSolution(tv))
}

logicCentroidWeberSet<-function(A){
  retVal=NULL
  n=getNumberOfPlayers(A)
  N=length(A)
  setVertices=webersetVertices(A)
  
  if(!isEssentialGame(A) || (nrow(setVertices) == 0) ){
    print("Weber set is empty and so no centroid of the Weber set can be retrieved")
  }else{
    setVertices=webersetVertices(A)
    centroid=colSums(setVertices)/nrow(setVertices)
    retVal = centroid
  }
  return(retVal)
}


#' @title CentroidWeberSetConcept - S4 class for centroid weber set concept
#' @name CentroidWeberSetConceptClass
#' @noRd
#' @description S4 class containing logic for solving centroid weber set concept
#' @include PointSolutionConcept.R
#' #@exportClass CentroidWeberSetConcept

setClass(
  "CentroidWeberSetConcept",
  contains = "PointSolutionConcept"
)

#' @title Constructor for CentroidWeberSetConcept
#' @noRd
#' @template author/JS
#' @name CentroidWeberSetConcept
#' @inheritParams CoopGameBaseParams
#'# @export
CentroidWeberSetConcept<-function(A){
  retCentroidWeberSetConcept=methods::new("CentroidWeberSetConcept",A)
  return(retCentroidWeberSetConcept)
}
#' @rdname calculatePointSolution-methods
#' @aliases calculatePointSolution,CentroidWeberSetConcept-method
setMethod(
  "calculatePointSolution",
  signature="CentroidWeberSetConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicCentroidWeberSet(A))
  }
)

#' @name drawCentroidWeberSet
#' @title drawCentroidWeberSet for 3 or 4 players
#' @family CentroidWeberSetConcept
#' @family PointSolutionConcept
#' @description drawCentroidWeberSet draws the centroid of the weber set for 3 or 4 players.
#' @aliases drawCentroidWeberSet
#' @export drawCentroidWeberSet
#' @template author/JS
#' @template cites/WEBER_1988
#' @templateVar WEBER_1988_P pp. 101--119
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 327 ff.
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' A=c(0,0,5,4,8,9,12)
#' drawCentroidWeberSet(A,colour="green")
drawCentroidWeberSet<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "centroid of Weber set"){
  A=GameVector(A)
  pcn=centroidWeberSet(A);
  visualize(A, pointsToDraw=pcn, holdOn=holdOn, colour = colour , label=label, name = name)
}

#' @name centroidReasonableSet
#' @title centroidReasonableSet
#' @description Calculates the centroid of the reasonable set for specified game.
#' @aliases centroidReasonableSet
#' @export centroidReasonableSet
#' @template author/JS
#' @template cites/MILNOR_1953
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 43 ff.
#' @template cites/GERARD_VARET_ET_ZAMIR_1987
#' @templateVar GERARD_VARET_ET_ZAMIR_1987_P pp. 123--143
#' @inheritParams CoopGameBaseParams
#' @return Calculates the centroid of the reasonable set for a game specified by a game vector A.
#' @examples
#' #Example for centroid of the reasonable set
#' A=c(0,0,0,2,2,3,5)
#' centroidReasonableSet(A) 
#' #[1] 1 2 2
#' 
centroidReasonableSet<-function(A){
  tv=CentroidReasonableSetConcept(A)
  return(calculatePointSolution(tv))
}

logicCentroidReasonableSet<-function(A){
  retVal=NULL
  n=getNumberOfPlayers(A)
  N=length(A)
  setVertices=reasonableSetVertices(A)
  
  if(!isEssentialGame(A) || (nrow(setVertices) == 0) ){
    print("Reasonable set is empty and so no centroid of the reasonable set can be retrieved")
  }else{
    setVertices=reasonableSetVertices(A)
    centroid=colSums(setVertices)/nrow(setVertices)
    retVal = centroid
  }
  return(retVal)
}


#' @title CentroidReasonableSetConcept - S4 class for centroid reasonable set concept
#' @name CentroidReasonableSetConceptClass
#' @noRd
#' @description S4 class containing logic for solving centroid reasonable set concept
#' @include PointSolutionConcept.R
#' #@exportClass CentroidReasonableSetConcept

setClass(
  "CentroidReasonableSetConcept",
  contains = "PointSolutionConcept"
)

#' @title Constructor for CentroidReasonableSetConcept
#' @noRd
#' @template author/JS
#' @name CentroidReasonableSetConcept
#' @inheritParams CoopGameBaseParams
#'# @export
CentroidReasonableSetConcept<-function(A){
  retCentroidReasonableSetConcept=methods::new("CentroidReasonableSetConcept",A)
  return(retCentroidReasonableSetConcept)
}
#' @rdname calculatePointSolution-methods
#' @aliases calculatePointSolution,CentroidReasonableSetConcept-method
setMethod(
  "calculatePointSolution",
  signature="CentroidReasonableSetConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicCentroidReasonableSet(A))
  }
)

#' @name drawCentroidReasonableSet
#' @title drawCentroidReasonableSet for 3 or 4 players
#' @family CentroidReasonableSetConcept
#' @family PointSolutionConcept
#' @description drawCentroidReasonableSet draws the centroid of the reasonable set for 3 or 4 players.
#' @aliases drawCentroidReasonableSet
#' @export drawCentroidReasonableSet
#' @template author/JS
#' @template cites/MILNOR_1953
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 43 ff.
#' @template cites/GERARD_VARET_ET_ZAMIR_1987
#' @templateVar GERARD_VARET_ET_ZAMIR_1987_P pp. 123--143
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' A=c(0,0,5,4,8,9,12)
#' drawCentroidReasonableSet(A,colour="green")
drawCentroidReasonableSet<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "centroid of reasonable set"){
  A=GameVector(A)
  pcn=centroidReasonableSet(A);
  visualize(A, pointsToDraw=pcn, holdOn=holdOn, colour = colour , label=label, name = name)
}

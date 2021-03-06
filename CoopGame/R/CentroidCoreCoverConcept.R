#' @name centroidCoreCover
#' @title centroidCoreCover
#' @description Calculates the centroid of the core cover for specified game.
#' @aliases centroidCoreCover
#' @export centroidCoreCover
#' @template author/JS
#' @template cites/TIJS_LIPPERTS_1982
#' @templateVar TIJS_LIPPERTS_1982_P pp. 27--37
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21 ff.
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 45 ff.
#' @inheritParams CoopGameBaseParams
#' @return It calculates the centroid of the core cover for a game specified by a game vector A.
#' @examples
#' #Example for centroid of the core cover
#' A=c(0,0,0,2,2,3,5)
#' centroidCoreCover(A) 
#' #[1] 1 2 2
#' 
centroidCoreCover<-function(A){
  tv=CentroidCoreCoverConcept(A)
  return(calculatePointSolution(tv))
}

logicCentroidCoreCover<-function(A){
  retVal=NULL
  n=getNumberOfPlayers(A)
  N=length(A)
  
  if(!isQuasiBalancedGame(A)){
    print("Game is not quasi balanced therefore no centroid of the core cover can be retrieved")
  }else{
    setVertices=coreCoverVertices(A)
    centroid=colSums(setVertices)/nrow(setVertices)
    retVal = centroid
  }
  return(retVal)
}


#' @title CentroidCoreCoverConcept - S4 class for centroid core cover concept
#' @name CentroidCoreCoverConceptClass
#' @noRd
#' @description S4 class containing logic for solving centroid core cover concept
#' @include PointSolutionConcept.R
#' #@exportClass CentroidCoreCoverConcept

setClass(
  "CentroidCoreCoverConcept",
  contains = "PointSolutionConcept"
)

#' @title Constructor for CentroidCoreCoverConcept
#' @noRd
#' @template author/JS
#' @name CentroidCoreCoverConcept
#' @inheritParams CoopGameBaseParams
#'# @export
CentroidCoreCoverConcept<-function(A){
  retCentroidCoreCoverConcept=methods::new("CentroidCoreCoverConcept",A)
  return(retCentroidCoreCoverConcept)
}
#' @rdname calculatePointSolution-methods
#' @aliases calculatePointSolution,CentroidCoreCoverConcept-method
setMethod(
  "calculatePointSolution",
  signature="CentroidCoreCoverConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicCentroidCoreCover(A))
  }
)

#' @name drawCentroidCoreCover
#' @title drawCentroidCoreCover for 3 or 4 players
#' @family CentroidCoreCoverConcept
#' @family PointSolutionConcept
#' @description drawCentroidCoreCover draws the centroid of the core cover for 3 or 4 players.
#' @aliases drawCentroidCoreCover
#' @export drawCentroidCoreCover
#' @template author/JS
#' @template cites/TIJS_LIPPERTS_1982
#' @templateVar TIJS_LIPPERTS_1982_P pp. 27--37
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21 ff.
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 45 ff.
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' A=c(0,0,5,4,8,9,12)
#' drawCentroidCoreCover(A,colour="green")
drawCentroidCoreCover<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "centroid of core cover"){
  A=GameVector(A)
  pcn=centroidCoreCover(A);
  visualize(A, pointsToDraw=pcn, holdOn=holdOn, colour = colour , label=label, name = name)
}

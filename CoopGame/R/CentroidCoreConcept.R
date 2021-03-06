#' @name centroidCore
#' @title centroidCore
#' @description Calculates the centroid of the core for specified game.
#' @aliases centroidCore
#' @export centroidCore
#' @template author/JS
#' @template cites/GILLIES_1953
#' @template cites/AUMANN_1961
#' @templateVar AUMANN_1961_P pp. 539--552
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 27
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 686 ff.
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 257 ff.
#' @inheritParams CoopGameBaseParams
#' @return Calculates the centroid of the core for a game specified by a game vector A.
#' @examples
#' #Example for centroid of the core
#' A=c(0,0,0,2,2,3,5)
#' centroidCore(A) 
#' #[1] 1 2 2
#' 
centroidCore<-function(A){
  tv=CentroidCoreConcept(A)
  return(calculatePointSolution(tv))
}

logicCentroidCore<-function(A){
  retVal=NULL
  n=getNumberOfPlayers(A)
  N=length(A)
  retVal=NULL
  if(!isBalancedGame(A)){
    print("Game is not balanced therefore no centroid of the core can be retrieved")
  }else{
    setVertices=coreVertices(A)
    centroid=colSums(setVertices)/nrow(setVertices)
    retVal = centroid
  }
  return(retVal)
}


#' @title CentroidCoreConcept - S4 class for centroid core concept
#' @name CentroidCoreConceptClass
#' @noRd
#' @description S4 class containing logic for solving centroid core concept
#' @include PointSolutionConcept.R
#' #@exportClass CentroidCoreConcept

setClass(
  "CentroidCoreConcept",
  contains = "PointSolutionConcept"
)

#' @title Constructor for CentroidCoreConcept
#' @noRd
#' @template author/JS
#' @name CentroidCoreConcept
#' @inheritParams CoopGameBaseParams
#'# @export
CentroidCoreConcept<-function(A){
  retCentroidCoreConcept=methods::new("CentroidCoreConcept",A)
  return(retCentroidCoreConcept)
}
#' @rdname calculatePointSolution-methods
#' @aliases calculatePointSolution,centroidCoreConcept-method
setMethod(
  "calculatePointSolution",
  signature="CentroidCoreConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicCentroidCore(A))
  }
)

#' @name drawCentroidCore
#' @title drawCentroidCore for 3 or 4 players
#' @family CentroidCoreConcept
#' @family PointSolutionConcept
#' @description drawCentroidCore draws the centroid of the core for 3 or 4 players.
#' @aliases drawCentroidCore
#' @export drawCentroidCore
#' @template author/JS
#' @template cites/GILLIES_1953
#' @template cites/AUMANN_1961
#' @templateVar AUMANN_1961_P pp. 539--552
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 27
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 686 ff.
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 257 ff.
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' A=c(0,0,5,4,8,9,12)
#' drawCentroidCore(A,colour="green")
drawCentroidCore<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "centroid of core"){
  A=GameVector(A)
  pcn=centroidCore(A);
  visualize(A, pointsToDraw=pcn, holdOn=holdOn, colour = colour , label=label, name = name)
}

#' @name centroidImputationSet
#' @title centroidImputationSet
#' @description Calculates the centroid of the imputation set for specified game.
#' @aliases centroidImputationSet
#' @export centroidImputationSet
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 20
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 674 ff.
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 278
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P p. 407
#' @inheritParams CoopGameBaseParams
#' @return It calculates the centroid of the imputation set for a game specified by a game vector A.
#' @examples
#' #Example for centroid of the imputation set
#' A=c(0,0,0,2,2,3,5)
#' centroidImputationSet(A) 
#' #[1] 1.666667 1.666667 1.666667
#' 
centroidImputationSet<-function(A){
  tv=CentroidImputationSetConcept(A)
  return(calculatePointSolution(tv))
}

logicCentroidImputationSet<-function(A){
  retVal=NULL
  n=getNumberOfPlayers(A)
  N=length(A)
  
  if(!isEssentialGame(A)){
    print("Game is not essential therefore no centroid of the imputation set can be retrieved")
  }else{
    setVertices=imputationsetVertices(A)
    centroid=colSums(setVertices)/n
    retVal = centroid
  }
  return(retVal)
}


#' @title CentroidImputationSetConcept - S4 class for centroid imputation set concept
#' @name CentroidImputationSetConceptClass
#' @noRd
#' @description S4 class containing logic for solving centroid imputation set concept
#' @include PointSolutionConcept.R
#' #@exportClass CentroidImputationSetConcept

setClass(
  "CentroidImputationSetConcept",
  contains = "PointSolutionConcept"
)

#' @title Constructor for CentroidImputationSetConcept
#' @noRd
#' @template author/JS
#' @name CentroidImputationSetConcept
#' @inheritParams CoopGameBaseParams
#'# @export
CentroidImputationSetConcept<-function(A){
  retCentroidImputationSetConcept=methods::new("CentroidImputationSetConcept",A)
  return(retCentroidImputationSetConcept)
}
#' @rdname calculatePointSolution-methods
#' @aliases calculatePointSolution,CentroidImputationSetConcept-method
setMethod(
  "calculatePointSolution",
  signature="CentroidImputationSetConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicCentroidImputationSet(A))
  }
)

#' @name drawCentroidImputationSet
#' @title drawCentroidImputationSet for 3 or 4 players
#' @family CentroidImputationSetConcept
#' @family PointSolutionConcept
#' @description drawCentroidImputationSet draws the centroid of the imputation set for 3 or 4 players.
#' @aliases drawCentroidImputationSet
#' @export drawCentroidImputationSet
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 20
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 674 ff.
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 278
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P p. 407
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' A=c(0,0,5,4,8,9,12)
#' drawCentroidImputationSet(A,colour="green")
drawCentroidImputationSet<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "centroid of imputation set"){
  A=GameVector(A)
  pcn=centroidImputationSet(A);
  visualize(A, pointsToDraw=pcn, holdOn=holdOn, colour = colour , label=label, name = name)
}

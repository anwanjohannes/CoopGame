#' @name publicHelpIndex
#' @title publicHelpIndex
#' @description Calculates the Public Help Index for a specified simple TU game.
#' @aliases publicHelpIndex publicHelpThetaIndex publicHelpIndexTheta
#' @export publicHelpIndex
#' @template author/JA
#' @template author/JS
#' @template cites/BERTINI_ET_AL_2008
#' @templateVar BERTINI_ET_AL_2008_P pp. 83 -- 98
#' @inheritParams CoopGameBaseParams
#' @return Public Help Index for specified simple game 
#' @examples 
#' A=c(0,0,0,1,1,0,1)
#' publicHelpIndex(A) #result: 0.4285714 0.2857143 0.2857143
publicHelpIndex<-function(A){
  phi=PublicHelpIndexConcept(A)
  return(calculatePowerIndex(phi))
}



logicPublicHelpIndex=function(A){
  retVal = NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Public Help Index can be retrieved.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    #the winning coalitions
    wcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    tempVar=sapply(c(1:n),function(i){sum(wcs[wcs[,i]==1,"cVal"])})
    sumPlayerValues=sum(tempVar)
    phi=sapply(c(1:n),function(i){tempVar[i]/sumPlayerValues})
    retVal = phi
  }
  return(retVal)
}

#' @title PublicHelpIndexConcept
#' @noRd
#' @description PublicHelpIndexConcept
#' @include PointSolutionConcept.R
# @exportClass PublicHelpIndexConcept

setClass(
  "PublicHelpIndexConcept",
  contains = "PointSolutionConcept"
)

#' @rdname calculatePowerIndex-methods
#' @aliases calculatePowerIndex,PublicHelpIndexConcept-method
setMethod(
  "calculatePowerIndex",
  signature="PublicHelpIndexConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicPublicHelpIndex(A))
  }
)

#' @title Constructor for PublicHelpIndexConcept
#' @noRd
#' @template author/JA
#' @template author/JS
#' @name PublicHelpIndexConcept
#' #@export
PublicHelpIndexConcept<-function(A){
  retPublicHelpIndex=methods::new("PublicHelpIndexConcept",A)
  return(retPublicHelpIndex)
}



#' @name drawPublicHelpIndex
#' @title drawPublicHelpIndex for 3 or 4 players
#' @description drawPublicHelpIndex draws the Public Help Index for 3 or 4 players.
#' @aliases drawPublicHelpIndex
#' @export drawPublicHelpIndex
#' @template author/JA
#' @template author/JS
#' @template cites/BERTINI_ET_AL_2008
#' @templateVar BERTINI_ET_AL_2008_P pp. 83 -- 98
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @return Draws Public Help index for given game vector with 3 or 4 players
#' @examples
#' A=c(0,0,0,1,1,0,1)
#' drawPublicHelpIndex(A) 
drawPublicHelpIndex<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Public Help Index"){
  A=GameVector(A)
  sm=publicHelpIndex(A);
  visualize(A, pointsToDraw=sm, holdOn=holdOn, colour = colour , label=label, name = name)
}

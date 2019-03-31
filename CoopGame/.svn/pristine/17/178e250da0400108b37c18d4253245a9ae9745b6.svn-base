#' @name publicGoodIndex
#' @title publicGoodIndex
#' @description Calculates the Public Good Index (aka Holler Index) for a specified simple game.
#' @aliases publicGoodIndex hollerIndex
#' @export publicGoodIndex
#' @template author/JA
#' @template author/JS
#' @template cites/HOLLER_ET_PACKEL_1983
#' @templateVar HOLLER_ET_PACKEL_1983_P pp. 21 -- 29
#' @template cites/HOLLER_1982
#' @templateVar HOLLER_1982_P pp. 262 -- 271
#' @inheritParams CoopGameBaseParams
#' @return The return value is a vector which contains for each player the calculated public good index
#' @examples
#' 
#' A=c(0,0,0,1,1,0,1)
#' publicGoodIndex(A) #result: 0.50 0.25 0.25 
publicGoodIndex<-function(A){
  pgi=PublicGoodIndexConcept(A)
  return(calculatePowerIndex(pgi))
}

logicPublicGoodIndex=function(A){
  retVal = NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Public Good Index can be retrieved.")
  }
  else 
  {
    n=getNumberOfPlayers(A)
    mwcs=getMinimumWinningCoalitions(A)
   
    
    tempVar=sapply(c(1:n),function(i){sum(mwcs[mwcs[,i]==1,"cVal"])})
    sumPlayerValues=sum(tempVar)
    pgi=sapply(c(1:n),function(i){tempVar[i]/sumPlayerValues})
    retVal = pgi
  }
  return(retVal)
}


#' @title PublicGoodIndexConcept - S4 class for Public Good concept
#' @noRd
#' @description PublicGoodIndexConcept
#' @include PointSolutionConcept.R
# @exportClass PublicGoodIndexConcept

setClass(
  "PublicGoodIndexConcept",
  contains = "PointSolutionConcept"
)

#' @rdname calculatePowerIndex-methods
#' @aliases calculatePowerIndex,PublicGoodIndexConcept-method
setMethod(
  "calculatePowerIndex",
  signature="PublicGoodIndexConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicPublicGoodIndex(A))
  }
)

#' @title Constructor for PublicGoodIndexConcept
#' @noRd
#' @template author/JA
#' @name PublicGoodIndexConcept
#' #@export
PublicGoodIndexConcept<-function(A){
  retPublicGoodIndex=methods::new("PublicGoodIndexConcept",A)
  return(retPublicGoodIndex)
}


#' @name drawPublicGoodIndex
#' @title drawPublicGoodIndex for 3 or 4 players
#' @description drawPublicGoodIndex draws the Public Good Value for 3 or 4 players.
#' @aliases drawPublicGoodIndex
#' @export drawPublicGoodIndex
#' @template author/JA
#' @template author/JS
#' @template cites/HOLLER_ET_PACKEL_1983
#' @templateVar HOLLER_ET_PACKEL_1983_P pp. 21 -- 29
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' A=c(0,0,0,1,1,0,1)
#' drawPublicGoodIndex(A)
drawPublicGoodIndex<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Public Good Index"){
  A=GameVector(A)
  pgv=publicGoodIndex(A);
  visualize(A, pointsToDraw=pgv, holdOn=holdOn, colour = colour , label=label, name = name)
}

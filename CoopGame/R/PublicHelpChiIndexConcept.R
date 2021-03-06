#' @name publicHelpChiIndex
#' @title publicHelpChiIndex
#' @description Calculates the Public Help Chi Index for a specified simple TU game.
#' @aliases publicHelpChiIndex publicHelpIndexChi
#' @export publicHelpChiIndex
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9 -- 25
#' @template cites/STACH_2016
#' @templateVar STACH_2016_P pp. 99 -- 110
#' @inheritParams CoopGameBaseParams
#' @return Public Help Chi Index for specified simple game 
#' @examples 
#' A=c(0,0,0,1,1,0,1)
#' publicHelpChiIndex(A) #result: 0.4583333 0.2708333 0.2708333
publicHelpChiIndex<-function(A){
  phi=PublicHelpChiIndexConcept(A)
  return(calculatePowerIndex(phi))
}



logicPublicHelpChiIndex=function(A){
  retVal = NULL
  n=getNumberOfPlayers(A)
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Public Help Chi Index can be retrieved.")
  }
  else
  {
    bm=createBitMatrix(n,A)
    #the winning coalitions
    wcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    # Loop over all winning coalitions
    factor1 <- 0
    tempVariable <- numeric(n)
    for (i in 1:nrow(wcs))
    {
      playersInCoalition = getPlayersFromBMRow(bmRow=wcs[i,])
      noPlayersInCoalition = length(playersInCoalition)
      factor1 = factor1 + 1/noPlayersInCoalition
      for (j in 1:noPlayersInCoalition)
      {
        tempVariable[playersInCoalition[j]] = tempVariable[playersInCoalition[j]] + 1/(noPlayersInCoalition^2)
      }
    }
    phiChi=(1/factor1)*tempVariable
    retVal = phiChi
  }
  return(retVal)
}

#' @title PublicHelpChiIndexConcept
#' @noRd
#' @description PublicHelpChiIndexConcept
#' @include PointSolutionConcept.R
# @exportClass PublicHelpChiIndexConcept

setClass(
  "PublicHelpChiIndexConcept",
  contains = "PointSolutionConcept"
)

#' @rdname calculatePowerIndex-methods
#' @aliases calculatePowerIndex,PublicHelpChiIndexConcept-method
setMethod(
  "calculatePowerIndex",
  signature="PublicHelpChiIndexConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicPublicHelpChiIndex(A))
  }
)

#' @title Constructor for PublicHelpChiIndexConcept
#' @noRd
#' @template author/JS
#' @name PublicHelpChiIndexConcept
#' #@export
PublicHelpChiIndexConcept<-function(A){
  retPublicHelpChiIndex=methods::new("PublicHelpChiIndexConcept",A)
  return(retPublicHelpChiIndex)
}



#' @name drawPublicHelpChiIndex
#' @title drawPublicHelpChiIndex for 3 or 4 players
#' @description drawPublicHelpChiIndex draws the public help chi index for 3 or 4 players.
#' @aliases drawPublicHelpChiIndex
#' @export drawPublicHelpChiIndex
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9 -- 25
#' @template cites/STACH_2016
#' @templateVar STACH_2016_P pp. 99 -- 110
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @return Public Help Chi Index for given game vector with 3 or 4 players
#' @examples
#' A=c(0,0,0,1,1,0,1)
#' drawPublicHelpChiIndex(A) 
drawPublicHelpChiIndex<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Public Help Chi Index"){
  A=GameVector(A)
  sm=publicHelpChiIndex(A);
  visualize(A, pointsToDraw=sm, holdOn=holdOn, colour = colour , label=label, name = name)
}

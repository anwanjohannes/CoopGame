#' @name publicHelpChiValue
#' @title publicHelpChiValue
#' @description Calculates the (normalized) Public Help Chi Value for a nonnegative TU game.
#' @aliases publicHelpChiValue publicHelpChiValue publicHelpChiValueChi
#' @export publicHelpChiValue
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9 -- 25
#' @inheritParams CoopGameBaseParams
#' @return Public Help Value for specified nonnegative TU game 
#' @examples 
#' A=c(0,0,0,2,2,0,2)
#' publicHelpChiValue(A) 
#' #[1] 0.9166667 0.5416667 0.5416667
publicHelpChiValue<-function(A)
{
  retVal = NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Therefore we do not compute any (normalized) public help chi value.")
  }
  else
  {
    phvchi=PublicHelpChiValueConcept(A)
    n = as.numeric(getNumberOfPlayers(A))
    N = length(A)
    phvchics=calculatePointSolution(phvchi)
    phvres = numeric(n)
    phvres[1:n]=(phvchics[1:n]/phvchics[n+1])*A[N]
    retVal = phvres
  }
  return(retVal)
}

#' @name absolutePublicHelpChiValue
#' @title absolutePublicHelpChiValue
#' @description Calculates the absolute Public Help Chi Value for a specified nonnegative TU game.
#' Note that in general the absolute Public Help Chi Value is not an 
#' efficient vector, i.e. the sum of its entries is not always 1. Hence no 
#' drawing routine for the absolute Public Help Chi Value is provided.
#' @aliases absolutePublicHelpChiValue absolutePublicHelpValueChi
#' @export absolutePublicHelpChiValue
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9 -- 25
#' @inheritParams CoopGameBaseParams
#' @return Absolute Public Help Value for specified simple game 
#' @examples 
#' A=c(0,0,0,2,2,0,2)
#' absolutePublicHelpChiValue(A) 
#' #[1] 1.2222222 0.7222222 0.7222222
#' 
absolutePublicHelpChiValue<-function(A)
{
  retVal = NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Therefore we do not compute any public help chi value.")
  }
  else
  {
    phv=PublicHelpChiValueConcept(A)
    phvcs=calculatePointSolution(phv)
    n = as.numeric(getNumberOfPlayers(A))
    retVal = phvcs[1:n]
  }
  return(retVal)
}



logicPublicHelpChiValue=function(A){
  retVal = NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Therefore we do not compute any Public Help Value.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    retVal = numeric(n+1)
    #the gaining coalitions
    gcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    tempVar1 <- numeric(n)
    tempVar2 <- 0
    sum <- 0
    for (i in 1:nrow(gcs))
    {
      playersInCoalition = getPlayersFromBMRow(bmRow=gcs[i,])
      noPlayersInCoalition = length(playersInCoalition)
      value = gcs[[i,"cVal"]]
      for (j in 1:noPlayersInCoalition)
      {
        tempVar1[playersInCoalition[j]] = tempVar1[playersInCoalition[j]] + value/(noPlayersInCoalition^2)
      }
      tempVar2 = tempVar2 + value/noPlayersInCoalition
    }
    retVal[1:n]=tempVar1
    retVal[n+1]=tempVar2
  }
  return(retVal)
}

#' @title PublicHelpChiValueConcept
#' @noRd
#' @description PublicHelpChiValueConcept
#' @include PointSolutionConcept.R
# @exportClass PublicHelpChiValueConcept

setClass(
  "PublicHelpChiValueConcept",
  contains = "PointSolutionConcept"
)

#' @rdname calculatePointSolution-methods
#' @aliases calculatePointSolution,PublicHelpChiValueConcept-method
setMethod(
  "calculatePointSolution",
  signature="PublicHelpChiValueConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicPublicHelpChiValue(A))
  }
)

#' @title Constructor for PublicHelpChiValueConcept
#' @noRd
#' @template author/JS
#' @name PublicHelpChiValueConcept
#' #@export
PublicHelpChiValueConcept<-function(A){
  retPublicHelpChiValue=methods::new("PublicHelpChiValueConcept",A)
  return(retPublicHelpChiValue)
}



#' @name drawPublicHelpChiValue
#' @title drawPublicHelpChiValue for 3 or 4 players
#' @description drawPublicHelpChiValue draws the (normalized) Public Help Value for 3 or 4 players.
#' @aliases drawPublicHelpChiValue
#' @export drawPublicHelpChiValue
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9 -- 25
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @return Draws (normalized) Public Help Chi value for given game vector with 3 or 4 players
#' @examples
#' A=c(0,0,0,1,1,0,1)
#' drawPublicHelpChiValue(A) 
drawPublicHelpChiValue<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Normalized Public Help Chi Value"){
  A=GameVector(A)
  sm=publicHelpChiValue(A);
  visualize(A, pointsToDraw=sm, holdOn=holdOn, colour = colour , label=label, name = name)
}

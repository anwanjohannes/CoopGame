#' @name publicHelpValue
#' @title publicHelpValue
#' @description Calculates the (normalized) Public Help Value for a specified nonnegative TU game.
#' @aliases publicHelpValue publicHelpThetaValue publicHelpValueTheta
#' @export publicHelpValue
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9 -- 25
#' @inheritParams CoopGameBaseParams
#' @return Public Help Value for specified nonnegative TU game 
#' @examples 
#' A=c(0,0,0,0.7,11,0,15)
#' publicHelpValue(A) 
#' #[1] 0.3903509 0.2295322 0.3801170
#' 
publicHelpValue<-function(A)
{
  retVal=NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Therefore we do not compute any (normalized) public help value.")
  }
  else
  {
    phv=PublicHelpValueConcept(A)
    phvcs=calculatePointSolution(phv)
    phvcs=phvcs/sum(phvcs)
    retVal = phvcs
  }
  return(retVal)
}

#' @name absolutePublicHelpValue
#' @title absolutePublicHelpValue
#' @description Calculates the absolute Public Help Value for a specified nonnegative TU game.
#' Note that in general the absolute Public Help Value is not an efficient vector, 
#' i.e. the sum of its entries is not always 1.
#' Hence no drawing routine for the absolute Public Help Value is provided.
#' @aliases absolutePublicHelpValue absolutePublicHelpThetaValue absolutePublicHelpValueTheta
#' @export absolutePublicHelpValue
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9 -- 25
#' @inheritParams CoopGameBaseParams
#' @return Absolute Public Help Value for specified simple game 
#' @examples 
#' A=c(0,0,0,0.7,11,0,15)
#' absolutePublicHelpValue(A) 
#' #[1] 26.7 15.7 26.0
#' 
absolutePublicHelpValue<-function(A)
{
  retVal = NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Therefore we do not compute any public help value.")
  }
  else
  {
    phv=PublicHelpValueConcept(A)
    phvcs=calculatePointSolution(phv)
    retVal=phvcs
  }
  return(retVal)
}



logicPublicHelpValue=function(A){
  retVal=NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Therefore we do not compute any Public Help Value.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    #the gaining coalitions
    gcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    phv=sapply(c(1:n),function(i){sum(gcs[gcs[,i]==1,"cVal"])})
    retVal = phv
  }
  return(retVal)
}

#' @title PublicHelpValueConcept
#' @noRd
#' @description PublicHelpValueConcept
#' @include PointSolutionConcept.R
# @exportClass PublicHelpValueConcept

setClass(
  "PublicHelpValueConcept",
  contains = "PointSolutionConcept"
)

#' @rdname calculatePointSolution-methods
#' @aliases calculatePointSolution,PublicHelpValueConcept-method
setMethod(
  "calculatePointSolution",
  signature="PublicHelpValueConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicPublicHelpValue(A))
  }
)

#' @title Constructor for PublicHelpValueConcept
#' @noRd
#' @template author/JS
#' @name PublicHelpValueConcept
#' #@export
PublicHelpValueConcept<-function(A){
  retPublicHelpValue=methods::new("PublicHelpValueConcept",A)
  return(retPublicHelpValue)
}



#' @name drawPublicHelpValue
#' @title drawPublicHelpValue for 3 or 4 players
#' @description drawPublicHelpValue draws the (normalized) Public Help Value for 3 or 4 players.
#' @aliases drawPublicHelpValue
#' @export drawPublicHelpValue
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9 -- 25
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @return Draws (normalized) Public Help value for given game vector with 3 or 4 players
#' @examples
#' A=c(0,0,0,1,1,0,1)
#' drawPublicHelpValue(A) 
drawPublicHelpValue<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Normalized Public Help Value"){
  A=GameVector(A)
  sm=publicHelpValue(A);
  visualize(A, pointsToDraw=sm, holdOn=holdOn, colour = colour , label=label, name = name)
}

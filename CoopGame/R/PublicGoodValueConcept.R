#' @name publicGoodValue
#' @title publicGoodValue
#' @description Calculates the (normalized) Public Good Value for a specified nonnegative TU game.
#' @aliases publicGoodValue hollerValue
#' @export publicGoodValue
#' @template author/JS
#' @template cites/HOLLER_ET_LI_1995
#' @templateVar HOLLER_ET_LI_1995_P pp. 257 -- 270
#' @template cites/HOLLER_ET_PACKEL_1983
#' @templateVar HOLLER_ET_PACKEL_1983_P pp. 21 -- 29
#' @template cites/HOLLER_1982
#' @templateVar HOLLER_1982_P pp. 262 -- 271
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9 -- 25
#' @inheritParams CoopGameBaseParams
#' @return Public Good Value for specified nonnegative TU game 
#' @examples 
#' # Example from original 1995 paper by Holler and Li
#' A <- c(1,2,3,4,0,0,0)
#' publicGoodValue(A)
#' #[1] 0.3571429 0.4285714 0.2142857
#' 
#' A=c(0,0,0,0.7,11,0,15)
#' publicGoodValue(A) 
#' #[1] 0.3903509 0.2295322 0.3801170
#' 
publicGoodValue<-function(A)
{
  retVal=NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Therefore we do not compute any (normalized) public Good value.")
  }
  else
  {
    pgv=PublicGoodValueConcept(A)
    pgvcs=calculatePointSolution(pgv)
    pgvcs=pgvcs/sum(pgvcs)
    retVal = pgvcs
  }
  return(retVal)
}

#' @name absolutePublicGoodValue
#' @title absolutePublicGoodValue
#' @description Calculates the absolute Public Good Value for a specified nonnegative TU game.
#' @aliases absolutePublicGoodValue absoluteHollerValue
#' @export absolutePublicGoodValue 
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9 -- 25
#' @template cites/HOLLER_ET_LI_1995
#' @templateVar HOLLER_ET_LI_1995_P pp. 257 -- 270
#' @inheritParams CoopGameBaseParams
#' @return Absolute Public Good Value for specified simple game 
#' @examples 
#' #Example from original 1995 paper by Holler and Li
#' A <- c(1,2,3,4,0,0,0)
#' absolutePublicGoodValue(A)
#' #[1] 5 6 3
#' 
#' A=c(0,0,0,0.7,11,0,15)
#' absolutePublicGoodValue(A) 
#' #[1] 26.7 15.7 26.0
#' # Note that in general the absolute Public Good Value is not an efficient vector, 
#' # i.e. the sum of its entries is not always 1.
#' 
absolutePublicGoodValue<-function(A)
{
  retVal = NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. We do not compute any Public Good value in this case.")
  }
  else
  {
    pgv=PublicGoodValueConcept(A)
    pgvcs=calculatePointSolution(pgv)
    retVal=pgvcs
  }
  return(retVal)
}



logicPublicGoodValue=function(A){
  retVal=NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Therefore we do not compute any Public Good Value.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    #the gaining coalitions
    rgcs= getRealGainingCoalitions(A)
    pgv=sapply(c(1:n),function(i){sum(rgcs[rgcs[,i]>0,"cVal"])})
    retVal = pgv
  }
  return(retVal)
}

#' @title PublicGoodValueConcept
#' @noRd
#' @description PublicGoodValueConcept
#' @include PointSolutionConcept.R
# @exportClass PublicGoodValueConcept

setClass(
  "PublicGoodValueConcept",
  contains = "PointSolutionConcept"
)

#' @rdname calculatePointSolution-methods
#' @aliases calculatePointSolution,PublicGoodValueConcept-method
setMethod(
  "calculatePointSolution",
  signature="PublicGoodValueConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicPublicGoodValue(A))
  }
)

#' @title Constructor for PublicGoodValueConcept
#' @noRd
#' @template author/JS
#' @name PublicGoodValueConcept
#' #@export
PublicGoodValueConcept<-function(A){
  retPublicGoodValue=methods::new("PublicGoodValueConcept",A)
  return(retPublicGoodValue)
}



#' @name drawPublicGoodValue
#' @title drawPublicGoodValue for 3 or 4 players
#' @description drawPublicGoodValue draws the (normalized) Public Good Value for 3 or 4 players.
#' @aliases drawPublicGoodValue
#' @export drawPublicGoodValue
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9 -- 25
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @return Draws (normalized) Public Good value for given game vector with 3 or 4 players
#' @examples
#' A=c(0,0,0,1,1,0,1)
#' drawPublicGoodValue(A) 
drawPublicGoodValue<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Normalized Public Good Value"){
  A=GameVector(A)
  sm=publicGoodValue(A);
  visualize(A, pointsToDraw=sm, holdOn=holdOn, colour = colour , label=label, name = name)
}

#' @name baruaChakravartySarkarIndex
#' @title Barua Chakravarty Sarkar Index
#' @description Calculates the Barua Chakravarty Sarkar Index for a specified simple TU game.
#' Note that in general the Barua Chakravarty Sarkar Index is not an efficient vector, 
#' i.e. the sum of its entries is not always 1.
#' Hence no drawing routine for the Barua Chakravarty Sarkar Index is provided.
#' @aliases baruaChakravartySarkarIndex
#' @export baruaChakravartySarkarIndex
#' @template author/JS
#' @template cites/BARUA_CHAKRAVARTY_ET_SARKAR_2012
#' @templateVar BARUA_CHAKRAVARTY_ET_SARKAR_2012_P pp. 81--91
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 120 -- 123
#' @inheritParams CoopGameBaseParams
#' @return Barua Chakravarty Sarkar Index for specified simple game 
#' @examples 
#' A=c(0,0,0,1,1,0,1)
#' baruaChakravartySarkarIndex(A) 
#' #result: [1] 1.0 0.5 0.5
#' 
baruaChakravartySarkarIndex<-function(A){
  baruaChakravartySarkar=BaruaChakravartySarkarConcept(A)
  return(calculatePointSolution(baruaChakravartySarkar))
}



logicBaruaChakravartySarkarIndex=function(A){
  retVal=NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Barua Chakravarty Sarkar Index can be retrieved.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    numerator = numeric(n)
    numerator = rawBanzhafIndex(A)
    #the winning coalitions
    wcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    denominator = numeric(n)
    denominator = sapply(c(1:n),function(i){sum(wcs[wcs[,i]==1,"cVal"])})
    baruaChakravartySarkar =sapply(c(1:n),function(i){numerator[i]/denominator[i]})
    retVal=baruaChakravartySarkar
  }
  return(retVal)
}

#' @title BaruaChakravartySarkarConcept
#' @noRd
#' @description BaruaChakravartySarkarConcept
#' @include PointSolutionConcept.R
# @exportClass BaruaChakravartySarkarConcept

setClass(
  "BaruaChakravartySarkarConcept",
  contains = "PointSolutionConcept"
)

#' @rdname calculatePowerIndex-methods
#' @aliases calculatePowerIndex,BaruaChakravartySarkarConcept-method
setMethod(
  "calculatePointSolution",
  signature="BaruaChakravartySarkarConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicBaruaChakravartySarkarIndex(A))
  }
)

#' @title Constructor for BaruaChakravartySarkarConcept
#' @noRd
#' @template author/JS
#' @name BaruaChakravartySarkarConcept
#' #@export
BaruaChakravartySarkarConcept<-function(A){
  retbaruaChakravartySarkarIndex=methods::new("BaruaChakravartySarkarConcept",A)
  return(retbaruaChakravartySarkarIndex)
}





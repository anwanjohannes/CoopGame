#' @name koenigBraeuningerIndex
#' @title koenigBraeuningerIndex
#' @description Calculates the Koenig-Braeuninger Index for a specified simple TU game.
#' Note that in general the Koenig-Braeuninger Index is not an efficient vector, 
#' i.e. the sum of its entries is not always 1. 
#' Hence no drawing routine for the Koenig-Braeuninger Index is provided.
#' @aliases koenigBraeuningerIndex
#' @export koenigBraeuningerIndex
#' @template author/JS
#' @template cites/KOENIG_ET_BRAEUNINGER_1998
#' @templateVar KOENIG_ET_BRAEUNINGER_1998_P pp. 125 -- 142
#' @template cites/NEVISON_ET_AL_1978
#' @templateVar NEVISON_ET_AL_1978_P pp. 130 -- 131
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9 -- 25
#' @inheritParams CoopGameBaseParams
#' @return Koenig-Braeuninger Index for specified simple game 
#' @examples 
#' A=c(0,0,0,1,1,0,1)
#' koenigBraeuningerIndex(A) 
#' #result: [1] 1.0000000 0.6666667 0.6666667
#' 
koenigBraeuningerIndex<-function(A){
  koenigBraeuninger=KoenigBraeuningerConcept(A)
  return(calculatePointSolution(koenigBraeuninger))
}



logickoenigBraeuningerIndex=function(A){
  retVal = NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Koenig Braeuninger Index can be retrieved")
    return(retVal)
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    #the winning coalitions
    wcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    temp=sapply(c(1:n),function(i){sum(wcs[wcs[,i]==1,"cVal"])})
    koenigBraeuninger =sapply(c(1:n),function(i){temp[i]/nrow(wcs)})
    retVal = koenigBraeuninger
    return(retVal)
  }
}

#' @title KoenigBraeuningerConcept
#' @noRd
#' @description KoenigBraeuningerConcept
#' @include PointSolutionConcept.R
# @exportClass KoenigBraeuningerConcept

setClass(
  "KoenigBraeuningerConcept",
  contains = "PointSolutionConcept"
)

#' @rdname calculatePowerIndex-methods
#' @aliases calculatePowerIndex,KoenigBraeuningerConcept-method
setMethod(
  "calculatePointSolution",
  signature="KoenigBraeuningerConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logickoenigBraeuningerIndex(A))
  }
)

#' @title Constructor for KoenigBraeuningerConcept
#' @noRd
#' @template author/JS
#' @name KoenigBraeuningerConcept
#' #@export
KoenigBraeuningerConcept<-function(A){
  retkoenigBraeuningerIndex=methods::new("KoenigBraeuningerConcept",A)
  return(retkoenigBraeuningerIndex)
}





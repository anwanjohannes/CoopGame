#' @name perCapitaNucleolus
#' @title perCapitaNucleolus for n Players
#' @description Calculates the per capita nucleolus for a game specified by game vector A.
#' @aliases perCapitaNucleolus
#' @export perCapitaNucleolus
#' @template author/JA
#' @template cites/YOUNG_1985
#' @templateVar YOUNG_1985_P pp. 65--72
#' @inheritParams CoopGameBaseParams
#' @template param/enableTermOutLP
#' @return value for the per capita nucleolus
#' @examples
#' A<-generateGameVector(cFuncCostSharing,n=3,C=c(15,20,55,35,61,65,78))
#' perCapitaNucleolus(A)
#' expected_x<-c(4/6,7/6,71/6)
#' 
#' #Example out of YOUNG 1985, p. 68
#' A=c(0,0,0,0,9,10,12)
#' perCapitaNucleolus(A) #[1]  0.6666667  1.1666667 10.1666667
perCapitaNucleolus <- function(A, enableTermOutLP = FALSE){
  pcn=PerCapitaNucleolus(A)
  return(calculateNucleolus(pcn,enableTermOutLP=enableTermOutLP))
} 


#' @title Constructor for PerCapitaNucleolus
#' @noRd
#' @template author/JA
#' @name PerCapitaNucleolus
# @export
PerCapitaNucleolus<-function(A){
  retPerCapitaNucleolus=methods::new("PerCapitaNucleolus",A=A)
  return(retPerCapitaNucleolus)
}

#' @title PerCapitaNucleolus
#' @noRd
#' @template author/JA
#' @template author/JS
#' @description An S4 class for the PerCapitaNucleolus
#' @include NucleolusBase.R
#' @name PerCapitaNucleolus
# @exportClass PerCapitaNucleolus

setClass(
  "PerCapitaNucleolus",
  contains="NucleolusBase"
)

#' @rdname determineExcessCoefficients-methods
#' @aliases determineExcessCoefficients,PerCapitaNucleolus-method
setMethod(
  "determineExcessCoefficients",
  signature="PerCapitaNucleolus",
  definition=function(.Object){
    N=length(.Object@A)
    n=log2(N+1)
    coeffMat<-createBitMatrix(n)
    cardS=sapply(1:N,function(ix){sum(coeffMat[ix,1:n])})
    return(c(cardS[-N],0))
  }
)


#' @name drawPerCapitaNucleolus
#' @title drawPerCapitaNucleolus for 3 or 4 players
#' @description drawPerCapitaNucleolus draws the nucleolus for 3 or 4 players.
#' @aliases drawPerCapitaNucleolus
#' @export drawPerCapitaNucleolus
#' @template author/JA
#' @template cites/YOUNG_1985
#' @templateVar YOUNG_1985_P pp. 65--72
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' #Example out of YOUNG 1985, p. 68
#' A=c(0,0,0,0,9,10,12)
#' drawPerCapitaNucleolus(A)
drawPerCapitaNucleolus<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Per Capita Nucleolus"){
  A=GameVector(A)
  pcn=perCapitaNucleolus(A);
  visualize(A, pointsToDraw=pcn, holdOn=holdOn, colour = colour , label=label, name = name)
}
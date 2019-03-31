#' @title simplifiedModiclus for n-player TU games
#' @description Computes the simplifiedModiclus of a TU game with n players.
#' @name simplifiedModiclus
#' @aliases simplifiedModiclus
#' @export simplifiedModiclus
#' @template author/JA
#' @template cites/TARASHNINA_2011
#' @templateVar TARASHNINA_2011_P pp. 150 -- 166.
#' @inheritParams CoopGameBaseParams
#' @template param/enableTermOutLP
#' @return Numeric vector of length n representing the modified nucleolus
#' @examples
#' simplifiedModiclus(c(0, 0, 0, 1, 1, 0, 1))
simplifiedModiclus <- function(A, enableTermOutLP = FALSE){
  mn=SimplifiedModiclus(A=A)
  return(calculateNucleolus(mn,enableTermOutLP=enableTermOutLP))
}


#' @title Constructor for SimplifiedModiclus
#' @noRd
#' @template author/DG
#' @name SimplifiedModiclus
#' #@export
SimplifiedModiclus<-function(A){
  retSimplifiedModiclus=methods::new("SimplifiedModiclus",A=A)
  return(retSimplifiedModiclus)
}

#' @title SimplifiedModiclus - S4 class for Simplified Modiclus Concept
#' @noRd
#' @description Class for the implementation of the simplified modiclus concept
#' @template author/JA
#' @include NucleolusBase.R
#'# @exportClass SimplifiedModiclus
setClass(
  "SimplifiedModiclus",
  contains="NucleolusBase"
)


#' @rdname initLPMatrix-methods
#' @aliases initLPMatrix,SimplifiedModiclus-method
setMethod(
  "initLPMatrix",
  signature="SimplifiedModiclus",
  definition=function(.Object){
    lpCoopGameUtils<-.Object@LPCoopGameUtils
    n=getNumberOfPlayers(.Object@A)
    N=length(.Object@A)
    lpMatrix=createBitMatrix(n)
    lpMatrix[lpMatrix==0]=-1
    lpMatrix[,(n+1)]=1
    lpMatrix[N,(n+1)]=0
    setMatrix(lpCoopGameUtils)<-lpMatrix
    eval.parent(substitute(.Object@LPCoopGameUtils<-lpCoopGameUtils))
  }
)



#' @rdname initLPRows-methods
#' @aliases initLPRows,SimplifiedModiclus-method
setMethod(
  "initLPRows",
  signature="SimplifiedModiclus",
  definition=function(.Object){
    lpCoopGameUtils<-.Object@LPCoopGameUtils
    N<-length(.Object@A)
    n<-getNumberOfPlayers(.Object@A)
    A<-.Object@A
    rlb<-sapply(1:(N-1), FUN=function(ix){A[ix]-A[N-ix]})
    rlb<-c(rlb,A[N])
    setRlb(lpCoopGameUtils)<-rlb
    setRub(lpCoopGameUtils)<-c(rep(Inf, length(rlb) - 1), .Object@A[N])
    setRtype(lpCoopGameUtils)<-c(rep(GLP_LO, length(rlb) - 1), GLP_FX)
    eval.parent(substitute(.Object@LPCoopGameUtils<-lpCoopGameUtils))
  }
)




determineSimplifiedModiclusMatrix<-function(A){

  n=getNumberOfPlayers(A)
  N=length(A)
    
  lpMatrix=createBitMatrix(n)
  lpMatrix[lpMatrix==0]=-1
  lpMatrix[,(n+1)]=1
  lpMatrix[N,(n+1)]=0
  return(lpMatrix)
}


determineSimplifiedModiclusRlb<-function(A){
  rlb<-c()
  N=length(A)
  rlb<-sapply(1:(N-1), FUN=function(ix){A[ix]-A[N-ix]})
  rlb<-c(rlb,A[N])
  return(rlb)
}


#' @name drawSimplifiedModiclus
#' @title drawSimplifiedModiclus for for 3 or 4 players
#' @description drawSimplifiedModiclus draws the modiclus for 3 or 4 players.
#' @aliases drawSimplifiedModiclus
#' @export drawSimplifiedModiclus
#' @template author/JA
#' @template cites/TARASHNINA_2011
#' @templateVar TARASHNINA_2011_P pp. 150 -- 166.
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @return calculated simplified modiclus for given game vector with n players
#' @examples
#' A=c(0, 0, 0, 1, 1, 0, 1)
#' drawSimplifiedModiclus(A)
drawSimplifiedModiclus<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Simplified Modiclus"){
  A=GameVector(A)
  sm=simplifiedModiclus(A);
  visualize(A, pointsToDraw=sm, holdOn=holdOn, colour = colour , label=label, name = name)
}
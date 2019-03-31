#' @name modiclus
#' @title modiclus for n-player TU games
#' @description Calculates the modiclus of a TU game with n players.
#' @aliases modiclus
#' @export modiclus
#' @template author/JA
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 124 ff.
#' @template cites/SUDHOELTER_1997
#' @templateVar SUDHOELTER_1997_P pp. 147 -- 182
#' @inheritParams CoopGameBaseParams
#' @template param/enableTermOutLP
#' @return Numeric vector of length n representing the Modified Nucleolus
#' @examples
#' modiclus(c(1, 1, 1, 2, 3, 4, 5))
#' modiclus(c(0, 0, 0, 0, 5, 5, 8, 9, 10, 8, 13, 15, 16, 17, 21), FALSE)

modiclus <- function(A, enableTermOutLP = FALSE){
  mn=Modiclus(A=A)
  return(calculateNucleolus(mn,enableTermOutLP=enableTermOutLP))
}


#' @title Constructor for Modiclus
#' @noRd
#' @template author/JA
#' @name Modiclus
# @export
Modiclus<-function(A){
  retModiclus=methods::new("Modiclus",A=A)
  return(retModiclus)
}

#' @title Modiclus - S4 class for Modiclus
#' @noRd
#' @description An S4 class for Modiclus concept
#' @template author/JA
#' @include NucleolusBase.R
#' @name Modiclus
# @exportClass Modiclus



setClass(
  "Modiclus",
  contains="NucleolusBase"
)


#' @rdname initLPMatrix-methods
#' @aliases initLPMatrix,Modiclus-method
setMethod(
  "initLPMatrix",
  signature="Modiclus",
  definition=function(.Object){
    lpCoopGameUtils<-.Object@LPCoopGameUtils
    n<-getNumberOfPlayers(.Object@A)
    N<-length(.Object@A)
    
    tempBM<-as.data.frame(createBitMatrix(n))
    lpMatrix<-matrix(ncol=(n+1),nrow=0)
    for(i in 1:(nrow(tempBM)-1)){
      currEntry=tempBM[i,]
      corrEntries=tempBM[c(-i,-N),]
      matTemp=matrix(
        unlist(
          apply(corrEntries,1,FUN=function(x){
            currEntry-x
            })
        ),
        ncol=(n+1),
        byrow = TRUE
      )
      lpMatrix=rbind(lpMatrix,matTemp)
    }
    lpMatrix[,(n+1)]=1
    lpMatrix=rbind(lpMatrix,c(rep(1,n),0))
    colnames(lpMatrix)[(n+1)]<-"cVal"
    
    setMatrix(lpCoopGameUtils)<-lpMatrix
    eval.parent(substitute(.Object@LPCoopGameUtils<-lpCoopGameUtils))
  }
)



#' @rdname initLPRows-methods
#' @aliases initLPRows,Modiclus-method
setMethod(
  "initLPRows",
  signature="Modiclus",
  definition=function(.Object){
    lpCoopGameUtils<-.Object@LPCoopGameUtils
    N<-length(.Object@A)
    n<-getNumberOfPlayers(.Object@A)
    A<-.Object@A
    rlb<-c()
    N=length(A)
    for(i in 1:(N-1)){   
      valuesOfT=A[c(-i,-N)] 
      rlbValues=sapply(valuesOfT,function(x){A[i]-x})
      rlb<-c(rlb,rlbValues)
    }
    rlb<-c(rlb,A[N])
    setRlb(lpCoopGameUtils)<-rlb
    setRub(lpCoopGameUtils)<-c(rep(Inf, length(rlb) - 1), .Object@A[N])
    setRtype(lpCoopGameUtils)<-c(rep(GLP_LO, length(rlb) - 1), GLP_FX)
    eval.parent(substitute(.Object@LPCoopGameUtils<-lpCoopGameUtils))
  }
)




determineModiclusMatrix<-function(A){

  n=getNumberOfPlayers(A)
  N=length(A)
    
  tempBM<-as.data.frame(createBitMatrix(n))
  lpMatrix<-matrix(ncol=(n+1),nrow=0)
  for(i in 1:(nrow(tempBM)-1)){ #original (nrow(tempBM)-1))
    currEntry=tempBM[i,]
    corrEntries=tempBM[c(-i,-N),] #original c(-i,-N)
    matTemp=matrix(unlist(apply(corrEntries,1,FUN=function(x){currEntry-x})),ncol=(n+1),byrow = TRUE)
    lpMatrix=rbind(lpMatrix,matTemp)
  }
  lpMatrix[,(n+1)]=1
  lpMatrix=rbind(lpMatrix,c(rep(1,n),0))
  colnames(lpMatrix)[(n+1)]<-"cVal"
  return(lpMatrix)
}


determineModiclusRlb<-function(A){
  rlb<-c()
  N=length(A)
  for(i in 1:(N-1)){   #originial (N-1)
    valuesOfT=A[c(-i,-N)] #original c(-i,-N)
    rlbValues=sapply(valuesOfT,function(x){A[i]-x})
    rlb<-c(rlb,rlbValues)
  }

  rlb<-c(rlb,A[N])
  return(rlb)
}


#' @name drawModiclus
#' @title drawModiclus for n players
#' @description drawModiclus draws the Modiclus for n players.
#' @aliases drawModiclus
#' @export drawModiclus
#' @template author/JA
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 124 ff.
#' @template cites/SUDHOELTER_1997
#' @templateVar SUDHOELTER_1997_P pp. 147 -- 182
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' A=c(1, 1, 1, 2, 3, 4, 5)
#' drawModiclus(A)
drawModiclus<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Modiclus"){
  A=GameVector(A)
  mod=modiclus(A);
  visualize(A, pointsToDraw=mod, holdOn=holdOn, colour = colour , label=label, name = name)
}

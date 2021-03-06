#' @name tauValue
#' @title tauValue
#' @description Calculates the tau value for specified game.
#' @aliases tauValue, tijsValue
#' @export tauValue
#' @template author/JA
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 31
#' @template cites/TIJS_1981
#' @inheritParams CoopGameBaseParams
#' @return It calculates the tau value for a game specified by a game vector A.
#' @examples
#' #Example for tau value
#' A=c(0,0,0,2,2,3,5)
#' tauValue(A) 
#' #1.250   1.875   1.875
#' 
tauValue<-function(A){
  tv=TauConcept(A)
  return(calculatePointSolution(tv))
}

logicTauValue<-function(A){
  retVal=NULL
  n=getNumberOfPlayers(A)
  N=length(A)
  
  if(!isQuasiBalancedGame(A)){
    print("Game is not quasi balanced therefore no tau value can be retrieved.")
  }else{
    mc=matrix(nrow=2,ncol=n)
    rownames(mc)<-c("M","m")
    mc["M",]<-getUtopiaPayoff(A=A)
    mc["m",]<-getMinimalRights(A=A)
    
    if(identical(mc["M",],mc["m",])){
      retVal=mc["M",]
    }else{
      diffM_m=mc["M",]-mc["m",]
      tDiagMatrix=diag(1,nrow=n,ncol = n)
      coeffMat=cbind(tDiagMatrix,diffM_m)
      coeffMat=rbind(coeffMat,c(rep(1,n),0))
      tauResult=solve(coeffMat,c(mc["M",],A[N]))
      retVal = unname(tauResult[1:n])
    }
  }
  return(retVal)
}


remainder<-function(A){
  n=getNumberOfPlayers(A)
  N=length(A)
  bm=createBitMatrix(n=n,A)
  mc=getUtopiaPayoff(A = A)
  bm[,1:n]=bm[,1:n]*mc
  remainder=matrix(
    unlist(
      apply(bm,1,
            FUN = function(x,N,n){
              return(sapply(1:(n),function(ix){x["cVal"]-sum(x[-c(ix,(n+1))])}))
            },
            N=N,
            n=n
      )
    ),
    ncol=(n),
    byrow=TRUE
  )
  return(remainder)
}




#' @title TauConcept - S4 class for tau concept
#' @name TauConceptClass
#' @noRd
#' @description S4 class containing logic for solving tau concept
#' @include PointSolutionConcept.R
#' #@exportClass TauConcept

setClass(
  "TauConcept",
  contains = "PointSolutionConcept"
)

#' @title Constructor for TauConcept
#' @noRd
#' @template author/JA
#' @name TauConcept
#' @inheritParams CoopGameBaseParams
#'# @export
TauConcept<-function(A){
  retTauConcept=methods::new("TauConcept",A)
  return(retTauConcept)
}
#' @rdname calculatePointSolution-methods
#' @aliases calculatePointSolution,TauConcept-method
setMethod(
  "calculatePointSolution",
  signature="TauConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicTauValue(A))
  }
)

#' @name drawTauValue
#' @title drawTauValue for 3 or 4 players
#' @family TauConcept
#' @family PointSolutionConcept
#' @description drawTauValue draws the Tau Value for 3 or 4 players.
#' @aliases drawTauValue, drawTijsValue
#' @export drawTauValue
#' @template author/JA
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 31
#' @template cites/TIJS_1981
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' A=c(0,0,5,4,8,9,12)
#' drawTauValue(A,colour="green")
drawTauValue<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Tau value"){
  A=GameVector(A)
  pcn=tauValue(A);
  visualize(A, pointsToDraw=pcn, holdOn=holdOn, colour = colour , label=label, name = name)
}

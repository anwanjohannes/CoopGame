#' @name banzhafValue
#' @title banzhafValue
#' @description banzhafValue computes the Banzhaf Value for a specified TU game
#'              The Banzhaf value itself is an alternative to the Shapley value. \cr
#'              Conceptually, the Banzhaf value is very similar to the Shapley value. 
#'              Its main difference from the Shapley value is that the Banzhaf Value is coalition
#'              based rather than permutation based. Hence the factors by which we multiply the marginal contributions. \cr
#'              Shapley factor: \eqn{((k-1)!*(n-k))}\cr
#'              Banzhaf factor: \eqn{(1/(2^(n-1))} \cr
#'              Note that in general the Banzhaf vector is not efficient!
#'
#' @aliases banzhafValue
#' @export banzhafValue
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 367
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 118 f.
#' @inheritParams CoopGameBaseParams 
#' @template param/test
#' @return The return value is a vector which contains the Banzhaf value for each player.
#' @examples
#' A=c(0,0,0,2,2,3,5)
#' banzhafValue(A)
#' #[1] 1.5 2.0 2.0
#' 
banzhafValue<-function(A){
  bv=BanzhafConcept(A)
  n = getNumberOfPlayers(A)
  banzhafFactor=1/(2^(n-1))
  return(calculatePowerIndex(bv)*banzhafFactor)
}

#' @name rawBanzhafValue
#' @title rawBanzhafValue
#' @description raw Banzhaf Value, i.e. the Banzhaf Value without the scaling factor \eqn{(1/(2^(n-1))} 
#' @aliases rawBanzhaValue
#' @export rawBanzhafValue
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 367 ff.
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 118 f.
#' @inheritParams CoopGameBaseParams
#' @return The return value is a vector which contains the raw Banzhaf value for each player.
#' @examples 
#' A<- c(0,0,0,2,2,3,5)
#' rawBanzhafValue(A)
#' #[1] 6 8 8
#' 
rawBanzhafValue<-function(A){
  bi=BanzhafConcept(A)
  n = getNumberOfPlayers(A)
  return(calculatePowerIndex(bi))
}

#' @name rawBanzhafIndex
#' @title rawBanzhafIndex
#' @description Raw Banzhaf Index for a specified simple game 
#' @aliases rawBanzhafIndex
#' @export rawBanzhafIndex
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 367 ff.
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 118 f.
#' @inheritParams CoopGameBaseParams
#' @return The return value is a vector which contains the raw Banzhaf index for each player.
#' @examples 
#' A<- cFuncApexVector(n = 4,apexPlayer=3)
#' rawBanzhafIndex(A=A)
#' #[1] 2 2 6 2
#' 
#' #N=c(1,2,3), w=(50,49,1), q=51   
#' A=cFuncQuotaVector(n=3, w=c(50,49,1),q=51)
#' rawBanzhafIndex(A)
#' #[1] 3 1 1
#' 
#' A<-cFuncQuotaVector(n=3,w=c(50,30,20),q=c(67))
#' rawBanzhafIndex(A)
#' #[1] 3 1 1
#' 
rawBanzhafIndex<-function(A){
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no raw Banzhaf Index can be retrieved")
    return(NULL)
  }
  else
  {
    bi=BanzhafConcept(A)
    n = getNumberOfPlayers(A)
    return(calculatePowerIndex(bi))
  }
}



logicRawBanzhafValue=function(A){

  retVal=NULL
  bI=c()
  
  numberOfPlayers=getNumberOfPlayers(A)
  bm=createBitMatrix(n=numberOfPlayers,A)
  
  for(i in 1:numberOfPlayers){
    #Get all coalitions K where player i takes part
    K=bm[bm[,i]==1,]
    
    #Get all coalitions K \ {i}
    KwithoutI=K
    KwithoutI[,i]=0
    #set v({})=0
    KwithoutI[1,"cVal"]=0
    
    
    for(k in 2:nrow(K)){
      ix=indexCoalition(n=numberOfPlayers, S=getPlayersFromBMRow(KwithoutI[k,]))
      KwithoutI[k, "cVal"]=bm[ix,"cVal"]
    }
    sumMarginalContributions=sum(K[,"cVal"]-KwithoutI[,"cVal"])
    bI[i]=sumMarginalContributions#*banzhafFactor
  }
  
  return (bI)
}

#' @title BanzhafConcept
#' @noRd
#' @include PointSolutionConcept.R
#'# @exportClass BanzhafConcept

setClass(
  "BanzhafConcept",
  contains = "PointSolutionConcept"
)

#' @title Constructor for BanzhafConcept
#' @noRd
#' @template author/JA
#' @name BanzhafConcept
#' #@export
BanzhafConcept<-function(A){
  retBanzhafConcept=methods::new("BanzhafConcept",A)
  return(retBanzhafConcept)
}

#' @rdname calculatePowerIndex-methods
#' @aliases calculatePowerIndex,BanzhafConcept-method
setMethod(
  "calculatePowerIndex",
  signature="BanzhafConcept",
  definition=function(.Object){
    A<-.Object@A
    result=logicRawBanzhafValue(A)
    return(result)
  }
)

#' @name drawNormalizedBanzhafIndex
#' @title draw normalized Banzhaf Index for 3 or 4 players
#' @description drawNormalizedBanzhafIndex draws the Banzhaf Value for 3 or 4 players. \cr
#'              Drawing any kind of Banzhaf values only makes sense from our point of view \cr
#'              for the normalized Banzhaf index for simple games, because \cr
#'              only in this case will the Banzhaf index be efficient.
#' @aliases drawNormalizedBanzhafIndex
#' @export drawNormalizedBanzhafIndex
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 367 ff.
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 118 f.
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' A<-cFuncQuotaVector(n=3,w=c(50,30,20),q=c(67))
#' drawNormalizedBanzhafIndex(A)
#' 
drawNormalizedBanzhafIndex<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Normalized Banzhaf index"){
  A=GameVector(A)
  bv=normalizedBanzhafIndex(A);
  visualize(A, pointsToDraw=bv, holdOn=holdOn, colour = colour , label=label, name = name)
}

##normalized Banzhaf Index
#' @name normalizedBanzhafIndex
#' @title normalizedBanzhafIndex
#' @description Normalized Banzhaf Index for a specified simple game 
#' @aliases normalizedBanzhafIndex
#' @export normalizedBanzhafIndex
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 367 ff.
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 118 f.
#' @inheritParams CoopGameBaseParams
#' @return The return value is a vector which contains the normalized Banzhaf index for each player.
#' @examples 
#' A<-cFuncQuotaVector(n=4,w=c(8,6,4,2),q=c(12))
#' normalizedBanzhafIndex(A)
#' #[1] 0.41666667 0.25000000 0.25000000 0.08333333
#' 
#' A<- cFuncApexVector(n = 4,apexPlayer=3)
#' normalizedBanzhafIndex(A=A)
#' #[1] 0.1666667 0.1666667 0.5000000 0.1666667
#' 
#' #N=c(1,2,3), w=(50,49,1), q=51   
#' A=cFuncQuotaVector(n=3, w=c(50,49,1),q=51)
#' normalizedBanzhafIndex(A)
#' #[1] 0.6 0.2 0.2
#' 
#' A<-cFuncQuotaVector(n=3,w=c(50,30,20),q=c(67))
#' normalizedBanzhafIndex(A)
#' #[1] 0.6 0.2 0.2
#' 
normalizedBanzhafIndex<-function(A){
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Normalized Banzhaf Index can be retrieved")
    return(NULL)
  }
  else
  {
    bc=BanzhafConcept(A)
    bidcs=calculatePowerIndex(bc)
    bidcs=bidcs/sum(bidcs)
    return(bidcs)
  }
}

## non-normalized Banzhaf Index
#' @name nonNormalizedBanzhafIndex
#' @title nonNormalizedBanzhafIndex
#' @description non-normalized Banzhaf Index for a specified simple game
#' @aliases nonNormalizedBanzhafIndex
#' @export nonNormalizedBanzhafIndex
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 367 ff.
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 118 f.
#' @inheritParams CoopGameBaseParams
#' @return The return value is a vector which contains the nonnormalized Banzhaf index for each player.
#' @examples 
#' A<-cFuncQuotaVector(n=4,w=c(8,6,4,2),q=c(12))
#' nonNormalizedBanzhafIndex(A)
#' #[1] 0.625 0.375 0.375 0.125
#' 
#' A<- cFuncApexVector(n = 4,apexPlayer=3)
#' nonNormalizedBanzhafIndex(A=A)
#' #[1] 0.25 0.25 0.75 0.25
#' 
#' #N=c(1,2,3), w=(50,49,1), q=51   
#' A=cFuncQuotaVector(n=3, w=c(50,49,1),q=51)
#' nonNormalizedBanzhafIndex(A)
#' #[1] 0.75 0.25 0.25
#' 
#' A<-cFuncQuotaVector(n=3,w=c(50,30,20),q=c(67))
#' nonNormalizedBanzhafIndex(A)
#' #[1] 0.75 0.25 0.25
#' 
nonNormalizedBanzhafIndex<-function(A){
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Non-normalized Banzhaf Index can be retrieved")
    return(NULL)
  }
  else
  {
    bc=BanzhafConcept(A)
    facBv=2^(getNumberOfPlayers(A)-1)
    bidcs=calculatePowerIndex(bc)
    bidcs=bidcs/facBv
    return(bidcs)
  }
}

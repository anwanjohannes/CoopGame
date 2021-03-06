#' @title GameVector -  class for game vector
#' @description Class for game vector. Constructor is provided by function \link{GameVector}.
#' @exportClass GameVector
setClass(
  "GameVector",
  contains = "numeric",
  validity = function(object){
     paramCheckResult=getEmptyParamCheckResult()
     stopOnInvalidGameVectorA(paramCheckResult,A=(object@.Data))
  }
)

setMethod(
  f="initialize",
  signature = "GameVector",
  definition =  function(.Object, A) {
    paramCheckResult=getEmptyParamCheckResult()
    if(!is.null(A)){
      stopOnInvalidGameVectorA(paramCheckResult,A=A)
    .Object@.Data=A
    }
    methods::validObject(.Object)
    return(.Object)
  }
)

#' @title Constructor for GameVector
#' @description Constructor for instance of \linkS4class{GameVector}.
#' @template author/JA
#' @name GameVector
#' @template param/A
#' @export GameVector
#' @examples 
#' library(CoopGame)
#' GameVector(c(0,0,0,2,5,7,10))
#' #An object of class "GameVector"
#' #[1]  0  0  0  2  5  7 10
GameVector<-function(A){
  retAP=methods::new("GameVector",A)
  return(retAP)
}

#' @title Method getZeroOneNormalizedGame
#' @description This method retrieves the zero-one-normalized game.
#' @rdname getZeroOneNormalizedGame-methods
#' @name getZeroOneNormalizedGame
#' @docType methods
#' @template author/JA
#' @template param/Object
# @exportMethod getZeroOneNormalizedGame
setGeneric("getZeroOneNormalizedGame",function(.Object){standardGeneric("getZeroOneNormalizedGame")})

#' @rdname getZeroOneNormalizedGame-methods
#' @aliases getZeroOneNormalizedGame,GameVector-method
setMethod(
  "getZeroOneNormalizedGame",
  signature = "GameVector",
  definition = function(.Object){
    retVal=NULL
    AzeroNorm<-getZeroNormalizedGame(.Object)
    V_N=AzeroNorm[length(AzeroNorm)]
    if(V_N==0){
      print("Zero-one-normalized game can not be retrieved since value of grand coalition of zero-normalized game is 0.")
    }else{
      retVal=AzeroNorm/V_N
    }
    return(retVal)
  }
)


#' @name getZeroOneNormalizedGameVector
#' @title getZeroOneNormalizedGameVector for TU game with n players
#' @description Computes the zero-one-normalized game for a given game specified by a game vector A.
#' @aliases getZeroOneNormalizedGameVector
#' @export getZeroOneNormalizedGameVector
#' @template author/JA
#' @template cites/GILLES_2015
#' @templateVar GILLES_2015_P p. 18
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 670
#' @inheritParams CoopGameBaseParams
#' @return Numeric vector of length (2^n)-1 representing the zero-one-normalized game.
#' @examples
#' library(CoopGame)
#' A<-c(1:7)
#' #[1] 1 2 3 4 5 6 7
#' getZeroOneNormalizedGameVector(A)
#' #[1] 0 0 0 1 1 1 1
getZeroOneNormalizedGameVector<-function(A){
  gv<-GameVector(A);
  zeroOneNormalizedGame<-getZeroOneNormalizedGame(gv)
  return(zeroOneNormalizedGame)
}

#' @title Method getZeroNormalizedGame
#' @description This method retrieves the zero-normalized game.
#' @rdname getZeroNormalizedGame-methods
#' @name getZeroNormalizedGame
#' @docType methods
#' @template author/JA
#' @template param/Object
# @exportMethod getZeroNormalizedGame
setGeneric("getZeroNormalizedGame",function(.Object){standardGeneric("getZeroNormalizedGame")})

#' @rdname getZeroNormalizedGame-methods
#' @aliases getZeroNormalizedGame,GameVector-method
setMethod(
  "getZeroNormalizedGame",
  signature = "GameVector",
  definition = function(.Object){
    A<-.Object@.Data
    N=length(A)
    n=getNumberOfPlayers(A)
    AzeroNorm=sapply(c(1:N),function(ix){
      involvedPlayers=getPlayersFromIndex(n,ix)
      A[ix]-sum(A[involvedPlayers])
    })
    return(AzeroNorm)
  }
)

#' @name getZeroNormalizedGameVector
#' @title getZeroNormalizedGameVector for TU game with n players
#' @description Computes the zero-normalized game for a given game specified by a game vector A.
#' @aliases getZeroNormalizedGameVector
#' @export getZeroNormalizedGameVector
#' @template author/JA
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 9
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 11
#' @inheritParams CoopGameBaseParams
#' @return Numeric vector of length (2^n)-1 representing the zero-normalized game.
#' @examples
#' library(CoopGame)
#' A<-c(1:7)
#' #[1] 1 2 3 4 5 6 7
#' getZeroNormalizedGameVector(A)
#' #[1] 0 0 0 1 1 1 1
#' 
getZeroNormalizedGameVector<-function(A){
  gv<-GameVector(A);
  zeroNormalizedGame<-getZeroNormalizedGame(gv)
  return(zeroNormalizedGame)
}

#' @title Method getDualGame
#' @description This method retrieves the dual game.
#' @rdname getDualGame-methods
#' @name getDualGame
#' @docType methods
#' @template author/JA
#' @template param/Object
# @exportMethod getDualGame
setGeneric("getDualGame",function(.Object){standardGeneric("getDualGame")})

#' @rdname getDualGame-methods
#' @aliases getDualGame,GameVector-method
setMethod(
  "getDualGame",
  signature = "GameVector",
  definition = function(.Object){
    A<-.Object@.Data
    N=length(A)
    return(c(A[N]-rev(A[-N]),A[N]))
  }
)

#' @name getDualGameVector
#' @title getDualGameVector for TU game with n players
#' @description Computes the dual game for a given TU game specified by a game vector A.
#' @aliases getDualGameVector
#' @export getDualGameVector
#' @template author/JA
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 125
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 7
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 737
#' @inheritParams CoopGameBaseParams
#' @return Numeric vector of length (2^n)-1 representing the dual game.
#' @examples
#' library(CoopGame)
#' A<-cFuncUnanimityVector(4,c(1,2))
#' #[1] 0 0 0 0 1 0 0 0 0 0 1 1 0 0 1
#' getDualGameVector(A)
#' #[1] 1 1 0 0 1 1 1 1 1 0 1 1 1 1 1
#'
getDualGameVector<-function(A){
  gv<-GameVector(A);
  dualGame<-getDualGame(gv)
  return(dualGame)
}



#' @name generateGameVector
#' @title generating game vector
#' @description Generates game vector for given number of players and a given game function
#' @aliases generateGameVector
#' @include bitMatrix.R
#' @export generateGameVector
#' @template author/AT
#' @template param/v
#' @template param/n
#' @template param/others
#' @return generated numeric game vector with 2^n-1 elements (n=number of players)
#' @examples
#' library(CoopGame)
#' generateGameVector(v = cFuncUnanimity, n = 3, T=c(1,2))
#' #An object of class "GameVector"
#' #[1] 0 0 0 1 0 0 1
#' 
generateGameVector<-function(v,n,...){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionFunctionV(paramCheckResult,v)
  stopOnInvalidNumberOfPlayers(paramCheckResult,n)
  #stop on v no function
  cFunc=v(n=n,...)
  #stop oncFunc no Coalition class
  gameVector=cFunc@A
  return(gameVector)
}

#' @name getNumberOfPlayers
#' @title getNumberOfPlayers
#' @description Gets the number of players from a game vector A
#' @aliases getNumberOfPlayers
#' @export getNumberOfPlayers
#' @template author/MM
#' @param A  is a numeric vector of dimension 1x((2^n) - 1)
#' @return number of players
#' @examples
#' library(CoopGame)
#' A=c(0,0,0,60,60,60,72)
#' getNumberOfPlayers(A)
#' #[1] 3
getNumberOfPlayers <- function(A) {
  A<-GameVector(A)
  n <- log2(length(A) + 1)
  numberOfPlayers<-NumberOfPlayers(n)
  return(numberOfPlayers)
}

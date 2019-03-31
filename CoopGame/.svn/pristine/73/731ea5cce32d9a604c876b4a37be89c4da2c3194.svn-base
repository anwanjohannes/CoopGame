#' @title CoopGameCFunc - Virtual class for coalition functions
#' @description A virtual class for the implementation of various 
#' game function concepts like e.g. apex games or cost sharing games, 
#' which can be recognized in CoopGame by the prefix 'cFunc'.
#' @include GameVector.R
#' @include Coalition.R
#' @include NumberOfPlayers.R
#' @template slot/A
#' @template slot/n
#' @exportClass CoopGameCFunc
setClass(
  "CoopGameCFunc",
  representation(A="GameVector", n="NumberOfPlayers", "VIRTUAL"),
  validity=function(object){
    paramCheckResult=getEmptyParamCheckResult()
    stopOnInvalidGameVectorA(paramCheckResult,object@A)
    stopOnInvalidNumberOfPlayers(paramCheckResult,object@n)
  }
)

setMethod(
  f="initialize",
  signature = "CoopGameCFunc",
  definition=function(.Object,n,...){
    .Object@n=NumberOfPlayers(n)
    bitMatrix = createBitMatrix(n)[,1:n]
    A<-c()
    i<-1
    N<-((2^n)-1)
    while(i<=N){
      currCoal<-which(bitMatrix[i,]&1)
      A[i] = getCoalitionValue(.Object,S=currCoal, ...)
      i<-i+1
    }
    .Object@A=GameVector(A=A)
    methods::validObject(.Object)
    return(.Object)
  }
)

#' @title Method getCoalitionValue
#' @description This method returns the coalition value of a given coalition S.
#' @rdname getCoalitionValue-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/S
#' @template param/others
#' @exportMethod getCoalitionValue
setGeneric(
  "getCoalitionValue",
  function(.Object,S="numeric",...){
    standardGeneric("getCoalitionValue")
  }
)




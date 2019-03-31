#' @name isImputation
#' @title isImputation for proving the imputation criteria
#' @description isImputation checks if a given allocation x is an imputation.
#' @aliases isImputation
#' @export isImputation
#' @template author/JA
#' @inheritParams CoopGameBaseParams
#' @param tolerance represents tolerance for checking imputation property
#' @template param/x
#' @return  \code{TRUE} if allocation fits imputation criteria and \code{FALSE} otherwise
#' @examples 
#' #no imputation
#' library(CoopGame)
#' isImputation(x=c(1,1,1),A=c(1:7))
#'
isImputation<-function(x,A,tolerance=1e-12){
  tv=AllocationPropertyImputation(A,x=x,tolerance)
  return(determineProperty(tv))
}

logicIsImputation<-function(A,x,tolerance){
  n=getNumberOfPlayers(A)
  retVal=TRUE
  #check Pareto efficiency x1+..+xn = v(N)
  if(abs(sum(x)-A[length(A)])>tolerance){
    print("Constraint for Pareto efficiency is hurt")
    retVal=FALSE
  #individual rationality
  }else if(!all(A[1:n]<=x,TRUE)){
    print("Constraint for individual rationality is hurt")
    retVal=FALSE
  }
  return(retVal)
}



#' @title AllocationPropertyImputation -  class for checking imputation property
#' @noRd
#' @description Class for implementation of imputation concept 
#' @include GameProperty.R
#'# @exportClass AllocationPropertyImputation

setClass(
  "AllocationPropertyImputation",
  contains = "AllocationProperty",
  representation(
    tolerance = "numeric"
  )
)

#' @title Constructor for AllocationPropertyImputation
#' @noRd
#' @template author/JA
#' @name AllocationPropertyImputation
#' #@export
AllocationPropertyImputation<-function(A,x=x,tolerance=1e-12){
  retAP=methods::new("AllocationPropertyImputation",A,x=x,tolerance=tolerance)
  return(retAP)
}

#' @rdname determineProperty-methods
#' @aliases determineProperty,AllocationPropertyImputation-method
setMethod(
  "determineProperty",
  signature="AllocationPropertyImputation",
  definition=function(.Object){
    A<-.Object@A
    x<-.Object@x
    tolerance<-.Object@tolerance
    return(logicIsImputation(A,x,tolerance))
  }
)

setMethod(
  f="initialize",
  signature = "AllocationPropertyImputation",
  definition =  function(.Object, ...,tolerance) {
    .Object@tolerance<-tolerance
    .Object<-methods::callNextMethod(.Object, ...)
    return(.Object)
  }
)



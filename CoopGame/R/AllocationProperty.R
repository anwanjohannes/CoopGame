#' @title AllocationProperty - Virtual class for allocation concepts
#' @description Virtual class for allocation property concepts, inherits methods and slots from class
#' \linkS4class{CoopGameProperty}.
#' @include CoopGameProperty.R
#' @include Allocation.R
#' @template slot/x
#' @exportClass AllocationProperty
setClass(
  "AllocationProperty",
  representation(x="Allocation","VIRTUAL"),
  contains = "CoopGameProperty"
)

setMethod(
  f="initialize",
  signature = "AllocationProperty",
  definition =  function(.Object, ..., x=x) {
    .Object@x<-Allocation(x)
    .Object<-methods::callNextMethod(.Object, ...)
    return(.Object)
  }
)

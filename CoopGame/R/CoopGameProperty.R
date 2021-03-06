#' @title CoopGameProperty - Virtual class for game and allocation property concepts
#' @description Virtual class for property concepts, inherits methods and slots from class
#' \linkS4class{CoopGameBase}.
#' @include CoopGameBase.R
#' @exportClass CoopGameProperty
setClass(
  "CoopGameProperty",
  representation("VIRTUAL"),
  contains = "CoopGameBase"
)

setMethod(
  f="initialize",
  signature = "CoopGameProperty",
  definition =  function(.Object, ...) {
    .Object<-methods::callNextMethod(.Object, ...)
    return(.Object)
  }
)


#' @title Method determineProperty
#' @description This method determines if a game property is satisfied.
#' @rdname determineProperty-methods
#' @template author/JA
#' @docType methods
#' @template param/Object
#' @exportMethod determineProperty
setGeneric(
  "determineProperty",
  function(.Object){
    standardGeneric("determineProperty")
  }
)



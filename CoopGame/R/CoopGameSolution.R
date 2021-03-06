#' @title CoopGameSolution - Virtual class for solution concepts
#' @description Virtual class for point or set based solution concepts, inherits methods and slots from class
#' \linkS4class{CoopGameBase}
#' @include CoopGameBase.R
#' @exportClass CoopGameSolution

setClass(
  "CoopGameSolution",
  representation("VIRTUAL"),
  contains = "CoopGameBase"
)

#' @title Method calculateSolution
#' @description This method calculates the allocation to a solution concept.
#' @rdname calculateSolution-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod calculateSolution
setGeneric(
  "calculateSolution",
  function(.Object){
    standardGeneric("calculateSolution")
  }
)


#' @title SetSolutionConcept - Virtual class for set based solution concepts
#' @description Virtual class for set based solution concepts, inherits methods and slots from class
#' \linkS4class{CoopGameSolution}
#' @include CoopGameSolution.R
#' @exportClass SetSolutionConcept

setClass(
  "SetSolutionConcept",
  representation("VIRTUAL"),
  contains = "CoopGameSolution"
)

#' @title calculateSetSolution
#' @name calculateSetSolution
#' @description calculateSetSolution
#' @template author/JA
#' @template param/Object
#' @exportMethod calculateSetSolution
setGeneric(
  "calculateSetSolution",
  function(.Object){
    standardGeneric("calculateSetSolution")
  }
)

#' @title Method calculateSetSolution
#' @description This method calculates the allocation to an set based solution concept.
#' @rdname calculateSetSolution-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod calculateSetSolution
setMethod(
  "calculateSetSolution",
  signature="SetSolutionConcept",
  definition=function(.Object){
    A<-.Object@A
    VRep=(getSetSolutionVertices(.Object))
    payoffSpace=PayoffSpace(VRepMatrix = VRep)
    return(payoffSpace)
  }
)

#' @rdname calculateSolution-methods
#' @aliases calculateSolution,SetSolutionConcept-method
setMethod(
  "calculateSolution",
  signature="SetSolutionConcept",
  definition=function(.Object){
    calculateSetSolution(.Object)
  }
)

#' @title Method getSetSolutionVertices
#' @description This method retrieves the vertices of the v-Representation.
#' @rdname getSetSolutionVertices-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getSetSolutionVertices
setGeneric(
  "getSetSolutionVertices",
  function(.Object){
    standardGeneric("getSetSolutionVertices")
  }
)


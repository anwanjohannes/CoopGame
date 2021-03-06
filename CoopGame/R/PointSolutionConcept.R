#' @title PointSolutionConcept - Virtual class for point valued solution concepts
#' @description Virtual class for point valued solution concepts, inherits methods and slots from class
#' \linkS4class{CoopGameSolution}
#' @include CoopGameSolution.R
#' @exportClass PointSolutionConcept

setClass(
  "PointSolutionConcept",
  representation("VIRTUAL"),
  contains = "CoopGameSolution"
)



#' @title Method calculatePointSolution
#' @description This method calculates the allocation of a unique solution concept.
#' @rdname calculatePointSolution-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod calculatePointSolution
setGeneric(
  "calculatePointSolution",
  function(.Object){
    standardGeneric("calculatePointSolution")
  }
)

#' @rdname calculatePointSolution-methods
#' @aliases calculatePointSolution,PointSolutionConcept-method
setMethod(
  "calculatePointSolution",
  signature="PointSolutionConcept",
  definition=function(.Object){
    A<-.Object@A
    N<-length(A)
    powerIndex<-calculatePowerIndex(.Object)
    return((powerIndex*A[N])/sum(powerIndex))
  }
)

#' @rdname calculateSolution-methods
#' @aliases calculateSolution,PointSolutionConcept-method
setMethod(
  "calculateSolution",
  signature="PointSolutionConcept",
  definition=function(.Object){
    calculatePointSolution(.Object)
  }
)

#' @title Method calculatePowerIndex
#' @description This method calculates the power index of a unique solution concept.
#' @rdname calculatePowerIndex-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod calculatePowerIndex
setGeneric(
  "calculatePowerIndex",
  function(.Object){
    standardGeneric("calculatePowerIndex")
  }
)


#' @rdname calculatePowerIndex-methods
#' @aliases calculatePowerIndex,PointSolutionConcept-method
setMethod(
  "calculatePowerIndex",
  signature="PointSolutionConcept",
  definition=function(.Object){
    pointSolution=calculatePointSolution(.Object)
    return(pointSolution)
  }
)

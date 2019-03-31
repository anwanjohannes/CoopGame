#' @title CoopGameBase Parameters - Basic parameters for CoopGame 
#' @name CoopGameBaseParams
#' @description In the package CoopGame all solution, allocation and game property as well as utility concepts
#' share at least the game vector A which represents a cooperative game.
#' @template param/A
NULL 

#' @title CoopGameBase - Virtual class as base for abstract concepts from classical cooperative game theory
#' @description A virtual class as base for abstract concepts from classical cooperative game theory.
#' @include GameVector.R
#' @template slot/A
#' @exportClass CoopGameBase
setClass(
  "CoopGameBase",
  representation(A="GameVector", "VIRTUAL"),
  validity=function(object){
    paramCheckResult=getEmptyParamCheckResult()
    stopOnInvalidGameVectorA(paramCheckResult,object@A)
  }
  
)

setMethod(
    f="initialize",
    signature = "CoopGameBase",
    definition =  function(.Object, A){
      A=GameVector(A)
      .Object@A=A
      #methods::validObject(.Object)
      return(.Object)
    }
)



#' @title Method setA.
#' @description This method sets the object's slot to a game vector.
#' @rdname setA-methods
#' @name setA<-
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/value
#' @exportMethod setA<-
setGeneric(
  "setA<-",
  function(.Object, value){
    standardGeneric("setA<-")
  }
  
)

#' @rdname setA-methods
#' @aliases setA<-,CoopGameBase-method
setReplaceMethod(
  "setA",
  signature="CoopGameBase",
  definition=function(.Object, value){
    .Object@A<-value
    return(.Object)
  }
)


#' @title Method getA.
#' @description This method returns a game vector of the slot of the object.
#' @rdname getA-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getA
setGeneric(
  "getA",
  function(.Object){
    standardGeneric("getA")
  }
)

#' @rdname getA-methods
#' @aliases getA,CoopGameBase-method
setMethod(
  "getA",
  signature="CoopGameBase",
  definition=function(.Object){
    return(.Object@A)
  }
)
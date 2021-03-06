#' @title Constructor for LPMatrix
#' @description Constructor for instance of class \linkS4class{LPMatrix}
#' @template author/JA
#' @name LPMatrix
#' @template param/matrix
# @export
LPMatrix<-function(matrix){methods::new("LPMatrix",matrix=matrix)}

#' @title LPMatrix - Managing numeric values of the LP constraint coefficients
#' @description An S4 class representing a utility for managing numeric values of the constraint coefficients of a linear program 
#' frequently used in CoopGame.
#' @import glpkAPI
#' @slot matrix containing the numeric values of the constraint coefficients
#' @exportClass LPMatrix
setClass(
  "LPMatrix",
  representation(matrix="matrix")
)

#' @title Method getMatrix
#' @description This method returns the matrix data.
#' @rdname getMatrix-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getMatrix
setGeneric("getMatrix",function(.Object){standardGeneric("getMatrix")})

#' @rdname getMatrix-methods
#' @aliases getMatrix,LPMatrix-method
setMethod(
  "getMatrix",
  signature = "LPMatrix",
  definition = function(.Object){
    return(.Object@matrix)
  }
)


#' @title Method setMatrix<-
#' @description This method sets the matrix data for the linear program containing the coefficents.
#' @rdname setMatrix-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/value
#' @exportMethod setMatrix<-
setGeneric("setMatrix<-",function(.Object,value){standardGeneric("setMatrix<-")})

#' @rdname setMatrix-methods
#' @aliases setMatrix<-,LPMatrix-method
setReplaceMethod(
  f="setMatrix",
  signature="LPMatrix",
  definition=function(.Object,value){
    .Object@matrix<-value
    return(.Object)
  }
)


#' @title Method setMatrixLastCol<-
#' @description This method sets the last column of the matrix data for the linear program containing the coefficents.
#' @rdname setMatrixLastCol-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/value
#' @exportMethod setMatrixLastCol<-
setGeneric("setMatrixLastCol<-",function(.Object,value){standardGeneric("setMatrixLastCol<-")})


#' @rdname setMatrixLastCol-methods
#' @aliases setMatrixLastCol<-,LPMatrix-method
setReplaceMethod(
  f="setMatrixLastCol",
  signature="LPMatrix",
  definition=function(.Object,value){
    .Object@matrix[,"cVal"]<-value
    return(.Object)
  }
)


#' @title Method getMatrixLastCol
#' @description This method gets the last column of the matrix data for the linear program containing the coefficents.
#' @rdname getMatrixLastCol-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getMatrixLastCol
setGeneric("getMatrixLastCol",function(.Object){standardGeneric("getMatrixLastCol")})

#' @rdname getMatrixLastCol-methods
#' @aliases getMatrixLastCol,LPMatrix-method
setMethod(
  "getMatrixLastCol",
  signature = "LPMatrix",
  definition = function(.Object){
    return(.Object@matrix[,"cVal"])
  }
)

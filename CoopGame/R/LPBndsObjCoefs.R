#' @title Constructor for LPBndsObjCoefs
#' @noRd
#' @description Constructs instance of class \linkS4class{LPBndsObjCoefs}.
#' @template param/clb
#' @template param/cub
#' @template param/ctype
#' @template param/obj
#' @name LPBndsObjCoefs
#' @template author/JA
# @export
LPBndsObjCoefs<-function(clb,cub,ctype,obj){
  methods::new("LPBndsObjCoefs",clb=clb,cub=cub,ctype=ctype,obj=obj)
}

#' @title LPBndsObjCoefs - Managing columns of LPs
#' @template author/JA
#' @description An S4 class representing a utility for the columns of a linear program 
#' frequently used in CoopGame.
#' @slot clb numeric vector with lower bounds
#' @slot cub numeric vector with upper bounds
#' @slot ctype numeric vector indicating the column types
#' @slot obj numeric vector containing the objective function
#' @exportClass LPBndsObjCoefs

#Class LPBndsObjCoefs definition
setClass(
  "LPBndsObjCoefs",
  representation(
                 clb="numeric",
                 cub="numeric",
                 ctype="numeric",
                 obj="numeric"
  ),
  validity = function(object){
    lengthClb=length(object@clb)
    lengthCub=length(object@cub)
    lengthCtype=length(object@ctype)
    lengthObj=length(object@obj)
    if(!all(c(lengthClb,lengthCub,lengthCtype)==lengthObj)){
      return(FALSE)
    }
  }
)


#' @title Method getClb
#' @description This method gets lower bounds of the objective function.
#' @rdname getClb-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getClb
setGeneric("getClb",function(.Object){standardGeneric("getClb")})

#' @rdname getClb-methods
#' @aliases getClb,LPBndsObjCoefs-method
setMethod(
  "getClb",
  signature = "LPBndsObjCoefs",
  definition = function(.Object){
    return(.Object@clb)
  }
)


#' @title Method setClb
#' @description This method sets lower bounds of the objective function.
#' @rdname setClb-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/value
#' @exportMethod setClb<-
setGeneric("setClb<-",function(.Object,value){standardGeneric("setClb<-")})

#' @rdname setClb-methods
#' @aliases setClb<-,LPBndsObjCoefs-method
setReplaceMethod(
  f="setClb",
  signature="LPBndsObjCoefs",
  definition=function(.Object,value){
    .Object@clb<-value
    return(.Object)
  }
)


#' @title Method getCub
#' @description This method gets upper bounds of the objective function.
#' @rdname getCub-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getCub
setGeneric("getCub",function(.Object){standardGeneric("getCub")})

#' @rdname getCub-methods
#' @aliases getCub,LPBndsObjCoefs-method
setMethod(
  "getCub",
  signature = "LPBndsObjCoefs",
  definition = function(.Object){
    return(.Object@cub)
  }
)



#' @title Method setCub
#' @description This method sets upper bounds of the objective function.
#' @rdname setCub-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/value
#' @exportMethod setCub<-
setGeneric("setCub<-",function(.Object,value){standardGeneric("setCub<-")})

#' @rdname setCub-methods
#' @aliases setCub<-,LPBndsObjCoefs-method
setReplaceMethod(
  f="setCub",
  signature="LPBndsObjCoefs",
  definition=function(.Object,value){
    .Object@cub<-value
    return(.Object)
  }
)



#' @title Method getObj
#' @description This method gets the coefficients of the objective function.
#' @rdname getObj-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getObj
setGeneric("getObj",function(.Object){standardGeneric("getObj")})

#' @rdname getObj-methods
#' @aliases getObj,LPBndsObjCoefs-method
setMethod(
  "getObj",
  signature = "LPBndsObjCoefs",
  definition = function(.Object){
    return(.Object@obj)
  }
)


#' @title Method setObj
#' @description This method sets the coefficients of the objective function. 
#' @rdname setObj-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/value
#' @exportMethod setObj<-
setGeneric("setObj<-",function(.Object,value){standardGeneric("setObj<-")})

#' @rdname setObj-methods
#' @aliases setObj<-,LPBndsObjCoefs-method
setReplaceMethod(
  f="setObj",
  signature="LPBndsObjCoefs",
  definition=function(.Object,value){
    .Object@obj<-value
    return(.Object)
  }
)


#' @title Method getCtype
#' @description This method gets the numeric vector indicating the column types. 
#' @rdname getCtype-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getCtype
setGeneric("getCtype",function(.Object){standardGeneric("getCtype")})

#' @rdname getCtype-methods
#' @aliases getCtype,LPBndsObjCoefs-method
setMethod(
  "getCtype",
  signature = "LPBndsObjCoefs",
  definition = function(.Object){
    return(.Object@ctype)
  }
)



#' @title Method setCtype
#' @description This method sets the numeric vector indicating the column types. 
#' @rdname setCtype-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/value
#' @exportMethod setCtype<-
setGeneric("setCtype<-",function(.Object,value){standardGeneric("setCtype<-")})

#' @rdname setCtype-methods
#' @aliases setCtype<-,LPBndsObjCoefs-method
setReplaceMethod(
  f="setCtype",
  signature="LPBndsObjCoefs",
  definition=function(.Object,value){
    .Object@ctype<-value
    return(.Object)
  }
)



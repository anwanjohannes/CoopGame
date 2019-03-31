#' @title Constructor for LPRows
#' @description Constructor for instance of \linkS4class{LPRows}
#' @name LPRows
#' @template author/JA
#' @template param/rlb
#' @template param/rub
#' @template param/rtype
#' @export
LPRows<-function(rlb,rub,rtype){methods::new("LPRows",rlb=rlb,rub=rub,rtype=rtype)}

#' @title LPRows - Managing rows of lp
#' @template author/JA
#' @description An S4 class representing an utility for rows of a linear program
#' as often used in the Game Theory package CoopGame.
#' @slot rlb numeric vector with lower bounds
#' @slot rub numeric vector with upper bounds
#' @slot rtype numeric vector indicating the row types
#' @exportClass LPRows


#Class LPRows definition
setClass(
  "LPRows",
  representation(
        rlb="numeric",
        rub="numeric",
        rtype="numeric"
  ),
  validity = function(object){
    retVal=TRUE
    lengthRlb=length(object@rlb)
    lengthRub=length(object@rub)
    lengthRtype=length(object@rtype)
    if(!all(c(lengthRub,lengthRlb)==lengthRtype)){
      retVal=FALSE
    }else if(log2(lengthRub+1)%%1!=0){
      retVal=FALSE
    }
    return(retVal)
  }
)



#' @title Method getRlb
#' @description This method gets the row lower bounds of the linear program.
#' @rdname getRlb-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getRlb
setGeneric("getRlb",function(.Object){standardGeneric("getRlb")})

#' @rdname getRlb-methods
#' @aliases getRlb,LPRows-method
setMethod(
  "getRlb",
  signature = "LPRows",
  definition = function(.Object){
    return(.Object@rlb)
  }
)


#' @title Method setRlb
#' @description This method sets the row lower bounds of the linear program.
#' @rdname setRlb-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/value
#' @exportMethod setRlb<-
setGeneric("setRlb<-",function(.Object,value){standardGeneric("setRlb<-")})

#' @rdname setRlb-methods
#' @aliases setRlb<-,LPRows-method
setReplaceMethod(
  f="setRlb",
  signature="LPRows",
  definition=function(.Object,value){
    .Object@rlb<-value
    return(.Object)
  }
)


#' @title Method getRub
#' @description This method gets the row upper bounds of the linear program.
#' @rdname getRub-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getRub
setGeneric("getRub",function(.Object){standardGeneric("getRub")})

#' @rdname getRub-methods
#' @aliases getRub,LPRows-method
setMethod(
  "getRub",
  signature = "LPRows",
  definition = function(.Object){
    return(.Object@rub)
  }
)


#' @title Method setRub
#' @description This method sets the row upper bounds of the linear program.
#' @rdname setRub-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/value
#' @exportMethod setRub<-
setGeneric("setRub<-",function(.Object,value){standardGeneric("setRub<-")})

#' @rdname setRub-methods
#' @aliases setRub<-,LPRows-method
setReplaceMethod(
  f="setRub",
  signature="LPRows",
  definition=function(.Object,value){
    .Object@rub<-value
    return(.Object)
  }
)



#' @title Method getRtype
#' @description This method gets the row types of the linear program.
#' @rdname getRtype-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getRtype
setGeneric("getRtype",function(.Object){standardGeneric("getRtype")})

#' @rdname getRtype-methods
#' @aliases getRtype,LPRows-method
setMethod(
  "getRtype",
  signature = "LPRows",
  definition = function(.Object){
    return(.Object@rtype)
  }
)



#' @title Method setRtype
#' @description This method sets the row types of the linear program.
#' @rdname setRtype-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/value
#' @exportMethod setRtype<-
setGeneric("setRtype<-",function(.Object,value){standardGeneric("setRtype<-")})

#' @rdname setRtype-methods
#' @aliases setRtype<-,LPRows-method
setReplaceMethod(
  f="setRtype",
  signature="LPRows",
  definition=function(.Object,value){
    .Object@rtype<-value
    return(.Object)
  }
)
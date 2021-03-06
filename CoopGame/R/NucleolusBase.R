#' @title NucleolusBase - Base class for various Nucleolus derivatives
#' @template author/JA
#' @description An S4 class for various solution concepts based on the solution concept Nucleolus. It inherits methods and slots from class
#' \linkS4class{PointSolutionConcept}
#' @include LPCoopGameUtils.R
#' @include PointSolutionConcept.R
#' @import glpkAPI
#' @slot LPCoopGameUtils instance of \linkS4class{LPCoopGameUtils}
#' @exportClass NucleolusBase


setClass(
  "NucleolusBase",
  contains = "PointSolutionConcept",
  representation(
    LPCoopGameUtils="LPCoopGameUtils",
    "VIRTUAL"
  )
)

#' @title Method updateLPBndsObjCoefs
#' @description This method updates the \linkS4class{LPBndsObjCoefs} 
#' class and sets attributes.
#' @rdname updateLPBndsObjCoefs-methods
#' @include LPBndsObjCoefs.R
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod updateLPBndsObjCoefs

setGeneric(
  "updateLPBndsObjCoefs",
  function(.Object){
    standardGeneric("updateLPBndsObjCoefs")
  }
)

#' @rdname updateLPBndsObjCoefs-methods
#' @aliases updateLPBndsObjCoefs,NucleolusBase-method
setMethod(
  "updateLPBndsObjCoefs",
  signature="NucleolusBase",
  definition=function(.Object){
    NULL
  }
)

setMethod(
  f="initialize",
  signature = "NucleolusBase",
  definition =  function(.Object, ...) {
    .Object<-methods::callNextMethod(.Object, ...)
    .Object@LPCoopGameUtils<-LPCoopGameUtils()
    initLPMatrix(.Object)
    initLPRows(.Object)
    initLPBndsObjCoefs(.Object)
    updateLPCoopGameUtils(.Object@LPCoopGameUtils)
    return(.Object)
  }
)


#' @title Method updateNucleolusBase
#' @description This method updates the nucleolus class and sets attributes.
#' @rdname updateNucleolusBase-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod updateNucleolusBase
setGeneric(
  "updateNucleolusBase",
  function(.Object){
    standardGeneric("updateNucleolusBase")
  }
)

#' @rdname updateNucleolusBase-methods
#' @aliases updateNucleolusBase,NucleolusBase-method
setMethod(
  "updateNucleolusBase",
  signature="NucleolusBase",
  definition=function(.Object){
    obj<-.Object
    updateLPMatrix(obj)
    updateLPRows(obj)
    updateLPBndsObjCoefs(obj)
    updateLPCoopGameUtils(obj@LPCoopGameUtils)
    eval.parent(substitute(.Object<-obj))
  }
)


#' @title Method updateLPMatrix
#' @description This method updates the \linkS4class{LPMatrix} class and sets attributes.
#' @rdname updateLPMatrix-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod updateLPMatrix

setGeneric(
  "updateLPMatrix",
  function(.Object){
    standardGeneric("updateLPMatrix")
  }
)

#' @rdname updateLPMatrix-methods
#' @aliases updateLPMatrix,NucleolusBase-method
setMethod(
  "updateLPMatrix",
  signature="NucleolusBase",
  definition=function(.Object){
    lpCoopGameUtils<-.Object@LPCoopGameUtils
    excessCoefs=getMatrixLastCol(lpCoopGameUtils)
    pos=getLPDualSolutionPos(.Object)
    excessCoefs[pos]<-0
    setMatrixLastCol(lpCoopGameUtils)<-excessCoefs
    eval.parent(substitute(.Object@LPCoopGameUtils<-lpCoopGameUtils))
  }
)


#' @title Method getLPCoopGameUtils
#' @description This method updates the \linkS4class{LPCoopGameUtils} class and sets attributes.
#' @rdname getLPCoopGameUtils-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getLPCoopGameUtils

setGeneric(
  "getLPCoopGameUtils",
  function(.Object){
    standardGeneric("getLPCoopGameUtils")
  }
)

#' @rdname getLPCoopGameUtils-methods
#' @aliases getLPCoopGameUtils,NucleolusBase-method
setMethod(
  "getLPCoopGameUtils",
  signature="NucleolusBase",
  definition=function(.Object){
    return(.Object@LPCoopGameUtils)
  }
)

#' @title Method setLPCoopGameUtils
#' @description This method sets \linkS4class{LPCoopGameUtils} class as attribute.
#' @rdname setLPCoopGameUtils-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/value
#' @exportMethod setLPCoopGameUtils<-
setGeneric(
  "setLPCoopGameUtils<-",
  function(.Object,value){
    standardGeneric("setLPCoopGameUtils<-")
  }
  
)

#' @rdname setLPCoopGameUtils-methods
#' @aliases setLPCoopGameUtils<-,NucleolusBase-method
setReplaceMethod(
  "setLPCoopGameUtils",
  signature="NucleolusBase",
  definition=function(.Object,value){
    .Object@LPCoopGameUtils<-value
    return(.Object)
  }
)


#' @title Method getLPRowsBoundsFunc
#' @description This method determines the value for the row bound.
#' @rdname getLPRowsBoundsFunc-methods
#' @docType methods
#' @template author/JA
#' @param x restriction which is processed
#' @param pos row index of restriction in coefficients matrix
#' @template param/Object
#' @exportMethod getLPRowsBoundsFunc

setGeneric(
  "getLPRowsBoundsFunc",
  function(.Object,x,pos){
    standardGeneric("getLPRowsBoundsFunc")
  }
)

#' @rdname getLPRowsBoundsFunc-methods
#' @aliases getLPRowsBoundsFunc,NucleolusBase-method
setMethod(
  "getLPRowsBoundsFunc",
  signature="NucleolusBase",
  definition=function(.Object,x,pos){
    primal=getLPPrimalSolution(.Object@LPCoopGameUtils)
    n=length(primal)
    primal=primal[-n]
    return(sum(x[-n] * primal[-n]))
  }
)

#' @title Method updateLPRows
#' @description This method updates the instance of \linkS4class{LPRows} and sets attributes.
#' @rdname updateLPRows-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod updateLPRows

setGeneric(
  "updateLPRows",
  function(.Object){
    standardGeneric("updateLPRows")
  }
)

#' @rdname updateLPRows-methods
#' @aliases updateLPRows,NucleolusBase-method
setMethod(
  "updateLPRows",
  signature="NucleolusBase",
  definition=function(.Object){
    lpCoopGameUtils<-.Object@LPCoopGameUtils
    rlb=getRlb(lpCoopGameUtils)
    rub=getRub(lpCoopGameUtils)
    rtype=getRtype(lpCoopGameUtils)
    pos=getLPDualSolutionPos(.Object)
    coeffMat=getMatrix(lpCoopGameUtils)[pos,,drop=FALSE]
    rlb[pos] <- rub[pos] <- apply(
      coeffMat,1,FUN = getLPRowsBoundsFunc,.Object=.Object,pos=pos
    )
    rtype[pos] <- GLP_FX
    setRlb(lpCoopGameUtils)<-rlb
    setRub(lpCoopGameUtils)<-rub
    setRtype(lpCoopGameUtils)<-rtype
    eval.parent(substitute(.Object@LPCoopGameUtils<-lpCoopGameUtils))
  }
)


#' @title Method getLPDualSolutionPos
#' @description This method identifies the restrictions to be updated.
#' @rdname getLPDualSolutionPos-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getLPDualSolutionPos

setGeneric(
  "getLPDualSolutionPos",
  function(.Object){
    standardGeneric("getLPDualSolutionPos")
  }
)

#' @rdname getLPDualSolutionPos-methods
#' @aliases getLPDualSolutionPos,NucleolusBase-method
setMethod(
  "getLPDualSolutionPos",
  signature="NucleolusBase",
  definition=function(.Object){
    return(which(getLPDualSolution(.Object@LPCoopGameUtils)>0))
  }
)

#' @title Method initLPRows
#' @description This method initializes the class \linkS4class{LPRows}.
#' @rdname initLPRows-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod initLPRows

setGeneric(
  "initLPRows",
  function(.Object){
    standardGeneric("initLPRows")
  }
)

#' @rdname initLPRows-methods
#' @aliases initLPRows,NucleolusBase-method
setMethod(
  "initLPRows",
  signature="NucleolusBase",
  definition=function(.Object){
    lpCoopGameUtils<-.Object@LPCoopGameUtils
    N<-length(.Object@A)
    setRlb(lpCoopGameUtils)<-.Object@A
    setRub(lpCoopGameUtils)<-c(rep(Inf, N - 1), .Object@A[N])
    setRtype(lpCoopGameUtils)<-c(rep(GLP_LO, N - 1), GLP_FX)
    eval.parent(substitute(.Object@LPCoopGameUtils<-lpCoopGameUtils))
  }
)


#' @title Method initLPMatrix
#' @description This method initializes the class \linkS4class{LPMatrix}.
#' @rdname initLPMatrix-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod initLPMatrix

setGeneric(
  "initLPMatrix",
  function(.Object){
    standardGeneric("initLPMatrix")
  }
)

#' @rdname initLPMatrix-methods
#' @aliases initLPMatrix,NucleolusBase-method
setMethod(
  "initLPMatrix",
  signature="NucleolusBase",
  definition=function(.Object){
    lpCoopGameUtils<-.Object@LPCoopGameUtils
    excess<-determineExcessCoefficients(.Object)
    coeffMat<-createBitMatrix(n=getNumberOfPlayers(.Object@A),excess)
    setMatrix(lpCoopGameUtils)<-coeffMat
    eval.parent(substitute(.Object@LPCoopGameUtils<-lpCoopGameUtils))
  }
)

#' @title Method initLPBndsObjCoefs
#' @description This method initializes the class \linkS4class{LPBndsObjCoefs}.
#' @rdname initLPBndsObjCoefs-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod initLPBndsObjCoefs
setGeneric(
  "initLPBndsObjCoefs",
  function(.Object){
    standardGeneric("initLPBndsObjCoefs")
  }
)

#' @rdname initLPBndsObjCoefs-methods
#' @aliases initLPBndsObjCoefs,NucleolusBase-method
setMethod(
  "initLPBndsObjCoefs",
  signature="NucleolusBase",
  definition=function(.Object){
    lpCoopGameUtils<-.Object@LPCoopGameUtils
    n=getNumberOfPlayers(.Object@A)
    setClb(lpCoopGameUtils)<- c(.Object@A[1:n], -Inf) #changed from c(rep(0, n), -Inf)
    setCub(lpCoopGameUtils)<- rep(Inf, n + 1)
    setCtype(lpCoopGameUtils)<- c(rep(GLP_DB, n), GLP_FR)
    setObj(lpCoopGameUtils)<- c(rep(0, n), 1)
    eval.parent(substitute(.Object@LPCoopGameUtils<-lpCoopGameUtils))
  }
)

#' @title Method determineExcessCoefficients
#' @description This method determines the excess coefficients.
#' @rdname determineExcessCoefficients-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod determineExcessCoefficients
setGeneric(
  "determineExcessCoefficients",
  function(.Object){
    standardGeneric("determineExcessCoefficients")
  }
)

#' @rdname determineExcessCoefficients-methods
#' @aliases determineExcessCoefficients,NucleolusBase-method
setMethod(
  "determineExcessCoefficients",
  signature="NucleolusBase",
  definition=function(.Object){
    N=length(.Object@A)
    return(c(rep(1,N-1),0))
  }
)


#' @title Method checkAbort
#' @description This method checks if calculation of nucleolus should be aborted.
#' @rdname checkAbort-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @param oldObjValue previous value of objective function
#' @exportMethod checkAbort
setGeneric(
  "checkAbort",
  function(.Object,oldObjValue){
    standardGeneric("checkAbort")
  }
)

#' @rdname checkAbort-methods
#' @aliases checkAbort,NucleolusBase-method
setMethod(
  "checkAbort",
  signature="NucleolusBase",
  definition=function(.Object,oldObjValue){
    retVal=FALSE
    lpCoopGameUtils<-.Object@LPCoopGameUtils
    if((!is.null(oldObjValue))){
      if(oldObjValue==getLPObjVal(lpCoopGameUtils)){
        retVal=TRUE
      }
    }else if(!isLPFeasible(lpCoopGameUtils)){
      stop("No Solution exists for this game.")
    }
    return(retVal)
  }
)


#' @title Method checkGamePreconditions
#' @description This method checks if preconditions for the calculation of nucleolus are satisfied.
#' @rdname checkGamePreconditions-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod checkGamePreconditions
setGeneric(
  "checkGamePreconditions",
  function(.Object){
    standardGeneric("checkGamePreconditions")
  }
)

#' @rdname checkGamePreconditions-methods
#' @aliases checkGamePreconditions,NucleolusBase-method
setMethod(
  "checkGamePreconditions",
  signature="NucleolusBase",
  definition=function(.Object){
    boolEssentialOrDegenerateGame=(isEssentialGame(.Object@A) || isDegenerateGame(.Object@A))
    if(!boolEssentialOrDegenerateGame){
      stop("Nucleolus solution is stopped as game is neither essential nor degenerate.")
    }
  }
)



#' @title Method calculateNucleolus
#' @description This method calculates the nucleolus or one of its derivatives.
#' @rdname calculateNucleolus-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/enableTermOutLP
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 82 ff.
#' @exportMethod calculateNucleolus
setGeneric(
  "calculateNucleolus",
  function(.Object,enableTermOutLP=FALSE){
    standardGeneric("calculateNucleolus")
  }
)


#' @rdname calculateNucleolus-methods
#' @aliases calculateNucleolus,NucleolusBase-method
setMethod(
  "calculateNucleolus",
  signature="NucleolusBase",
  definition=function(.Object,enableTermOutLP = FALSE){
    checkGamePreconditions(.Object)
    N=getNumberOfRows(.Object@LPCoopGameUtils)
    oldObjValue=NULL
    primal=NULL
    
    for(i in 1:N){
      solveLP(.Object@LPCoopGameUtils,enableTermOutLP)
      
      if(checkAbort(.Object,oldObjValue)){
        break
      }
      
      oldObjValue=getLPObjVal(.Object@LPCoopGameUtils)
      primal=getLPPrimalSolution(.Object@LPCoopGameUtils)
      updateNucleolusBase(.Object)
      
      if(all(getMatrixLastCol(.Object@LPCoopGameUtils)==0)) {
        break
      }
      
    }
    return((primal[-length(primal)]))
  }
)

#' @rdname calculatePointSolution-methods
#' @aliases calculatePointSolution,NucleolusBase-method
setMethod(
  "calculatePointSolution",
  signature="NucleolusBase",
  definition=function(.Object){
    calculateNucleolus(.Object)
  }
)


#' @rdname calculateSolution-methods
#' @aliases calculateSolution,NucleolusBase-method
setMethod(
  "calculateSolution",
  signature="NucleolusBase",
  definition=function(.Object){
    calculateNucleolus(.Object)
  }
)
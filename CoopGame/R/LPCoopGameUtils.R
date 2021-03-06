#' @title Constructor for LPCoopGameUtils
#' @description Constructor for instance of class \linkS4class{LPCoopGameUtils}
#' @template author/JA
#' @name LPCoopGameUtils
#' @template param/LPBndsObjCoefs
#' @template param/LPRows
#' @template param/LPMatrix
#' @template param/LPObjDir
# @export
LPCoopGameUtils<-function(LPBndsObjCoefs=methods::new("LPBndsObjCoefs"),LPRows=methods::new("LPRows"),LPMatrix=methods::new("LPMatrix"),LPObjDir=GLP_MIN){
  retLPCoopGameUtils=methods::new("LPCoopGameUtils",LPBndsObjCoefs=LPBndsObjCoefs,LPRows=LPRows,LPMatrix=LPMatrix,LPObjDir=LPObjDir)
  return(retLPCoopGameUtils)
}



#' @title LPCoopGameUtils - Linear Programming utils for CoopGame
#' @description An S4 class representing a utility for basic linear programming functionalities (requiring glpkAPI) 
#' as often used in the Game Theory package CoopGame.
#' @include LPRows.R
#' @include LPBndsObjCoefs.R
#' @include LPMatrix.R
#' @import glpkAPI
#' @template author/JA
#' @template slot/LPBndsObjCoefs
#' @template slot/LPRows
#' @template slot/LPMatrix 
#' @template slot/LPObjDir 
#' @template slot/LP
#' @exportClass LPCoopGameUtils
setClass(
  "LPCoopGameUtils",
  representation(
                 LPBndsObjCoefs="LPBndsObjCoefs",
                 LPRows="LPRows",
                 LPMatrix="LPMatrix",
                 LP="glpkPtr",
                 LPObjDir="numeric"
  ),
  prototype = prototype(LPObjDir=GLP_MIN)
  
)


#' @title Method updateLPCoopGameUtils
#' @description This method updates the the instance of LPCoopGameUtils according to its set class attributes.
#' @rdname updateLPCoopGameUtils-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod updateLPCoopGameUtils
setGeneric(
  "updateLPCoopGameUtils",
  function(.Object){
    standardGeneric("updateLPCoopGameUtils")
  }
)

#' @rdname updateLPCoopGameUtils-methods
#' @aliases updateLPCoopGameUtils,LPCoopGameUtils-method
setMethod(
  "updateLPCoopGameUtils",
  signature = "LPCoopGameUtils",
  function(.Object){
    objLP<-.Object@LP
    N=getNumberOfRows(.Object)
    n=getNumberOfCols(.Object)
    ia <- rep(1:N, each = n)
    ja <- rep(1:(n), N)
    if(isNULLpointerGLPK(objLP)){
      objLP<-initProbGLPK()
      
    }else{
      eraseProbGLPK(objLP)
    }
    addRowsGLPK(objLP, N)
    addColsGLPK(objLP, n)
    setRowsBndsGLPK(objLP, 1:N,    .Object@LPRows@rlb,
                    .Object@LPRows@rub, .Object@LPRows@rtype)
    setColsBndsObjCoefsGLPK(objLP, 1:(n), .Object@LPBndsObjCoefs@clb,.Object@LPBndsObjCoefs@cub,.Object@LPBndsObjCoefs@obj,.Object@LPBndsObjCoefs@ctype)
    setObjDirGLPK(objLP,.Object@LPObjDir)

    loadMatrixGLPK(objLP, N * (n), ia, ja, as.vector(t(.Object@LPMatrix@matrix)))
    eval.parent(substitute(.Object@LP<-objLP))
  }
)


#' @title Method updateLPCoopGameUtils
#' @description This methods changes the optimization flag of an LP to minimization
#' @rdname changeLPCoopGameUtilsObjDirToMin-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod changeLPCoopGameUtilsObjDirToMin

setGeneric(
  "changeLPCoopGameUtilsObjDirToMin",
  function(.Object){
    standardGeneric("changeLPCoopGameUtilsObjDirToMin")
  }
  
)

#' @rdname changeLPCoopGameUtilsObjDirToMin-methods
#' @aliases changeLPCoopGameUtilsObjDirToMin,LPCoopGameUtils-method
setMethod(
  "changeLPCoopGameUtilsObjDirToMin",
  signature="LPCoopGameUtils",
  definition=function(.Object){
    objDir=1
    eval.parent(substitute(.Object@LPObjDir<-objDir))
  }
)

#' @title Method changeLPCoopGameUtilsObjDirToMax
#' @description changeLPCoopGameUtilsObjDirToMax changes the optimization flag of an LP to maximization
#' @rdname changeLPCoopGameUtilsObjDirToMax-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod changeLPCoopGameUtilsObjDirToMax
setGeneric(
  "changeLPCoopGameUtilsObjDirToMax",
  function(.Object){
    standardGeneric("changeLPCoopGameUtilsObjDirToMax")
  }
  
)

#' @rdname changeLPCoopGameUtilsObjDirToMax-methods
#' @aliases changeLPCoopGameUtilsObjDirToMax,LPCoopGameUtils-method
setMethod(
  "changeLPCoopGameUtilsObjDirToMax",
  signature="LPCoopGameUtils",
  definition=function(.Object){
    objDir=2
    eval.parent(substitute(.Object@LPObjDir<-objDir))
  }
)

#' @title Method solveLP
#' @description solveLP solves the specified linear program.
#' @rdname solveLP-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/enableTermOutLP
#' @exportMethod solveLP

setGeneric("solveLP",function(.Object,enableTermOutLP=TRUE){standardGeneric("solveLP")})

#' @rdname solveLP-methods
#' @aliases solveLP,LPCoopGameUtils-method
setMethod(
  "solveLP",
  signature = "LPCoopGameUtils",
  definition = function(.Object,enableTermOutLP=TRUE){
    if(!isNULLpointerGLPK(.Object@LP)){
      objLP=.Object@LP
      termOutGLPK(ifelse(enableTermOutLP, GLP_ON, GLP_OFF))
      # GLP_SF_2N<-0x20
      # scaleProbGLPK(objLP,GLP_SF_2N)
      solveSimplexGLPK(objLP)
      eval.parent(substitute(.Object@LP<-objLP))
    }
  }
)



#' @title Method getLPBndsObjCoefs
#' @description This method gets the instance of class \linkS4class{LPBndsObjCoefs}. 
#' @rdname getLPBndsObjCoefs-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getLPBndsObjCoefs
setGeneric("getLPBndsObjCoefs",function(.Object){standardGeneric("getLPBndsObjCoefs")})

#' @rdname getLPBndsObjCoefs-methods
#' @aliases getLPBndsObjCoefs,LPCoopGameUtils-method
setMethod(
  "getLPBndsObjCoefs",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(.Object@LPBndsObjCoefs)
  }
)


#' @title Method setLPBndsObjCoefs
#' @description This method sets the bounds of the linear program's objective function. 
#' @rdname setLPBndsObjCoefs-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/value
#' @exportMethod setLPBndsObjCoefs<-
setGeneric("setLPBndsObjCoefs<-",function(.Object,value){standardGeneric("setLPBndsObjCoefs<-")})

#' @rdname setLPBndsObjCoefs-methods
#' @aliases setLPBndsObjCoefs,LPCoopGameUtils-method
setReplaceMethod(
  f="setLPBndsObjCoefs",
  signature="LPCoopGameUtils",
  definition=function(.Object,value){
    .Object@LPBndsObjCoefs<-value
    return(.Object)
  }
)


#' @title Method getLPRows
#' @description This method gets the instance of class \linkS4class{LPRows}. 
#' @rdname getLPRows-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getLPRows
setGeneric("getLPRows",function(.Object){standardGeneric("getLPRows")})

#' @rdname getLPRows-methods
#' @aliases getLPRows,LPCoopGameUtils-method
setMethod(
  "getLPRows",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(.Object@LPRows)
  }
)


#' @title Method setLPRows
#' @description This method sets the instance of class \linkS4class{LPRows}. 
#' @rdname setLPRows-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/value
#' @exportMethod setLPRows<-
setGeneric("setLPRows<-",function(.Object,value){standardGeneric("setLPRows<-")})

#' @rdname setLPRows-methods
#' @aliases setLPRows,LPCoopGameUtils-method
setReplaceMethod(
  f="setLPRows",
  signature="LPCoopGameUtils",
  definition=function(.Object,value){
    .Object@LPRows<-value
    return(.Object)
  }
)


#' @title Method getLPMatrix
#' @description This method gets the instance of class \linkS4class{LPMatrix}. 
#' @rdname getLPMatrix-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getLPMatrix
setGeneric("getLPMatrix",function(.Object){standardGeneric("getLPMatrix")})

#' @rdname getLPMatrix-methods
#' @aliases getLPMatrix,LPCoopGameUtils-method
setMethod(
  "getLPMatrix",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(.Object@LPMatrix)
  }
)


#' @title Method setLPMatrix
#' @description This method sets the instance of class \linkS4class{LPMatrix}. 
#' @rdname setLPMatrix-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/value
#' @exportMethod setLPMatrix<-
setGeneric("setLPMatrix<-",function(.Object,value){standardGeneric("setLPMatrix<-")})

#' @rdname setLPMatrix-methods
#' @aliases setLPMatrix<-,LPCoopGameUtils-method
setReplaceMethod(
  f="setLPMatrix",
  signature="LPCoopGameUtils",
  definition=function(.Object,value){
    .Object@LPMatrix<-value
    return(.Object)
  }
)

 
#' @title Method getNumberOfCols
#' @description This method gets the number of columns of \linkS4class{LPMatrix}. 
#' @rdname getNumberOfCols-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getNumberOfCols
setGeneric("getNumberOfCols",function(.Object){standardGeneric("getNumberOfCols")})

#' @rdname getNumberOfCols-methods
#' @aliases getNumberOfCols,LPCoopGameUtils-method
setMethod(
  "getNumberOfCols",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(ncol(.Object@LPMatrix@matrix))
  }
)


#' @title Method getNumberOfRows
#' @description This method gets the number of rows of \linkS4class{LPMatrix}. 
#' @rdname getNumberOfRows-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getNumberOfCols
setGeneric("getNumberOfRows",function(.Object){standardGeneric("getNumberOfRows")})

#' @rdname getNumberOfRows-methods
#' @aliases getNumberOfRows,LPCoopGameUtils-method
setMethod(
  "getNumberOfRows",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(nrow(.Object@LPMatrix@matrix))
  }
)



#' @title Method getLPObjVal
#' @description This method gets the objective value of the solved linear program. 
#' @rdname getLPObjVal-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getLPObjVal
setGeneric("getLPObjVal",function(.Object){standardGeneric("getLPObjVal")})

#' @rdname getLPObjVal-methods
#' @aliases getLPObjVal,LPCoopGameUtils-method
setMethod(
  "getLPObjVal",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(getObjValGLPK(.Object@LP))
  }
)


#' @title Method getLPDualSolution
#' @description This method gets the dual solution value of the solved lineare program. 
#' @rdname getLPDualSolution-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getLPDualSolution
setGeneric("getLPDualSolution",function(.Object){standardGeneric("getLPDualSolution")})

#' @rdname getLPDualSolution-methods
#' @aliases getLPDualSolution,LPCoopGameUtils-method
setMethod(
  "getLPDualSolution",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(getRowsDualGLPK(.Object@LP))
  }
)


#' @title Method getLPPrimalSolution
#' @description This method gets the primal solution value of the solved lineare program. 
#' @rdname getLPPrimalSolution-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getLPPrimalSolution
setGeneric("getLPPrimalSolution",function(.Object){standardGeneric("getLPPrimalSolution")})

#' @rdname getLPPrimalSolution-methods
#' @aliases getLPPrimalSolution,LPCoopGameUtils-method
setMethod(
  "getLPPrimalSolution",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(getColsPrimGLPK(.Object@LP))
  }
)


#' @title Method getPrimStat
#' @description This method gets the primal solution state of the solved lineare program. 
#' @rdname getPrimStat-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getPrimStat
setGeneric("getPrimStat",function(.Object){standardGeneric("getPrimStat")})

#' @rdname getPrimStat-methods
#' @aliases getPrimStat,LPCoopGameUtils-method
setMethod(
  "getPrimStat",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(getPrimStatGLPK(.Object@LP))
  }
)




#' @title Method isLPFeasible
#' @description This method if there is a feasible solution for the solved lineare program. 
#' @rdname isLPFeasible-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod isLPFeasible
setGeneric("isLPFeasible",function(.Object){standardGeneric("isLPFeasible")})


#' @rdname isLPFeasible-methods
#' @aliases isLPFeasible,LPCoopGameUtils-method
setMethod(
  "isLPFeasible",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    boolRetVal=FALSE
    primStat<-getPrimStat(.Object);
    if(primStat==GLP_FEAS){
      boolRetVal=TRUE
    }
    
    return(boolRetVal)
  }
)

#' @rdname setRub-methods
#' @aliases setRub<-,LPCoopGameUtils-method
setReplaceMethod(
  f="setRub",
  signature="LPCoopGameUtils",
  definition=function(.Object,value){
    setRub(.Object@LPRows)<-value
    return(.Object)
  }
)

#' @rdname setRlb-methods
#' @aliases setRlb<-,LPCoopGameUtils-method
setReplaceMethod(
  f="setRlb",
  signature="LPCoopGameUtils",
  definition=function(.Object,value){
    setRlb(.Object@LPRows)<-value
    return(.Object)
  }
)


#' @rdname setRtype-methods
#' @aliases setRtype<-,LPCoopGameUtils-method
setReplaceMethod(
  f="setRtype",
  signature="LPCoopGameUtils",
  definition=function(.Object,value){
    setRtype(.Object@LPRows)<-value
    return(.Object)
  }
)




#' @rdname getRlb-methods
#' @aliases getRlb,LPCoopGameUtils-method
setMethod(
  "getRlb",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(getRlb(.Object@LPRows))
  }
)

#' @rdname getRub-methods
#' @aliases getRub,LPCoopGameUtils-method
setMethod(
  "getRub",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(getRub(.Object@LPRows))
  }
)

#' @rdname getRtype-methods
#' @aliases getRtype,LPCoopGameUtils-method
setMethod(
  "getRtype",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(getRtype(.Object@LPRows))
  }
)



#' @rdname getMatrix-methods
#' @aliases getMatrix,LPCoopGameUtils-method
setMethod(
  "getMatrix",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(getMatrix(.Object@LPMatrix))
  }
)


#' @rdname setMatrix-methods
#' @aliases setMatrix<-,LPCoopGameUtils-method
setReplaceMethod(
  f="setMatrix",
  signature="LPCoopGameUtils",
  definition=function(.Object,value){
    setMatrix(.Object@LPMatrix)<-value
    return(.Object)
  }
)


#' @rdname getCub-methods
#' @aliases getCub,LPCoopGameUtils-method
setMethod(
  "getCub",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(getCub(.Object@LPBndsObjCoefs))
  }
)


#' @rdname getClb-methods
#' @aliases getClb,LPCoopGameUtils-method
setMethod(
  "getClb",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(getClb(.Object@LPBndsObjCoefs))
  }
)

#' @rdname getCtype-methods
#' @aliases getCtype,LPCoopGameUtils-method
setMethod(
  "getCtype",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(getCtype(.Object@LPBndsObjCoefs))
  }
)

#' @rdname getObj-methods
#' @aliases getObj,LPCoopGameUtils-method
setMethod(
  "getObj",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(getObj(.Object@LPBndsObjCoefs))
  }
)

#' @rdname setCub-methods
#' @aliases setCub<-,LPCoopGameUtils-method
setReplaceMethod(
  f="setCub",
  signature="LPCoopGameUtils",
  definition=function(.Object,value){
    setCub(.Object@LPBndsObjCoefs)<-value
    return(.Object)
  }
)

#' @rdname setClb-methods
#' @aliases setClb<-,LPCoopGameUtils-method
setReplaceMethod(
  f="setClb",
  signature="LPCoopGameUtils",
  definition=function(.Object,value){
    setClb(.Object@LPBndsObjCoefs)<-value
    return(.Object)
  }
)

#' @rdname setCtype-methods
#' @aliases setCtype<-,LPCoopGameUtils-method
setReplaceMethod(
  f="setCtype",
  signature="LPCoopGameUtils",
  definition=function(.Object,value){
    setCtype(.Object@LPBndsObjCoefs)<-value
    return(.Object)
  }
)


#' @rdname setObj-methods
#' @aliases setObj<-,LPCoopGameUtils-method
setReplaceMethod(
  f="setObj",
  signature="LPCoopGameUtils",
  definition=function(.Object,value){
    setObj(.Object@LPBndsObjCoefs)<-value
    return(.Object)
  }
)

#' @rdname setMatrixLastCol-methods
#' @aliases setMatrixLastCol<-,LPCoopGameUtils-method
setReplaceMethod(
  f="setMatrixLastCol",
  signature="LPCoopGameUtils",
  definition=function(.Object,value){
    setMatrixLastCol(.Object@LPMatrix)<-value
    return(.Object)
  }
)

#' @rdname getMatrixLastCol-methods
#' @aliases getMatrixLastCol,LPCoopGameUtils-method
setMethod(
  "getMatrixLastCol",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(getMatrixLastCol(.Object@LPMatrix))
  }
)



#' @title Method getLPObjDir
#' @description This method if minimization or maximization is set at solving the lineare program. 
#' @rdname getLPObjDir-methods
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @exportMethod getLPObjDir
setGeneric("getLPObjDir",function(.Object){standardGeneric("getLPObjDir")})

#' @rdname getLPObjDir-methods
#' @aliases getLPObjDir,LPCoopGameUtils-method
setMethod(
  "getLPObjDir",
  signature = "LPCoopGameUtils",
  definition = function(.Object){
    return(getRowsDualGLPK(.Object@LPObjDir))
  }
)

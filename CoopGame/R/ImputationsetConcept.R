#' @name imputationsetVertices
#' @title imputationsetVertices
#' @description Calculates the imputation set for given game vector A.
#' @aliases imputationsetVertices
#' @import rcdd
#' @export imputationsetVertices
#' @template author/MM 
#' @template author/FM
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 20
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 674 ff.
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 278
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P p. 407
#' @inheritParams CoopGameBaseParams
#' @return rows of the matrix are the vertices of the imputation set
#' @examples
#' library(CoopGame)
#' A = c(2, 4, 5, 18, 24, 9, 24)
#' 
#' imputationsetVertices(A)
#' 
#' #      [,1] [,2] [,3]
#' #[1,]   15    4    5
#' #[2,]    2   17    5
#' #[3,]    2    4   18
imputationsetVertices<-function(A){
  ic=ImputationsetConcept(A)
  payoffSpace=calculateSetSolution(ic)
  return(payoffSpace@VRepMatrix)
}


logicImputationsetVertices <- function(A) { 
  #Checks, if the game vector is correct
  numberOfPlayers = getNumberOfPlayers(A)
  
  # Checks if imputation set is not empty
  if(isEssentialGame(A)== TRUE || isDegenerateGame(A))
  {
    #Calculates the matrices and vectors for the function makeH of rcdd
    matrixA1 = diag(rep(-1,numberOfPlayers))
    matrixA2 = matrix(rep(-1,numberOfPlayers),nrow = 1)
    vectorB1 = -A[1:numberOfPlayers]
    vectorB2 = -A[length(A)]
    
    #Utilize the R-Package rcdd
    hRepresentation = makeH(matrixA1,vectorB1,matrixA2,vectorB2)
    vRepresentation = scdd(hRepresentation)
    
    #Transform the V-Representation into a matrix
    VectorCounter = length(vRepresentation[[1]]) / (numberOfPlayers + 2)
    OutcomeVector = vRepresentation[[1]][(VectorCounter * 2 + 1):(length(vRepresentation[[1]]))]
    OutcomeMatrix = matrix(OutcomeVector, VectorCounter, numberOfPlayers)
  }
  else
  {
    print("The imputation set is empty")
    OutcomeMatrix=(matrix(nrow=0,ncol=numberOfPlayers))
  }
  return (OutcomeMatrix)
}

#' @title ImputationsetConcept - Virtual class for point valued solution concepts
#' @noRd
#' @include SetSolutionConcept.R
#' #@exportClass ImputationsetConcept

setClass(
  "ImputationsetConcept",
  contains = "SetSolutionConcept"
)

#' @title Constructor for ImputationsetConcept
#' @noRd
#' @template author/JA
#' @name ImputationsetConcept
#' @include PayoffSpace.R
#' #@export
ImputationsetConcept<-function(A){
  retImputationsetConcept=methods::new("ImputationsetConcept",A)
  return(retImputationsetConcept)
}
#' @rdname getSetSolutionVertices-methods
#' @aliases getSetSolutionVertices,ImputationsetConcept-method
setMethod(
  "getSetSolutionVertices",
  signature="ImputationsetConcept",
  definition=function(.Object){
    A<-.Object@A
    VRep=(logicImputationsetVertices(A))
    return(VRep)
  }
)


#' @name belongsToImputationset
#' @title belongsToImputationset for TU game with n players
#' @description Checks if the point belongs to the imputation set
#' @aliases belongsToImputationset
#' @export belongsToImputationset
#' @template author/JA
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 20
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 674 ff.
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 278
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P p. 407
#' @template param/x
#' @inheritParams CoopGameBaseParams
#' @return \code{TRUE} for a point belonging to the imputation set and \code{FALSE} otherwise
#' @examples
#' library(CoopGame)
#' A=c(0,1,2,3,4,5,6)
#'
#' #Point belongs to imputation set:
#' belongsToImputationset(x=c(1.5,1,3.5),A)
#'
#' #Point does not belong to imputation set:
#' belongsToImputationset(x=c(2.05,2,2),A)

belongsToImputationset<-function(x,A){
  isv=imputationsetVertices(A) 
  ps=PayoffSpace(VRepMatrix=isv)
  return(isElementOfPayoffSpace(ps,x=x))
}


#' @name drawImputationset
#' @title drawImputationset for 3 or 4 players
#' @description drawImputationset draws the imputation set for 3 or 4 players.
#' @aliases drawImputationset
#' @export drawImputationset
#' @template author/JA
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 20
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 674 ff.
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 278
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P p. 407
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' library(CoopGame)
#' A=c(0,1,2,3,4,5,6)
#' drawImputationset(A)

drawImputationset<-function(A, label=TRUE){
  A=GameVector(A)
  imputationsetDraw(A, label)
}

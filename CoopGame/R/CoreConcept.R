#' @name coreVertices
#' @title coreVertices
#' @description Calculates the core for given game vector A.
#' @aliases coreVertices
#' @import rcdd
#' @importFrom grDevices chull
#' @export coreVertices
#' @template author/FM
#' @template cites/GILLIES_1953
#' @template cites/AUMANN_1961
#' @templateVar AUMANN_1961_P pp. 539--552
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 27
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 686 ff.
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 257 ff.
#' @inheritParams CoopGameBaseParams
#' @return rows of the matrix are the vertices of the core
#' @examples
#' library(CoopGame)
#' #In the following case the core consists of a single point
#' A1 = c(0,1,2,3,4,5,6)
#' coreVertices(A1)
#' #     [,1] [,2] [,3]
#' #[1,]    1    2    3
#' 
#' #Users may also want to try the following game vectors:
#' #A2 = c(0,0,0,60,80,100,135)
#' #A3 = c(5,2,4,7,15,15,15,15,15,15,20,20,20,20,35)
#' #A4 = c(0,0,0,0,0,5,5,5,5,5,5,5,5,5,5,15,15,15,15,15,15,15,15,15,15,30,30,30,30,30,60)
coreVertices<-function(A){
  wv=CoreConcept(A)
  payoffSpace=calculateSetSolution(wv)
  return(payoffSpace@VRepMatrix)
}


logicCoreVertices <- function(A){ 
  numberOfPlayers = getNumberOfPlayers(A)
  retVal=NULL
    if(isEssentialGame(A) == TRUE){
      vectorA1 = c()
      
      #Calculate the matrixA1 for the function makeH
      vectorRechnung = c(rep(0,(numberOfPlayers - 1)),c(1))
      vectorA1 = c(1, rep(0,(numberOfPlayers - 1)))
      b = TRUE

      
      #Build the matrices and vectors for the function makeH
      # matrixA1b = -t(matrix(vectorA1,numberOfPlayers,(length(A) - 1)))
      matrixA1 = -createBitMatrix(n=numberOfPlayers)[-length(A),-(numberOfPlayers+1)]#-t(matrix(vectorA1,numberOfPlayers,(length(A) - 1)))
      matrixA2 = -matrix(rep(1,numberOfPlayers),1,numberOfPlayers)
      vectorB1 = -A[1:(length(A)-1)]
      vectorB2 = -A[length(A)]
      
      # Utilize the R-Package rcdd
      hRepresentation = makeH(matrixA1,vectorB1,matrixA2,vectorB2)
      vRepresentation = scdd(hRepresentation)
      if (nrow(vRepresentation$output) >= 2)
      {
        vRepresentation = redundant(vRepresentation$output, representation = "V")
      }
      
      #Transform the V-Representation into a matrix
      VectorCounter = length(vRepresentation[[1]]) / (numberOfPlayers + 2)
      OutcomeVector = vRepresentation[[1]][(VectorCounter * 2 + 1):(length(vRepresentation[[1]]))]
      ResultMatrix = matrix(OutcomeVector, VectorCounter, numberOfPlayers)   
      
      if(numberOfPlayers == 3)
      {
      sequencevector = grDevices::chull(ResultMatrix)
      OutcomeMatrix = ResultMatrix[sequencevector,,drop = FALSE]
      }
      else
      {
        OutcomeMatrix = ResultMatrix
      }
      
      retVal=OutcomeMatrix
    }
    else
    {
      print("The core is empty")
      retVal=matrix(ncol = numberOfPlayers,nrow = 0)
    }
    return(retVal)
}

#' @title CoreConcept - Virtual class for point valued solution concepts
#' @noRd
#' @include SetSolutionConcept.R
#' #@exportClass CoreConcept

setClass(
  "CoreConcept",
  contains = "SetSolutionConcept"
)

#' @title Constructor for CoreConcept
#' @noRd
#' @description Constructor for CoreConcept
#' @template author/JA
#' @name CoreConcept
#' @include PayoffSpace.R
#' #@export
CoreConcept<-function(A){
  retCoreConcept=methods::new("CoreConcept",A)
  return(retCoreConcept)
}

#' @rdname getSetSolutionVertices-methods
#' @aliases getSetSolutionVertices,CoreConcept-method
setMethod(
  "getSetSolutionVertices",
  signature="CoreConcept",
  definition=function(.Object){
    A<-.Object@A
    VRep=(logicCoreVertices(A))
    return(VRep)
  }
)

#' @name belongsToCore
#' @title belongsToCore for a TU Game with n players
#' @description Checks if a given point is in the core
#' @export belongsToCore
#' @template author/FM
#' @template cites/GILLIES_1953
#' @template cites/AUMANN_1961
#' @templateVar AUMANN_1961_P pp. 539--552
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 27
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 686 ff.
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 257 ff.
#' @template param/A
#' @template param/x
#' @return \code{TRUE} for a point belonging to the core and \code{FALSE} otherwise
#' @examples
#' library(CoopGame)
#' A = c(0,1,2,3,4,5,6)
#' belongsToCore(c(1,2,3),A)
#' #[1] TRUE

belongsToCore<-function(x,A){
  cv=coreVertices(A) 
  ps=PayoffSpace(VRepMatrix=cv)
  return(isElementOfPayoffSpace(ps,x=x))
}


#' @name drawCore
#' @title drawCore for n players
#' @description drawCore draws the core set for 3 or 4 players.
#' @aliases drawCore
#' @export drawCore
#' @template author/JA
#' @template cites/GILLIES_1953
#' @template cites/AUMANN_1961
#' @templateVar AUMANN_1961_P pp. 539--552
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 27
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 686 ff.
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 257 ff.
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @return draw core for given game vector with 3 or 4 players
#' @examples
#' library(CoopGame)
#' A <- c(0,0,0,3,3,3,6)
#' drawCore(A)
#' 
drawCore<-function(A,holdOn=FALSE, colour = "red" , label=FALSE, name = "Core"){
  A=GameVector(A)
  co=coreVertices(A);
  visualize(A, pointsToDraw=co, holdOn=holdOn, colour = colour , label=label, name = name)
}
#' @name reasonableSetVertices
#' @title reasonableSetVertices
#' @description Calculates the reasonable set for given game vector A.
#' @aliases reasonableSetVertices
#' @import rcdd
#' @importFrom grDevices chull
#' @export reasonableSetVertices
#' @template author/JS
#' @template cites/MILNOR_1953
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 43 ff.
#' @template cites/GERARD_VARET_ET_ZAMIR_1987
#' @templateVar GERARD_VARET_ET_ZAMIR_1987_P pp. 123--143
#' @inheritParams CoopGameBaseParams
#' @return rows of the matrix are the vertices of the reasonable set
#' @examples
#' library(CoopGame)
#' A <- c(0,0,0,3,3,3,6)
#' reasonableSetVertices(A)
#' #       [,1] [,2] [,3]
#' # [1,]    3    0    3
#' # [2,]    0    3    3
#' # [3,]    3    3    0
reasonableSetVertices<-function(A){
  wv=ReasonableSetConcept(A)
  payoffSpace=calculateSetSolution(wv)
  return(payoffSpace@VRepMatrix)
}


logicReasonableSetVertices <- function(A){ 
  numberOfPlayers = getNumberOfPlayers(A)
    if(isEssentialGame(A) == TRUE){
      m <- A[1:numberOfPlayers]
      M <- rep(0,numberOfPlayers)
      marginalContributions <- getMarginalContributions(A)$marginal_values
      for (i in 1:numberOfPlayers)
      {
        M[i] <- max(marginalContributions[,i])
      }
      matrixA1_help1 = diag(rep(-1,numberOfPlayers))
      matrixA1_help2 = diag(rep(1,numberOfPlayers))
      matrixA1 <- rbind(matrixA1_help1, matrixA1_help2)
      matrixA2 = matrix(rep(-1,numberOfPlayers),nrow = 1)
      vectorB1 <- c(-m,M)
      vectorB2 = -A[length(A)]
      
      # Utilize the R-Package rcdd
      hRepresentation = makeH(matrixA1,vectorB1,matrixA2,vectorB2)
      vRepresentation = scdd(hRepresentation)
      
      #Transform the V-Representation into a matrix
      VectorCounter = length(vRepresentation[[1]]) / (numberOfPlayers + 2)
      OutcomeVector = vRepresentation[[1]][(VectorCounter * 2 + 1):(length(vRepresentation[[1]]))]
      ResultMatrix = matrix(OutcomeVector, VectorCounter, numberOfPlayers)   
      
      if(numberOfPlayers == 3)
      {
        sequencevector = grDevices::chull(ResultMatrix)
        OutcomeMatrix = ResultMatrix[sequencevector,,drop=FALSE]
      }
      else
      {
        OutcomeMatrix = ResultMatrix
      }
    }
    else
    {
      print("The reasonable set is empty")
      OutcomeMatrix = matrix(ncol = numberOfPlayers,nrow = 0)
    }
  return(OutcomeMatrix)
}

#' @title ReasonableSetConcept - class for reasonable set concept
#' @noRd
#' @include SetSolutionConcept.R
#' #@exportClass ReasonableSetConcept

setClass(
  "ReasonableSetConcept",
  contains = "SetSolutionConcept"
)

#' @title Constructor for ReasonableSetConcept
#' @noRd
#' @template author/JS
#' @name ReasonableSetConcept
#' @include PayoffSpace.R
#' #@export
ReasonableSetConcept<-function(A){
  retReasonableSetConcept=methods::new("ReasonableSetConcept",A)
  return(retReasonableSetConcept)
}

#' @rdname getSetSolutionVertices-methods
#' @aliases getSetSolutionVertices,ReasonableSetConcept-method
setMethod(
  "getSetSolutionVertices",
  signature="ReasonableSetConcept",
  definition=function(.Object){
    A<-.Object@A
    VRep=(logicReasonableSetVertices(A))
    return(VRep)
  }
)


#' @name belongsToReasonableSet
#' @title belongsToReasonableSet for every number of players
#' @description Checks, if the point is in the reasonable set
#' @aliases belongsToReasonableSet
#' @import geometry
#' @import rcdd
#' @export belongsToReasonableSet
#' @template author/JS
#' @template cites/MILNOR_1953
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 43 ff.
#' @template cites/GERARD_VARET_ET_ZAMIR_1987
#' @templateVar GERARD_VARET_ET_ZAMIR_1987_P pp. 123--143
#' @template param/x
#' @inheritParams CoopGameBaseParams
#' @return \code{TRUE} if point belongs to reasonable set, \code{FALSE} otherwise
#' @examples
#' library(CoopGame)
#' A <- c(0,0,0,3,3,3,6)
#' belongsToReasonableSet(x=c(2,2,2),A)
#' #[1] TRUE
#' belongsToReasonableSet(x=c(1,2,4),A)
#' #[1] FALSE

belongsToReasonableSet<-function(x,A){
  ccv=reasonableSetVertices(A) 
  ps=PayoffSpace(VRepMatrix=ccv)
  return(isElementOfPayoffSpace(ps,x=x))
}

#' @name drawReasonableSet
#' @title drawReasonableSet for n players
#' @description drawReasonableSet draws the Reasonable Set for 3 or 4 players.
#' @aliases drawReasonableSet
#' @export drawReasonableSet
#' @import rgl
#' @template author/JS
#' @template cites/MILNOR_1953
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 43 ff.
#' @template cites/GERARD_VARET_ET_ZAMIR_1987
#' @templateVar GERARD_VARET_ET_ZAMIR_1987_P pp. 123--143
#' @inheritParams  CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' library(CoopGame)
#' A <- c(0,0,0,3,3,3,6)
#' drawReasonableSet(A)
drawReasonableSet<-function(A,holdOn=FALSE, colour = NA , label=FALSE, name = "Reasonable Set"){
  A=GameVector(A)
  co=reasonableSetVertices(A);
  visualize(A, pointsToDraw=co, holdOn=holdOn, colour = colour , label=label, name = name)
}

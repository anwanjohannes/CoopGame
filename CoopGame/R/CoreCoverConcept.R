#' @name coreCoverVertices
#' @title coreCoverVertices
#' @description Calculates the core cover for given game vector A.
#' @aliases coreCoverVertices
#' @import rcdd
#' @importFrom grDevices chull
#' @export coreCoverVertices
#' @template author/JS
#' @template cites/TIJS_LIPPERTS_1982
#' @templateVar TIJS_LIPPERTS_1982_P pp. 27--37
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21 ff.
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 45 ff.
#' @inheritParams CoopGameBaseParams
#' @return rows of the matrix are the vertices of the core cover
#' @examples
#' library(CoopGame)
#' A <- c(0,0,0,3,3,3,6)
#' coreCoverVertices(A)
#' #       [,1] [,2] [,3]
#' # [1,]    3    0    3
#' # [2,]    0    3    3
#' # [3,]    3    3    0
coreCoverVertices<-function(A){
  wv=CoreCoverConcept(A)
  payoffSpace=calculateSetSolution(wv)
  return(payoffSpace@VRepMatrix)
}


logicCoreCoverVertices <- function(A){ 
  numberOfPlayers = getNumberOfPlayers(A)
    if(isEssentialGame(A) == TRUE){
      M <- getUtopiaPayoff(A)
      m <- getMinimalRights(A)
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
      print("The core cover is empty")
      OutcomeMatrix = matrix(ncol = numberOfPlayers,nrow = 0)
    }
  return(OutcomeMatrix)
}

#' @title CoreCoverConcept - class for core cover concept
#' @noRd
#' @include SetSolutionConcept.R
#' #@exportClass CoreCoverConcept

setClass(
  "CoreCoverConcept",
  contains = "SetSolutionConcept"
)

#' @title Constructor for CoreCoverConcept
#' @noRd
#' @template author/JA
#' @name CoreCoverConcept
#' @include PayoffSpace.R
#' #@export
CoreCoverConcept<-function(A){
  retCoreCoverConcept=methods::new("CoreCoverConcept",A)
  return(retCoreCoverConcept)
}

#' @rdname getSetSolutionVertices-methods
#' @aliases getSetSolutionVertices,CoreCoverConcept-method
setMethod(
  "getSetSolutionVertices",
  signature="CoreCoverConcept",
  definition=function(.Object){
    A<-.Object@A
    VRep=(logicCoreCoverVertices(A))
    return(VRep)
  }
)


#' @name belongsToCoreCover
#' @title belongsToCoreCover for every number of players
#' @description Checks, if the point is in the core cover
#' @aliases belongsToCoreCover
#' @import geometry
#' @import rcdd
#' @export belongsToCoreCover
#' @template author/JA
#' @template cites/TIJS_LIPPERTS_1982
#' @templateVar TIJS_LIPPERTS_1982_P pp. 27--37
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21 ff.
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 45 ff.
#' @template param/x
#' @inheritParams CoopGameBaseParams
#' @return \code{TRUE} if point belongs to core cover, \code{FALSE} otherwise
#' @examples
#' library(CoopGame)
#' A <- c(0,0,0,3,3,3,6)
#' belongsToCoreCover(x=c(2,2,2),A)
#' #[1] TRUE
#' belongsToCoreCover(x=c(1,2,4),A)
#' #[1] FALSE

belongsToCoreCover<-function(x,A){
  ccv=coreCoverVertices(A) 
  ps=PayoffSpace(VRepMatrix=ccv)
  return(isElementOfPayoffSpace(ps,x=x))
}

#' @name drawCoreCover
#' @title drawCoreCover for n players
#' @description drawCoreCover draws the Core Cover for 3 or 4 players.
#' @aliases drawCoreCover
#' @export drawCoreCover
#' @import rgl
#' @template author/JA
#' @template cites/TIJS_LIPPERTS_1982
#' @templateVar TIJS_LIPPERTS_1982_P pp. 27--37
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21 ff.
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 45 ff.
#' @inheritParams  CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' library(CoopGame)
#' A <- c(0,0,0,3,3,3,6)
#' drawCoreCover(A)
drawCoreCover<-function(A,holdOn=FALSE, colour = NA , label=FALSE, name = "Core Cover"){
  A=GameVector(A)
  co=coreCoverVertices(A);
  visualize(A, pointsToDraw=co, holdOn=holdOn, colour = colour , label=label, name = name)
}

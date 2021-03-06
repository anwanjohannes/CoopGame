#' @name webersetVertices
#' @title webersetVertices
#' @description Calculates the Weber Set for given game vector A.
#' @aliases webersetVertices
#' @importFrom grDevices chull
#' @export webersetVertices
#' @template author/AM
#' @template author/FM
#' @template cites/WEBER_1988
#' @templateVar WEBER_1988_P pp. 101--119
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 327 ff.
#' @inheritParams CoopGameBaseParams
#' @return rows of the matrix are the vertices of the Weber Set
#' @examples
#' #Example 1: A 3-player TU game (with a Weber Set with 6 vertices)
#' library(CoopGame)
#' A = c(0,1,2,3,4,5,6)
#' webersetVertices(A)
#'
#' #Example 2: A 4 player TU game (with a Weber Set with 14 vertices)
#' library(CoopGame)
#' A = c(5,2,4,7,15,15,15,15,15,15,20,20,20,20,35)
#' webersetVertices(A)
#' 
webersetVertices<-function(A){
  wv=WebersetConcept(A)
  payoffSpace=calculateSetSolution(wv)
  return(payoffSpace@VRepMatrix)
}

logicWebersetVertices <- function(A) {
  numberOfPlayers = getNumberOfPlayers(A)
  OutcomeMatrix = matrix(ncol = numberOfPlayers,nrow = 0)
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Vertices of Weber Set are only computed for nonnegative games.")
  }
  else if (!isMonotonicGame(A)){
    print("Game is not monotonic. Vertices of Weber Set are only computed for monotonic games.")
  }
  else
  {
    #getMarginalContributions gives the marginal vectors
    ResultList = getMarginalContributions(A)
    ResultMatrix = ResultList[[3]]
    
    if(numberOfPlayers == 3)
    {
      sequencevector = grDevices::chull(ResultMatrix)
      OutcomeMatrix = ResultMatrix[sequencevector,,drop = FALSE]
    }
    else
    {
      #Delete redundant points
      vRep = makeV(points = ResultMatrix)
      if (nrow(vRep) >= 2)
      {
        vRep = redundant(vRep, representation = "V")
      }
      
      VectorCounter = length(vRep[[1]]) / (numberOfPlayers + 2)
      OutcomeVector = vRep[[1]][(VectorCounter * 2 + 1):(length(vRep[[1]]))]
      ResultMatrix = matrix(OutcomeVector, VectorCounter, numberOfPlayers) 
      OutcomeMatrix = ResultMatrix
    }
  }
  return (OutcomeMatrix)
}

#' @title WebersetConcept - S4 class 
#' @noRd
#' @include SetSolutionConcept.R
#' #@exportClass WebersetConcept

setClass(
  "WebersetConcept",
  contains = "SetSolutionConcept"
)

#' @title Constructor for WebersetConcept
#' @noRd
#' @template author/JA
#' @name WebersetConcept
#' @include PayoffSpace.R
#' #@export
WebersetConcept<-function(A){
  retWebersetConcept=methods::new("WebersetConcept",A)
  return(retWebersetConcept)
}

#' @rdname getSetSolutionVertices-methods
#' @aliases getSetSolutionVertices,WebersetConcept-method
setMethod(
  "getSetSolutionVertices",
  signature="WebersetConcept",
  definition=function(.Object){
    A<-.Object@A
    VRep=(logicWebersetVertices(A))
    return(VRep)
  }
)


#' @name belongsToWeberset
#' @title belongsToWeberset for every number of players
#' @description Checks, if the point is in the Weber Set
#' @aliases belongsToWeberset
#' @export belongsToWeberset
#' @template author/JA
#' @template cites/WEBER_1988
#' @templateVar WEBER_1988_P pp. 101--119
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 327 ff.
#' @template param/x
#' @inheritParams CoopGameBaseParams
#' @return \code{TRUE} if point belongs to Weber Set and \code{FALSE} otherwise
#' @examples
#' library(CoopGame)
#' A=c(0,1,2,3,4,5,6)
#' 
#' #Point belongs to Weber Set:
#' belongsToWeberset(x=c(1.5,1,3.5),A)
#' 
#' #Point does not belong to Weber Set:
#' belongsToWeberset(x=c(2.05,2,2),A)

belongsToWeberset<-function(x,A){
  wv=webersetVertices(A) 
  ps=PayoffSpace(VRepMatrix=wv)
  return(isElementOfPayoffSpace(ps,x=x))
}


#' @name drawWeberset
#' @title drawWeberset for 3 or 4 players
#' @description drawWeberset draws the Weber Set for 3 or 4 players.
#' @aliases drawWeberset
#' @export drawWeberset
#' @template author/JA
#' @template cites/WEBER_1988
#' @templateVar WEBER_1988_P pp. 101--119
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 327 ff.
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' library(CoopGame)
#' A = c(0,1,2,3,4,5,6)
#' drawWeberset(A)
drawWeberset<-function(A,holdOn=FALSE, colour = NA , label=FALSE, name = "Weber Set"){
  A=GameVector(A)
  wv=webersetVertices(A);
  visualize(A, pointsToDraw=wv, holdOn=holdOn, colour = colour , label=label, name = name)
}
#' @title PayoffSpace
#' @description A class for calculations in payoff space
#' @include SetSolutionConcept.R
#' @slot VRepMatrix A matrix of vertices of a set solution concept
#' @exportClass PayoffSpace

setClass(
  "PayoffSpace",
  slots = c(VRepMatrix="matrix")
)

#' @title Constructor for AllocationPropertyPayoffSpace
#' @noRd
#' @description Constructor for AllocationPropertyPayoffSpace
#' @template author/JA
#' @name PayoffSpace
#' #@export
PayoffSpace<-function(VRepMatrix=NULL){
  retAP=methods::new("PayoffSpace",VRepMatrix=VRepMatrix)
  return(retAP)
}

setMethod(
  f="initialize",
  signature = "PayoffSpace",
  definition =  function(.Object, ...,VRepMatrix) {
    .Object@VRepMatrix=VRepMatrix
    .Object<-methods::callNextMethod(.Object, ...)
    methods::validObject(.Object)
    return(.Object)
  }
)



#' @title Method isElementOfPayoffSpace
#' @description This method checks if one allocation is element of payoff-space.
#' @rdname isElementOfPayoffSpace-methods
#' @name isElementOfPayoffSpace
#' @docType methods
#' @template author/JA
#' @template param/Object
#' @template param/x
#' @exportMethod isElementOfPayoffSpace

setGeneric(
  "isElementOfPayoffSpace",
  function(.Object,x="Allocation"){
    standardGeneric("isElementOfPayoffSpace")
  }
)


#' @rdname isElementOfPayoffSpace-methods
#' @aliases isElementOfPayoffSpace,PayoffSpace-method
setMethod(
  "isElementOfPayoffSpace",
  signature="PayoffSpace",
  definition=function(.Object,x){
    return(isElementOfConvexSet(.Object@VRepMatrix,x))
  }
)

#' @title isElementOfConvexSet
#' @description check if a point is included in a convex set
#' @name isElementOfConvexSet
#' @template author/FM
#' @template author/JA
#' @template author/NC
#' @template author/JS
#' @template param/VRepMatrix
#' @template param/x
#' @return If the point is included in a convex set return \code{TRUE}, else return \code{FALSE}
#' @export
isElementOfConvexSet <- function(VRepMatrix,x){
  pointRepMatrix = VRepMatrix
  # get number of rows of the above matrix ...
  numberOfPlayers=length(x)
  boolRetVal=FALSE
  numberOfVertices = nrow(pointRepMatrix)
  
  if(numberOfVertices!=0){
  
    if(numberOfPlayers == ncol(pointRepMatrix))
    {
      pointRepMatrix = rbind(pointRepMatrix,x)
      
      vRepresentation = makeV(points = pointRepMatrix)
      
      if(nrow(vRepresentation)>=2){
        vRepresentation = redundant(vRepresentation)
        newpos = vRepresentation$new.position
        redundancyIndices = vRepresentation$redundant
        if (length(redundancyIndices)==0)
        {
          boolRetVal = FALSE
        }
        else if (length(redundancyIndices)>=2)
        {
          boolRetVal = FALSE
        }
        else if (length(redundancyIndices)==1)
        {
          if (redundancyIndices[1]==numberOfVertices+1)
          {
            boolRetVal = TRUE
          }
          else
          {
            equality = all.equal(pointRepMatrix[redundancyIndices[1],], pointRepMatrix[numberOfVertices+1,])
            if(equality[1] == TRUE)
            {
              boolRetVal=TRUE
            }
            else
            {
              boolRetVal = FALSE
            }
          }
        }
      }
    }
  }else{
    print("Payoff space is empty.")
  }
  return(boolRetVal)
}



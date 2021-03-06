#' @title Allocation -  class for allocation
#' @description Class for allocation object. Constructor is provided by function \link{Allocation}.
#' @exportClass Allocation
setClass(
  "Allocation",
  contains = "numeric",
  validity = function(object){
     paramCheckResult=getEmptyParamCheckResult()
     stopOnInvalidAllocation(paramCheckResult,x=(object))
  }
)


#' @title Constructor for Allocation
#' @description Constructor for instance of \linkS4class{Allocation}.
#' @template author/JA
#' @name Allocation
#' @template param/x
#' @export Allocation
#' @examples 
#' #Allocation where player 1 receives 2, player 2 receives 5 and player 3 receives 8.
#' library(CoopGame)
#' x<-c(2,5,8)
#' Allocation(x)
#' #An object of class "Allocation"
#' #[1] 2 5 8
Allocation<-function(x){
  retAllocation=methods::new("Allocation",x)
  return(retAllocation)
}
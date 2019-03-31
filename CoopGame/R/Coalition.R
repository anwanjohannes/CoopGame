#' @title Coalition - Class for representing coalition
#' @description Class for coalition. Constructor is provided by function \link{Coalition}.
#' @exportClass Coalition

setClass(
  "Coalition",
  contains = "numeric",
  validity = function(object){
    paramCheckResult=getEmptyParamCheckResult()
    stopOnInvalidCoalitionS(paramCheckResult,S=(object))
  }
)



#' @title Constructor for Coalition
#' @description Constructor for instance of class \linkS4class{Coalition}
#' @template author/JA
#' @template param/S
#' @name Coalition
#' @export Coalition
#' @examples 
#' #Coalition S consising of player 3, 5 and 8
#' library(CoopGame)
#' Coalition(c(3,5,8))
#' #An object of class "Coalition"
#' #[1] 3 5 8
#' 
Coalition<-function(S){
  retNoP=methods::new("Coalition",S)
  methods::validObject(retNoP)
  return(retNoP)
}
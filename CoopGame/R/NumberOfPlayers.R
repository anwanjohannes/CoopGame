#' @title NumberOfPlayers - Class for representing number of players
#' @description Class which represents the number of players. Constructor is provided by function \link{NumberOfPlayers}.
#' @exportClass NumberOfPlayers

setClass(
  "NumberOfPlayers",
  contains = "numeric",
  validity = function(object){
    paramCheckResult=getEmptyParamCheckResult()
    stopOnInvalidNumberOfPlayers(paramCheckResult,n=(object))
  }
)



#' @title Constructor for Number Of Players
#' @description Constructor for instance of class \linkS4class{NumberOfPlayers}
#' @template author/JA
#' @template param/n
#' @name NumberOfPlayers
#' @export NumberOfPlayers
#' @examples 
#' #Defining number of players as 4
#' library(CoopGame)
#' NumberOfPlayers(4)
#' #An object of class "NumberOfPlayers"
#' #[1] 4
NumberOfPlayers<-function(n){
  retNoP=methods::new("NumberOfPlayers",n)
  methods::validObject(retNoP)
  return(retNoP)
}
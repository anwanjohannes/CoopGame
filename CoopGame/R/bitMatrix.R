#' @title BitMatrix class
#' @template param/n
#' @template param/A
#' @description Class derived from matrix where each column represents 
#' characterisitc vector of player. Constructor is provided by function \link{BitMatrix}.
#' @slot .Data is matrix where each column represents the characteristic vector for a player. 
#' @exportClass BitMatrix
setClass(
  "BitMatrix",
  contains = "matrix"
)

setMethod(
  f="initialize",
  signature = "BitMatrix",
  definition =  function(.Object, n, A) {
    if(!is.null(A)){
      paramCheckResult=getEmptyParamCheckResult()
      stopOnInvalidGameVectorA(paramCheckResult,A=A)
      .Object@.Data=createBitMatrix(n,A)
    }else{
      .Object@.Data=createBitMatrix(n)
    }
    methods::validObject(.Object)
    return(.Object)
  }
)



#' @title Constructor for BitMatrix
#' @description Constructor for BitMatrix (\linkS4class{BitMatrix}).
#' @template author/JA
#' @name BitMatrix
#' @template param/n
#' @template param/A
#' @export BitMatrix
#' @return instance of BitMatrix
#' @examples
#' #Using bit matrix for glove game with 3 players and
#' #left glove players L={1,2} and right glove players R={3}.
#' library(CoopGame)
#' A<-cFuncGloveVector(n = 3,L = c(1,2), R=c(3))
#' BitMatrix(n=3,A)
#' #An object of class "BitMatrix"
#' #           cVal
#' #[1,] 1 0 0    0
#' #[2,] 0 1 0    0
#' #[3,] 0 0 1    0
#' #[4,] 1 1 0    0
#' #[5,] 1 0 1    1
#' #[6,] 0 1 1    1
#' #[7,] 1 1 1    1
#' 
BitMatrix<-function(n,A=NULL){
  retAP=methods::new("BitMatrix",n,A)
  return(retAP)
}


#' @title createBitMatrix
#' @name createBitMatrix
#' @description createBitMatrix creates a bit matrix with the dimensions (numberOfPlayers+1 X 2^numberOfPlayers-1) which contains all possible coalitions (apart from the null coalition) for the set of all players. 
#'              Each player is represented by a column which describes if this player is either participating (by value 1) or non-participating (by value 0).
#'              The last column (named cVal) contains the values created by each coalition.
#'              According to that each row expresses a coalition as a subset of all players
#'
#' @aliases createBitMatrix
#' @export createBitMatrix
#' @importFrom hier.part combos
#' @template author/JA
#' @template param/n
#' @template param/A
#' @return The return is a bit matrix containing all possible coalitions apart from the empty coalition
#' @examples
#' library(CoopGame)
#' A=cFuncQuotaVector(n=3,w=c(1,2,3),q=5)
#' bm=createBitMatrix(3,A)
#' bm
#'# Output:
#'#            cVal
#'# [1,] 1 0 0    0
#'# [2,] 0 1 0    0
#'# [3,] 0 0 1    0
#'# [4,] 1 1 0    0
#'# [5,] 1 0 1    0
#'# [6,] 0 1 1    1
#'# [7,] 1 1 1    1



createBitMatrix=function(n,A=NULL){

  #Create all possible subsets from set of all players
  bm=hier.part::combos(n)$binary

  #Add column named cVal which is intended to contain the values each created by the accordingly coalition
  bm=cbind(bm,cVal=0)
  #According to decision at meeting on 12/4 removed this row
  #bm=rbind(bm,ec=0)

  #Fill column named cVal by values of game vector A
  if(methods::hasArg(A)){
    bm[1:length(A),"cVal"]=A
  }
  #return bit matrix containing all possible coalition apart from the null coalition
  return (bm)
}

#' @title getPlayersFromBitVector
#' @name getPlayersFromBitVector
#' @description getPlayersFromBitVector determines players involved out of binary vector.#'
#' @aliases getPlayersFromBitVector
#' @export getPlayersFromBitVector
#' @template author/JA
#' @template author/JS
#' @param bitVector represents the binary vector 
#' @return playerVector contains the numbers of the involved players 
#' @examples 
#' library(CoopGame)
#' myBitVector <-c(1,0,1,0)
#' (players<-getPlayersFromBitVector(myBitVector))
#' #[1] 1 3 
#' 
getPlayersFromBitVector=function(bitVector){
  numberOfPlayers=length(bitVector)
  #changed identification of players for the playerVector on proposal of Alexandra Tiukkel to line below
  playerVector=which(bitVector&1)
  return(playerVector)
}

#' @title getPlayersFromBMRow
#' @name getPlayersFromBMRow
#' @description getPlayersFromBMRow determines players involved out of bit matrix row
#' @aliases getPlayersFromBMRow
#' @export getPlayersFromBMRow
#' @template author/JA
#' @template author/JS
#' @param bmRow represents the bit matrix row.
#' @return playerVector contains involved players (e.g. c(1,3) see example below for bitIndex=5 and n=3)
#' @examples
#' library(CoopGame)
#'  bm=createBitMatrix(n=3,A=c(1:7))
#' #Corresponding bit matrix:
#' #           cVal
#' #[1,] 1 0 0    1
#' #[2,] 0 1 0    2
#' #[3,] 0 0 1    3
#' #[4,] 1 1 0    4
#' #[5,] 1 0 1    5 <=Specified bit index
#' #[6,] 0 1 1    6
#' #[7,] 1 1 1    7
#'
#' #Determine players from bit matrix row by index 5
#'  players=getPlayersFromBMRow(bmRow=bm[5,])
#' #Result:
#'  players # [1] 1 4

getPlayersFromBMRow=function(bmRow){
  players=getPlayersFromBitVector(bmRow[1:(which(names(bmRow)=="cVal")-1)])
  #Remove col and rownames
  
  return(unname(players))
}
##Second Section: END

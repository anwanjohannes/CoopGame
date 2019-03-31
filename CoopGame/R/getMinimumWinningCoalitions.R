#' @name getMinimumWinningCoalitions
#' @title getMinimumWinningCoalitions
#' @description The function getMinimumWinningCoalitions identifies all minimal winning coalitions.
#' These coalitions are characterized by the circumstance that 
#' if any player break aways from them, then the coalition
#' generates no value (then also called a losing coalition) - all players
#' in the coalition can therefore be described as critical players.
#' @aliases getMinimumWinningCoalitions
#' @export getMinimumWinningCoalitions
#' @template author/JA
#' @template author/JS
#' @template cites/DEEGAN_ET_PACKEL_1978
#' @templateVar DEEGAN_ET_PACKEL_1978_P pp. 151-161
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 295
#' @template param/A
#' @return A data frame containing all minimum winning coalitions for a simple game.
#' @examples
#' library(CoopGame)
#' A=cFuncQuotaVector(n=3,w=c(1,2,3),q=5)
#'getMinimumWinningCoalitions(A)
#'# Output:
#'#   V1 V2 V3 cVal
#'# 6  0  1  1    1
#'# => the coalition containing player 2 and 3 is minimal 
#'


getMinimumWinningCoalitions=function(A){

  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidGameVectorA(paramCheckResult,A)
  if(!isSimpleGame(A)){
    print("Game is not simple therefore no minimum winning coalitions can be retrieved.")
    return(NULL)
  }else{
    bitMatrix=as.data.frame(createBitMatrix(n=getNumberOfPlayers(A),A))
    criticalMatrix=bitMatrix
    numberOfPlayers=getNumberOfPlayers(A)
    criticalCoalitions=c()
    rowsToBeRemoved=c()
    minimumWinningCoalitions=c()
    for(player in 1:numberOfPlayers){
      #get all critical coalitions of player 
      criticalCoalitions=getCriticalCoalitionsOfPlayer(player,A)[,"bmRow"]
      #define all entries as 0 for player column
      criticalMatrix[,player]=0
      
      #set 1 where player is critical
      criticalMatrix[criticalCoalitions,player]=1
    }
    #compare every row of bitMatrix and criticalMatrix
    for(bmIx in 1:nrow(bitMatrix)){
      #if coalition is mwc both rows are identical
      if(identical(bitMatrix[bmIx,],criticalMatrix[bmIx,])){
        minimumWinningCoalitions=c(minimumWinningCoalitions,bmIx)
      }
    }
    return(bitMatrix[minimumWinningCoalitions,,drop=FALSE])
  }
}
#' @name cFuncCostSharingValue
#' @title cFuncCostSharingValue
#' @description \strong{Coalition value for a cost sharing game:} \cr
#' For further information see \link{cFuncCostSharing} 
#' @aliases cFuncCostSharingValue
#' @export cFuncCostSharingValue
#' @template author/JA
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 14 f.
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 667 f.
#' @template param/S
#' @template param/Costs
#' @return The return is a numeric carrying the win of coalition S compared to taking part in single coalitions
#' @examples
#' #Example on 3 students sharing appartment:
#' #-------------------------------
#' #| costs     |  A  |  B  |  C  |
#' #- -----------------------------
#' #|single     | 300 | 270 | 280 |
#' #|appartment |     |     |     |
#' #-------------------------------
#' #
#' #Appartment for 2 persons => costs: 410
#' #Appartment for 3 persons => costs: 550
#' 
#' #Savings when A and B share appartment
#' library(CoopGame)
#' cFuncCostSharingValue(S=c(1,2),Costs=c(300,270,280,410,410,410,550))
#' #Output: 
#' #[1] 160
#'
cFuncCostSharingValue<-function(S,Costs){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S, n=getNumberOfPlayers(Costs))
  logicCFuncCostSharingValue(S,Costs)
}


#' @name cFuncCostSharingVector
#' @title cFuncCostSharingVector
#' @description \strong{Coalition vector for a cost sharing game:} \cr
#' For further information see \link{cFuncCostSharing} 
#' @aliases cFuncCostSharingVector
#' @export cFuncCostSharingVector
#' @template author/JA
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 14 f.
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 667 f.
#' @template param/n
#' @template param/Costs
#' @return The return is a numeric vector carrying the wins of each coalition S compared to taking part in single coalitions
#' @examples
#' #Example on 3 students sharing appartment:
#' #-------------------------------
#' #| costs     |  A  |  B  |  C  |
#' #- -----------------------------
#' #|single     | 300 | 270 | 280 |
#' #|appartment |     |     |     |
#' #-------------------------------
#' #
#' #Appartment for 2 persons => costs: 410
#' #Appartment for 3 persons => costs: 550
#' 
#' #Savings for all combinations sharing appartment
#' library(CoopGame)
#' (A=cFuncCostSharingVector(n=3, Costs=c(300,270,280,410,410,410,550)))
#' #Output: 
#' #An object of class "GameVector"
#' #[1]   0   0   0 160 170 140 300
#' 
cFuncCostSharingVector<-function(n,Costs){
  gameVector <- cFuncCostSharing(n,Costs)@A
  return(gameVector)
}


logicCFuncCostSharingValue=function(S,Costs){
  # C same structure as game vector A therefore number of player can be determined by same structure
  numberOfPlayers=getNumberOfPlayers(A=Costs)
  
  #Saves costs for all players out of set S under the single coalition assumption  
  costsOfSingleCoaltions=0
  
  #Saves costs for coalition S
  costsOfCoaltionS=0
  
  #win of coalition S
  winOfCoalitionS=0
  
  #Determines possible savings/losses by taking part in coalition S for all players of S
  indexOfS=indexCoalition(n = numberOfPlayers, S)
  costsOfSingleCoaltions=sum(Costs[as.numeric(S)])
  costsOfCoaltionS=Costs[indexOfS]
  winOfCoalitionS=costsOfSingleCoaltions-costsOfCoaltionS
  
  return(winOfCoalitionS)
}


#' @title cFuncCostSharing - class for cost sharing coalition game
#' @description Class for cost sharing coalition function,
#' inherits methods and slots from class \linkS4class{CoopGameCFunc}.
#' @include CoopGameCFunc.R
#' @exportClass cFuncCostSharing
#' @template slot/Costs

setClass(
  "cFuncCostSharing",
  representation(Costs="numeric"),
  contains = "CoopGameCFunc"
)

setMethod(
  f="initialize",
  signature = "cFuncCostSharing",
  definition=function(.Object,...,Costs){
    .Object@Costs=Costs
    .Object<-methods::callNextMethod(.Object, ...)
    return(.Object)
  }
)

#' @title Constructor for cFuncCostSharing
#' @description \strong{Constructor for a cost sharing game:} \cr
#' The user may specify the cost function of a cost allocation
#' problem. A corresponding savings game will be calculated.
#' The savings game specified by the game vector \code{A} 
#' will work like an ordinary TU game.
#' @template author/JA
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 14 f.
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 667 f.
#' @template param/n
#' @template param/Costs
#' @return An S4 object representing the specified cost game
#' @name cFuncCostSharing
#' @export
#' @section Related Functions: 
#' \link{cFuncCostSharingValue}, \link{cFuncCostSharingVector}
#' @examples 
#' #Example on 3 students sharing appartment:
#' #-------------------------------
#' #| costs     |  A  |  B  |  C  |
#' #- -----------------------------
#' #|single     | 300 | 270 | 280 |
#' #|appartment |     |     |     |
#' #-------------------------------
#' #
#' #Appartment for 2 persons => costs: 410
#' #Appartment for 3 persons => costs: 550
#' 
#' #Savings for all combinations sharing appartment
#' library(CoopGame)
#' (v <- cFuncCostSharing(n=3, Costs=c(300,270,280,410,410,410,550)))
#'#An object of class "cFuncCostSharing"
#'#Output:
#'#Slot "Costs":
#'#[1] 300 270 280 410 410 410 550
#'#
#'#Slot "A":
#'#An object of class "GameVector"
#'#[1]   0   0   0 160 170 140 300
#'#
#'#Slot "n":
#'#[1] 3
#'
cFuncCostSharing<-function(n,Costs){
  retCFuncCostSharing=methods::new("cFuncCostSharing",n=n,Costs=Costs)
  return(retCFuncCostSharing)
}

#' @rdname getCoalitionValue-methods
#' @aliases getCoalitionValue,cFuncCostSharing-method
setMethod(
  "getCoalitionValue",
  signature="cFuncCostSharing",
  definition=function(.Object,S,...){
    return(logicCFuncCostSharingValue(S,Costs=.Object@Costs))
  }
)



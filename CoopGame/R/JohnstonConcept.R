#' @name johnstonIndex
#' @title johnstonIndex
#' @description johnstonIndex calculates the Johnston index.
#' @aliases johnstonIndex
#' @export johnstonIndex
#' @template author/JA
#' @template author/MM
#' @template author/JS
#' @template cites/JOHNSTON_1977
#' @templateVar JOHNSTON_1977_P pp. 1055 -- 1066
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 124
#' @inheritParams CoopGameBaseParams
#' @return Johnston Index for a specified simple game
#' @examples
#' #player 1 has 3 votes
#' #player 2 has 2 votes
#' #player 3 has 1 vote
#' #majority for the decision is 4 (quota)
#'
#' #function call with game vector:
#' A <- generateGameVector(cFuncQuota, n = 3, w = c(3,2,1), q = 4)
#' #OR
#' #A <- c(0,0,0,1,1,0,1)
#'
#' johnstonIndex(A)
#' #[1] 0.6666667 0.1666667 0.1666667
#' 
johnstonIndex<-function(A){
  ji=JohnstonConcept(A)
  return(calculatePowerIndex(ji))
}

logicJohnstonIndex <- function(A) {
  retVal = NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Johnston Index can be retrieved")
  }
  else 
  {
    n <- getNumberOfPlayers(A)
    bm <- createBitMatrix(n, A)
    johnstonIndexVal <- rep.int(0, n)
    
    # gets the winning coalitions
    winningCoalitions=bm[bm[,"cVal"]>0,,drop=FALSE]
  
    if (length(winningCoalitions)==0) {
      print("There are no winning coalitions in this game")
    }
    else
    {
  
      mxRecVals <- matrix(ncol = n, nrow = nrow(winningCoalitions))
    
      # winning coalitions
      # [1] 1 1 0
      # [2] 1 0 1
      # [3] 1 1 1
    
      for (r in 1:nrow(winningCoalitions)) {
        tmpRow <- winningCoalitions[r, ]
    
        cntSwing <- 0
        vecSwingPlayers <- c()
    
        for (i in 1:n) {
          tmptmpRow <- tmpRow[1:n]
          # set player i to zero - 1 1 0 -> 0 1 0
          tmptmpRow[i] <- 0
          # compare bit pattern with entry in bitmatrix and get corresponding coalition value
          # null coalition generates no value
          if (sum(tmptmpRow != 0)) {
            ix <- indexCoalitionByBitVector(n, tmptmpRow)
            cVal <- bm[ix, 'cVal']
          } else {
            cVal <- -1
          }
    
          # check for each player in vulnerable coalition if player is critical (so called swing)
          # count number of players being in swing position
          if (cVal == 0) {
            cntSwing <- cntSwing + 1
            vecSwingPlayers <- c(vecSwingPlayers, 1)
          } else {
            vecSwingPlayers <- c(vecSwingPlayers, 0)
          }
        }
        if (cntSwing == 0) {
          recVal <- 0
        } else{
          # get reciprocal of number of swing players in coalition
          recVal <- 1 / cntSwing
        }
        # multiply vector containing swing players with reciprocal of number of swing players
        mxRecVals[r, ] <- vecSwingPlayers * recVal
      }
    
      # sum up the reciprocal for each player
      total <- colSums(mxRecVals)
    
      mxRecVals <- rbind(mxRecVals, total)
    
      totalNumberOfReciprocals <- sum(total)
    
      # The Johnston power of player i is the sum of the reciprocal of number of swings in
      # vulnerable  coalition c in  which i is  critical, divided by the total number of reciprocal of
      # number of swings in vulnerable coalition c of all players
      if (totalNumberOfReciprocals != 0) {
        johnstonIndexVal <- total / totalNumberOfReciprocals
      }
      retVal = johnstonIndexVal
      }
  }
  return(retVal)
}


# a winning coalition is vulnerable if, among its members, there is at least
# one  in  swing  position,  whose  swing  would  cause  the  coalition  to  lose.  Such  a  member  is
# called  critical.  If  only  one  player  is  critical,  then  this  player  is  uniquely  powerful  in  the
# coalition.
# getWinningCoalitions <- function(bm) {
#   winningCoalitions <- matrix()
#   winningCoalitions <- bm[bm[ ,'cVal'] != 0, , drop = FALSE]
# 
#   return(winningCoalitions)
# }

# isEmpty <- function(x){
#   return(length(x) == 0)
# }









#' @title JohnstonConcept - S4 class for Johnston Concept
#' @noRd
#' @include PointSolutionConcept.R
#' @slot A TODO
#' #@exportClass JohnstonConcept

setClass(
  "JohnstonConcept",
  contains = "PointSolutionConcept"
)

#' @title Constructor for JohnstonConcept
#' @noRd
#' @template author/JA
#' @name JohnstonConcept
#' #@export
JohnstonConcept<-function(A){
  retJohnstonConcept=methods::new("JohnstonConcept",A)
  return(retJohnstonConcept)
}

#' @rdname calculatePowerIndex-methods
#' @aliases calculatePowerIndex,JohnstonConcept-method
setMethod(
  "calculatePowerIndex",
  signature="JohnstonConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicJohnstonIndex(A))
  }
)
#' @name drawJohnstonIndex
#' @title drawJohnstonIndex for 3 or 4 players
#' @description drawJohnstonIndex draws the Johnston Value for 3 or 4 players.
#' @aliases drawJohnstonIndex
#' @export drawJohnstonIndex
#' @template cites/JOHNSTON_1977
#' @templateVar JOHNSTON_1977_P pp. 1055 -- 1066
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' A <- c(0,0,0,1,1,0,1)
#' drawJohnstonIndex(A)
drawJohnstonIndex<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Johnston value"){
  A=GameVector(A)
  lgv=johnstonIndex(A);
  visualize(A, pointsToDraw=lgv, holdOn=holdOn, colour = colour , label=label, name = name)
}
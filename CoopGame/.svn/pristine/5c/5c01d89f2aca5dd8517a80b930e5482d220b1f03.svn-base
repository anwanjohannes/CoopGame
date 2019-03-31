#' @name isWeaklySuperadditiveGame
#' @title isWeaklySuperadditiveGame
#' @description Checks if a TU game for n players is weakly superadditive. \cr
#' Let \code{S} be a coalition and \code{i} a player not contained in \code{S}. 
#' Then the TU game is weakly superadditive if for any \code{S} and 
#' any \code{i} the value of the union of \code{S} and \code{i} is
#' greater or equal the sum of the values of \code{S} and \code{i}. \cr
#' Note that weak superadditivity is equivalent to zero-monotonicity.
#' @aliases isZeroMonotonicGame
#' @export isWeaklySuperadditiveGame
#' @template author/JA
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 10
#' @inheritParams CoopGameBaseParams
#' @return \code{TRUE} if the game is weakly superadditive, else \code{FALSE}.
#' @examples
#' #Example of a weakly superadditive game
#' library(CoopGame)
#' A=c(1:15)
#' isWeaklySuperadditiveGame(A)
#' 
#' #Example of a game which is not weakly superadditive
#' library(CoopGame)
#' A=c(1:5,7,7)
#' isWeaklySuperadditiveGame(A)
#' 
isWeaklySuperadditiveGame<-function(A){
  isWS=GamePropertyWeakSuperadditivity(A)
  return(determineProperty(isWS))
}

logicIsWeaklySuperadditiveGame<-function(A){
  n=getNumberOfPlayers(A)
  bm=as.data.frame(createBitMatrix(n=n,A))
  
  for(i in (n-1):1){
    lb=indexLower(n,i);
    ub=indexUpper(n,i);

    for(j in indexUpper(n,i+1):indexLower(n,i+1)){
      playersInvolved=((bm[j,1:n])&1)
      playersUninvolved=!playersInvolved
      
 
      #builds expression for identifying rows of data.frame where uninvolved players are contained
      exp=paste0("(",paste(c("0",colnames(bm)[c(playersUninvolved,FALSE)],"0"),collapse = "|"),")")

      #select all possible coalition values only out of subset S union i
      #(therefore exclude rows for uninvolved players)
      v_S=subset(bm[lb:ub,],!(eval(parse(text=exp))==1),"cVal")
      
      #select all single coalition values of involved players and put them in reverse order
      v_i=rev(bm[1:n,][playersInvolved,"cVal"])
      
      #check if condition for currently considered entries is violated
      v_Splusi=v_S+v_i
      v_Sui=bm[j,"cVal"]
      if(!all(v_Splusi<=v_Sui,TRUE)){
        # print(paste("Game property 'weakly additive' is violated for instance for coalition S with index",rownames(v_S)[!v_Splusi<=v_Sui],"and player i as",i))
        return(FALSE)
      }

    }
  }
  
  return(TRUE)
  
  
}

#' @title GamePropertyWeakSuperadditivity
#' @noRd
#' @include GameProperty.R
#' #@exportClass GamePropertyWeakSuperadditivity

setClass(
  "GamePropertyWeakSuperadditivity",
  contains = "GameProperty"
)

#' @title Constructor for GamePropertyWeakSuperadditivity
#' @noRd
#' @template author/JA
#' @name GamePropertyWeakSuperadditivity
#' #@export
GamePropertyWeakSuperadditivity<-function(A){
  retGamePropertyWeakSuperadditivity=methods::new("GamePropertyWeakSuperadditivity",A)
  return(retGamePropertyWeakSuperadditivity)
}

#' @rdname determineProperty-methods
#' @aliases determineProperty,GamePropertyWeakSuperadditivity-method
setMethod(
  "determineProperty",
  signature="GamePropertyWeakSuperadditivity",
  definition=function(.Object){
    A<-.Object@A
    result=logicIsWeaklySuperadditiveGame(A)
    return(result)
  }
)

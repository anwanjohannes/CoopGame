#' @name isMonotonicGame
#' @title isMonotonicGame
#' @description Checks if a TU game for n players is monotonic. \cr
#' For a monotonic game a coalition \code{S} can never obtain 
#' a larger value than another coalition  \code{T} if \code{S}
#' is contained in \code{T}.
#' @export isMonotonicGame
#' @template author/JA
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 12
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P p. 408 
#' @inheritParams CoopGameBaseParams
#' @return \code{TRUE} if the game is monotonic, else \code{FALSE}
#' @examples
#' #Example of a non-monotonic game
#' library(CoopGame)
#' A=c(4,2,5,2,3,6,10)
#' isMonotonicGame(A)
#' 
#' #Example of a monotonic game
#' library(CoopGame)
#' A=c(2,5,7,10, 9, 13,20)
#' isMonotonicGame(A)
#'
isMonotonicGame<-function(A){
  isMG=GamePropertyMonotonicity(A)
  return(determineProperty(isMG))
}

logicIsMonotonicGame<-function(A){
   boolRetVal=TRUE
   n=getNumberOfPlayers(A)
   N=length(A)
   bm=as.data.frame(createBitMatrix(n,A))
   players=1:n
   for(i in N:indexLower(n,2)){
     involvedPlayers=getPlayersFromBMRow(bm[i,])
     uninvolvedPlayers=players[-involvedPlayers]
     corrCVals=getCorrespondingCVals(bm[1:(i-1),],uninvolvedPlayers)
     allSmallerOrEqual=all(corrCVals<=bm[i,"cVal"])
     if(!allSmallerOrEqual){
       boolRetVal=FALSE
       break
     }
   }
   return(boolRetVal)
}

#Identifies all coalition values where (by uninvolvedPlayers) specified players
#are not participating
getCorrespondingCVals<-function(bmDataFrame,uninvolvedPlayers){
  exp=paste0("(",paste(c("0",colnames(bmDataFrame)[c(uninvolvedPlayers,FALSE)],"0"),collapse = "|"),")")
  entries=subset(bmDataFrame[,],!(eval(parse(text=exp))==1),"cVal")
  return(entries)
}

#' @title GamePropertyMonotonicity
#' @noRd
#' @include GameProperty.R
#' #@exportClass GamePropertyMonotonicity

setClass(
  "GamePropertyMonotonicity",
  contains = "GameProperty"
)

#' @title Constructor for GamePropertyMonotonicity
#' @noRd
#' @template author/JA
#' @name GamePropertyMonotonicity
#' #@export
GamePropertyMonotonicity<-function(A){
  retGamePropertyMonotonicity=methods::new("GamePropertyMonotonicity",A)
  return(retGamePropertyMonotonicity)
}

#' @rdname determineProperty-methods
#' @aliases determineProperty,GamePropertyMonotonicity-method
setMethod(
  "determineProperty",
  signature="GamePropertyMonotonicity",
  definition=function(.Object){
    A<-.Object@A
    result=logicIsMonotonicGame(A)
    return(result)
  }
)

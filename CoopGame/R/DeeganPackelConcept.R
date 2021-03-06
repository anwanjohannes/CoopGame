#' @name deeganPackelIndex
#' @title deeganPackelIndex
#' @description deeganPackelIndex calculates the Deegan Packel index for simple games
#' @aliases deeganPackelIndex
#' @export deeganPackelIndex
#' @template author/JA
#' @template author/MM
#' @template author/JS
#' @template cites/DEEGAN_ET_PACKEL_1978
#' @templateVar DEEGAN_ET_PACKEL_1978_P pp. 151-161
#' @template cites/HOLLER_ET_ILLING_2006
#' @templateVar HOLLER_ET_ILLING_2006_P pp. 323
#' @inheritParams CoopGameBaseParams 
#' @return Deegan Packel Values for a specified simple game
#' @examples
#'  #Example out of HOLLER & ILLING (2006), chapter 6.3.3
#'  # v=(51;35,20,15,15,15) => dpv=(18/60,9/60,11/60,11/60,11/60)
#'  A=generateGameVector(v =cFuncQuota,n = 5, w=c(35,20,15,15,15), q=51)
#'  deeganPackelIndex(A)
#'  #Output (same to expected as in HOLLER & ILLING chapter 6.3.3) :
#'  #[1] 0.3000000 0.1500000 0.1833333 0.1833333 0.1833333
#'
deeganPackelIndex<-function(A){
  dpi=DeeganPackelConcept(A)
  return(calculatePowerIndex(dpi))
}

logicDeeganPackelIndex=function(A){
  retVal=NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Deegan Packel Index can be retrieved.")
  }
  else
  {
    #Create to game vector A corresponding bit matrix
    n=getNumberOfPlayers(A)
  
  
    #Determine minimum winning coalitions in bit matrix
    minimumWinningCoalitions=getMinimumWinningCoalitions(A)
  
    #determine cardinality of minimum winning coalitions in bit matrix
    cardMWC=nrow(minimumWinningCoalitions)
    dpv=c()
  
    if(cardMWC==0){
      #if there are no minimum winning coalitions (e.g for A=rep(0,15))
      #each players gets an deegan packel value of 0
      # Well, actually CoopGame is supposed not accept null games ...:-)
      dpv=rep(0,n)
    }else{
      #if there are minimum winning coalitions, they determine Deegan Packel Index
  
      #Determine constant factor which is same for each player
      #=> 1 divided by number of minimum winning coalitions
      constFactor=1/cardMWC
  
      #Loop through all players and determine each player's specific deegan packel value
      for(i in 1:n){
        sumvKDivk=0
        minimumWinningCoalitionsOfI=minimumWinningCoalitions[minimumWinningCoalitions[,i]==1,,drop=FALSE]
        if(nrow(minimumWinningCoalitionsOfI)!=0){
          #Loop through all minimum winning coalitions of player i
          for(j in 1:nrow(minimumWinningCoalitionsOfI)){
            #determine k as sum of players involved in current row  of minimumWinningCoalitionsOfI
            k=sum(minimumWinningCoalitionsOfI[j,1:n] == 1)
            #determine each player's specific part of the deegan packel equatation
            sumvKDivk=sumvKDivk+minimumWinningCoalitionsOfI[j,"cVal"]/k
          }
  
          #Deegan Packel Index is calculated by multiplying constant factor and specific part of each player
          dpv[i]=constFactor*(sumvKDivk)
        }else{
          #if player i is not involved in any minimum winning coalition he gets value 0 allocated
          dpv[i]=0
        }
      }
    }
    
    # if(sum(dpv)>1){
    #   dpv=dpv/sum(dpv)
    # }
    retVal = dpv
  }
  return(retVal)
}

#' @title DeeganPackelConcept - class for Deegan Packel Concept
#' @noRd
#' @include PointSolutionConcept.R
#' #@exportClass DeeganPackelConcept

setClass(
  "DeeganPackelConcept",
  contains = "PointSolutionConcept"
)

#' @title Constructor for DeeganPackelConcept
#' @noRd
#' @template author/JA
#' @name DeeganPackelConcept
#' #@export
DeeganPackelConcept<-function(A){
  retDeeganPackelConcept=methods::new("DeeganPackelConcept",A)
  return(retDeeganPackelConcept)
}

#' @rdname calculatePowerIndex-methods
#' @aliases calculatePowerIndex,DeeganPackelConcept-method
setMethod(
  "calculatePowerIndex",
  signature="DeeganPackelConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicDeeganPackelIndex(A))
  }
)




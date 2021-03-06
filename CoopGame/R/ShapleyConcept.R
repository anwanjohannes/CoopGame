#' @name shapleyValue
#' @title shapley for n players
#' @description Calculates the Shapley Value for n players with formula from Lloyd Shapley.
#' @aliases shapleyValue
#' @export shapleyValue
#' @template author/AT
#' @template cites/SHAPLEY_1953
#' @templateVar SHAPLEY_1953_P pp. 307--317
#' @template cites/AUMANN_2010
#' @templateVar AUMANN_2010_P pp. 3--10
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 156 ff.
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 748 ff.
#' @inheritParams CoopGameBaseParams
#' @return Shapley value for given game vector with n players
#' @examples
#' #glove game with l={1}, r={2,3} with 3 players
#' A=c(0,0,0,1,1,0,1)
#'
#' shapleyValue(A) #return (2/3, 1/6, 1/6)
#' 
shapleyValue<-function(A){
  sv=ShapleyConcept(A)
  return(calculatePointSolution(sv))
}

#' @name shapleyShubikIndex
#' @title Shapley-Shubik for n players
#' @description Calculates the Shapley index for a specified simple game with n players.
#' @aliases shapleyShubikIndex
#' @export shapleyShubikIndex
#' @template author/AT
#' @template author/JS
#' @template cites/SHAPLEY_SHUBIK_1954
#' @templateVar SHAPLEY_SHUBIK_1954_P pp. 787--792
#' @template cites/SHAPLEY_1953
#' @templateVar SHAPLEY_1953_P pp. 307--317
#' @template cites/AUMANN_2010
#' @templateVar AUMANN_2010_P pp. 3--10
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 156 ff.
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 748 ff.
#' @inheritParams CoopGameBaseParams
#' @return Shapley-Shubik Index for given simple game
#' @examples
#' #glove game with l={1}, r={2,3} with 3 players
#' A=c(0,0,0,1,1,0,1)
#'
#' shapleyShubikIndex(A) #return (2/3, 1/6, 1/6)
shapleyShubikIndex<-function(A){
  retVal = NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Shapley Shubik Index can be retrieved")
  }
  else
  {
    si=ShapleyConcept(A)
    retVal=calculatePowerIndex(si)
  }  
  return(retVal)
}

logicShapleyValue <- function(A) {
  numberOfPlayers=getNumberOfPlayers(A)
  
  shapley<-c()
  
  coalitions<-list()

  
  bitMatrix=createBitMatrix(n = numberOfPlayers, A)
  bitMatrix<-rbind(bitMatrix, 0)
  
  i<-1
  j<-1
  #loop over all players
  while(i<=numberOfPlayers){
    #get all coalitions the current player is not part of
    vectorWithIndx<-which(!(bitMatrix[, i]&1))
    #save in a list for result
    currCoal<-bitMatrix[vectorWithIndx,]
    coalitions[[i]]<-currCoal
    
    shapleyForPlayer = 0
    while(j<=nrow(currCoal)){
      curCoalSet = which(currCoal[j,1:numberOfPlayers]&1)
      mightiness = sum(currCoal[j,1:numberOfPlayers])
      #formula:sum(|C|!(n-|C|-1)!/n! *(v(Cv{i})-v(C)))
      shapleyForPlayer = shapleyForPlayer +
        ((factorial(mightiness)*(factorial(numberOfPlayers-mightiness-1))/factorial(numberOfPlayers))
         *(A[indexCoalition(numberOfPlayers,union(i,curCoalSet))]
           -currCoal[[j, "cVal"]]))
      
      j<-j+1
    }
    
    j<-1
    shapley[i] = shapleyForPlayer
    
    i<-i+1
  }
  
  # return(list(coalitions = coalitions, shapleyValue=shapley))
  return(shapley)
}

#' @title ShapleyConcept - S4 class for shapley concept
#' @noRd
#' @include PointSolutionConcept.R
#'# @exportClass ShapleyConcept

setClass(
  "ShapleyConcept",
  contains = "PointSolutionConcept"
)

#' @rdname calculatePointSolution-methods
#' @aliases calculatePointSolution,ShapleyConcept-method
setMethod(
  "calculatePointSolution",
  signature="ShapleyConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicShapleyValue(A))
  }
)

#' @title Constructor for ShapleyConcept
#' @noRd
#' @template author/JA
#' @name ShapleyConcept
ShapleyConcept<-function(A){
  retShapleyConcept=methods::new("ShapleyConcept",A)
  return(retShapleyConcept)
}



#' @name drawShapleyValue
#' @title drawShapleyValue for 3 or 4 players
#' @description drawShapleyValue draws the Shapley Value for 3 or 4 players.
#' @aliases drawShapleyValue
#' @export drawShapleyValue
#' @template author/AT
#' @template cites/SHAPLEY_1953
#' @templateVar SHAPLEY_1953_P pp. 307--317
#' @template cites/AUMANN_2010
#' @templateVar AUMANN_2010_P pp. 3--10
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 156 ff.
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 748 ff.
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' #glove game with l={1}, r={2,3} with 3 players
#' A=c(0,0,0,1,1,0,1)
#' drawShapleyValue(A) 
drawShapleyValue<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Shapley value"){
  A=GameVector(A)
  shapleyValue=shapleyValue(A);
  visualize(A, pointsToDraw=shapleyValue, holdOn=holdOn, colour = colour , label=label, name = name)
}


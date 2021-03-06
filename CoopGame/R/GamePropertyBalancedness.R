#' @name isBalancedGame
#' @title isBalancedGame for n Players
#' @description Checks if a game is balanced. \cr
#'              A game is balanced if the core is a nonempty set. \cr 
#'              This routine uses the 'dual' characterization 
#'              of balancedness via the Bondareva-Shapley theorem 
#'              and minimal balanced collections.
#' @aliases isBalancedGame
#' @export isBalancedGame
#' @template author/JA
#' @template author/JS
#' @import glpkAPI
#' @template cites/BONDAREVA_1963
#' @templateVar BONDAREVA_1963_P pp. 119--139
#' @template cites/SHAPLEY_1967
#' @templateVar SHAPLEY_1967_P pp. 453--460
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 27 ff.
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 691 ff.
#' @template cites/SLIKKER_ET_NOUWELAND_2001
#' @templateVar SLIKKER_ET_NOUWELAND_2001_P p. 6 ff.
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 262 ff.
#' @inheritParams CoopGameBaseParams
#' @return \code{TRUE} if the game is balanced, else \code{FALSE}
#' @examples
#' #Example of an unbalanced game with 3 players
#' library(CoopGame)
#' A=c(1,1,1,2,3,4,3)
#' isBalancedGame(A)
#' 
#' #Example of a balanced game with 3 players
#' library(CoopGame)
#' A=c(0,0,0,40,50,20,100)
#' isBalancedGame(A)
#' 
#' #Example of an unbalanced game with 4 players
#' library(CoopGame)
#' A=c(0,0,0,0,1,0,0,0,0,3,3,3,3,3,4)
#' isBalancedGame(A)
#' 
#' #Example of a balanced game with 4 players
#' library(CoopGame)
#' A= c(0,0,0,0,1,0,0,0,0,2,2,2,2,2,4)
#' isBalancedGame(A)
#' 
isBalancedGame<-function(A){
  isE=GamePropertyBalancedness(A)
  return(determineProperty(isE))
}

logicIsBalancedGame<-function(A, enableTermOutLP=FALSE){
  
  #Return value 
  retVal=NULL
  
  # determine if terminal output should be enabled or disabled
  termOutGLPK(ifelse(enableTermOutLP, GLP_ON, GLP_OFF))
  
  n=getNumberOfPlayers(A)
  bm=createBitMatrix(n,A)
  
  #number of all coalitions apart from null coalition
  N=length(A)
  
  #initialisation of linear maximisation problem
  prob=initProbGLPK()
  setObjDirGLPK(prob,GLP_MAX);
  setProbNameGLPK(prob,"Balanced");
  
  #Add variables, bounds and optimisation function
  addColsGLPK(prob,N);
  setColsNamesGLPK(prob,c(1:N),sapply(c(1:N),function(i){paste0("a",i)}))
  setColsBndsGLPK(prob,c(1:N),rep(0,N),rep(0,N),rep(GLP_LO,N))
  setObjCoefsGLPK(prob,c(1:N),as.vector(bm[,"cVal"]))
 
  #Add player 
  
  addRowsGLPK(prob,n+1)
  setRowsNamesGLPK(prob,c(1:n),sapply(c(1:n),function(i){paste0("a",i)}))
  setRowsBndsGLPK(prob,c(1:n),rep(1.0,n),rep(1.0,n),rep(GLP_FX,n))
  
  #Add constraints to linear problem
  LPM<-t(bm[,1:n])
  ia=c(rep(1:(n),each=N))
  ja=c(rep(1:N,(n)))
  loadMatrixGLPK(prob,(n)*N,ia,ja,as.vector(t(LPM)))
  
  solveSimplexGLPK(prob)
  objVal <- getObjValGLPK(prob)
  

  if (objVal == A[length(A)]) {
    retVal=TRUE
  }else if(objVal>A[length(A)]){
    retVal=FALSE
  }
  delProbGLPK(prob)
  return(retVal)
}

#' @title GamePropertyBalancedness
#' @noRd
#' @include GameProperty.R
#' #@exportClass GamePropertyBalancedness

setClass(
  "GamePropertyBalancedness",
  contains = "GameProperty"
)

#' @title Constructor for GamePropertyBalancedness
#' @noRd
#' @template author/JA
#' @template author/JS
#' @name GamePropertyBalancedness
#' #@export
GamePropertyBalancedness<-function(A){
  retGamePropertyBalancedness=methods::new("GamePropertyBalancedness",A)
  return(retGamePropertyBalancedness)
}

#' @rdname determineProperty-methods
#' @aliases determineProperty,GamePropertyBalancedness-method
setMethod(
  "determineProperty",
  signature="GamePropertyBalancedness",
  definition=function(.Object){
    A<-.Object@A
    result=logicIsBalancedGame(A)
    return(result)
  }
)




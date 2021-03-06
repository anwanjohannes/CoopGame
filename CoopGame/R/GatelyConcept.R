#' @name gatelyValue
#' @title gatelyValue for calculating the Gately Point
#' @description gatelyValue calculates the Gately Point for a  a certain game vector A
#' @aliases gatelyValue
#' @export gatelyValue
#' @template author/JA
#' @template cites/GATELY_1974
#' @templateVar GATELY_1974_P pp. 195 -- 208
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P  p. 455 f.
#' @inheritParams CoopGameBaseParams
#' @return gately value or NULL when not definite
#' @examples
#' A=c(0,0,0,4,0,3,6)
#' gatelyValue(A)
#'
#' #Output (18/11,36/11,12/11):
#' #1.636364 3.272727 1.090909
#' 
#' #GATELY 1972
#' A=c(0,0,0,1170,770,210,1530)
#' gatelyValue(A)
#' 
#' #Output:
#' #827.7049 476.5574 225.7377 
#' 
gatelyValue<-function(A){
  bv=GatelyConcept(A)
  return(calculatePointSolution(bv))
}


logicGatelyValue<-function(A){
  # J.Staudacher: This code needs changes ...
  # assign number of coalitions
  N <- length(A)

  # assign number of players
  n <- getNumberOfPlayers(A)
  
  gatelyValue <- NULL
  
  if(!isEssentialGame(A)){
    print("Gately Value does not exist as imputation set is empty.")
  }else if(!((sum(A[1:n])<A[N]))){
    print("Gately Value does not exist as A shows is no strict imputation set.")
  }else{
    eptd=equalPropensityToDisrupt(A,k=1)
    if(eptd==-1){
      additionVectorComplements=sapply(
                                  1:n,
                                  function(ix){A[ix]+A[N-ix]}
                                )
      if(all(additionVectorComplements==A[N],TRUE)){
        msg="Gately Value is not unique as equal with imputation set,"
        msg=paste(msg,"therefore centroid of the imputation set" )
        msg=paste(msg,"was defined as solution.")
        setVertices=imputationsetVertices(A)
        centroid=colSums(setVertices)/n
        gatelyValue=centroid
        print(msg)
      }else{
        print("No solution existing")
      }
    }else{
      coeffMat<-createBitMatrix(n,A)[indexLower(n,n-1):(N-1),]
      coeffMat[,1:n][coeffMat[,1:n]==0]=-eptd
      coeffMat[,"cVal"]=rev(
                          sapply(
                            1:n,
                            function(ix){
                              coeffMat[((n+1)-ix),"cVal"]-eptd*A[ix]
                            }
                          )
                        )
      gatelyValue=tryCatch(
        solve(coeffMat[,1:n],coeffMat[,"cVal"]),
        error=function(x){NULL}
      )
      if(!isImputation(x=gatelyValue,A)){
        gatelyValue=NULL
        print("Calculated value is no imputation hence no Gately Value exists.")
      }
    }
    
  }
  names(gatelyValue)<-NULL
  return(gatelyValue)
}


#' @name equalPropensityToDisrupt
#' @title equalPropensityToDisrupt for calculating coalition's index
#' @description equalPropensityToDisrupt calculates the equal propensity to disrupt for a specified number of players k  has either in a bit matrix or in a game vector where n players are involved in the game
#' @aliases equalPropensityToDisrupt
#' @export equalPropensityToDisrupt
#' @template author/JA
#' @template cites/LITTLECHILD_ET_VAIDYA_1976
#' @templateVar LITTLECHILD_ET_VAIDYA_1976_P pp. 151 -- 161
#' @template param/A
#' @param k is the number of players which should be considered when calculating the equal propensity to disrupt
#' @return the value for the equal propensity to disrupt
#' @examples
#' A=c(0,0,0,4,0,3,6)
#' equalPropensityToDisrupt(A, k=1)
#'
#' #Output (5/6):
#' #0.8333333

equalPropensityToDisrupt<-function(A,k=1){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_equalPropensityToDisrupt(paramCheckResult,A)
  
  n=as.numeric(getNumberOfPlayers(A))
  bm=createBitMatrix(n)

  bm[,"cVal"]=apply(bm,1,sum)
  pos_n_min_k=which(bm[,"cVal"]==(n-k))
  pos_k=which(bm[,"cVal"]==(k))


  vN=A[length(A)]

  factorNumerator=factorial(n-1)/(factorial(k)*factorial(n-k-1))
  sum_N_min_S=sum(A[pos_n_min_k])

  factorDenominator=factorial(n-1)/(factorial(k-1)*factorial(n-k))
  sum_S=sum(A[pos_k])

  numerator=factorNumerator*vN - sum_N_min_S
  denominator=factorDenominator*vN - sum_S

  if(denominator==0){
    dk=Inf
  }else{
    dk=numerator/denominator
  }

  return(dk)

}

initialParamCheck_equalPropensityToDisrupt<-function(paramCheckResult,A){
  stopOnInvalidGameVectorA(paramCheckResult,A)
}


#' @name propensityToDisrupt
#' @title propensityToDisrupt for calculating the propensity of disrupt
#' @description propensityToDisrupt for calculating the propensity of disrupt for game vector A, an allocation x and a specified coalition S'
#' @aliases propensityToDisrupt
#' @export propensityToDisrupt
#' @template author/JA
#' @template author/JS
#' @template param/A
#' @template param/x
#' @template param/S
#' @return propensity to disrupt as numerical value
#' @examples
#' A=c(0,0,0,4,0,3,6)
#' x=c(2,3,1)
#' propensityToDisrupt(A,x,S=c(1))
#' #Output
#' #0.5
#' propensityToDisrupt(A,x,S=c(2))
#' #Output
#' #1
#' propensityToDisrupt(A,x,S=c(3))
#' #Output
#' #1

propensityToDisrupt<-function(A,x,S){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_propensityToDisrupt(paramCheckResult,A, x, S)
  retVal = 0
  
  n=as.numeric(getNumberOfPlayers(A))
  N=2^n
  # return 0 for grand coalition
  if (length(S) < n)
  {
    N_min_S=c(1:n)[-S]
  
    nominator=(sum(x[N_min_S])-A[indexCoalition(n,N_min_S)])
    denominator=(sum(x[S])-A[indexCoalition(n,S)])
    di_x=nominator/denominator
  
    # if(is.nan(di_x)){
    #   di_x=0
    # }
    retVal = di_x
 }
 return(retVal)
}

initialParamCheck_propensityToDisrupt<-function(paramCheckResult,A,x,S){
  stopOnInvalidGameVectorA(paramCheckResult,A)
  stopOnInvalidCoalitionS(paramCheckResult, S)
  stopOnInvalidAllocation(paramCheckResult, x)
}

#' @name getVectorOfPropensitiesToDisrupt
#' @title getVectorOfPropensityToDisrupt for calculating the vector of propensities to disrupt'
#' @description vector of propensities to disrupt for game vector A and an allocation x'
#' @aliases getVectorOfPropensitiesToDisrupt
#' @export getVectorOfPropensitiesToDisrupt
#' @template author/JS
#' @template param/A
#' @template param/x
#' @return a numerical vector of propensities to disrupt at allocation x
#' @examples
#' A=c(0,0,0,4,0,3,6)
#' x=c(2,3,1)
#' getVectorOfPropensitiesToDisrupt(A,x)
#' #Output
#' #[1] 0.5 1.0 1.0 1.0 1.0 2.0 0.0
#'

getVectorOfPropensitiesToDisrupt<-function(A,x){
  paramCheckResult=getEmptyParamCheckResult()
  
  n=getNumberOfPlayers(A);
  propensitiesVector = numeric(length(A))
  for (i in 1:length(A))
  {
    S = as.vector(getPlayersFromIndex(n,i))
    propensitiesVector[i] = propensityToDisrupt(A,x,S)
  }
  # Note that vector of propensities to disrupt set to zero for grand coalition
  
  return(propensitiesVector)
}


#' @name getNondefiniteGameVector4GatelyValue
#' @title getNondefiniteGameVector4GatelyValue for generating a game vector for a not definite solution according to the gately solution concept
#' @description getNondefiniteGameVector4GatelyValue for generating a game vector for a not definite  solution according to the gately solution concept
#' @aliases getNondefiniteGameVector4GatelyValue
#' @export getNondefiniteGameVector4GatelyValue
#' @template author/JA
#' @param vN value of grand coalition
#' @param vSCs is a vector containing the coalition values for the single coalitions
#' @param w is a vector containing the coalition values for coalitions not considered when calculating the gately value
#' @return An indefinite game for the solution gately concept considering the specified parameters and an equal propensity to disrupt of minus 1
#' @examples
#' A=getNondefiniteGameVector4GatelyValue(10,c(0,1,2,3),w=rep(99,6))
#' equalPropensityToDisrupt(A) #-1

getNondefiniteGameVector4GatelyValue<-function(vN, vSCs, w=NULL){
  n=length(vSCs)
  N=(2^n)-1
  
  A=c()
  A[1:n]=vSCs
  
  if(n>3){
    lbNIC=n+1
    ubNIC=indexUpper(n,2)
    if((ubNIC-lbNIC+1)!=length(w)){
      stop("Parameter w not valid")
    }
    A[lbNIC:ubNIC]=w
  }
  
  
  lbCC=indexLower(n,(n-1))
  ubCC=N-1
  A[lbCC:ubCC]=vN-vSCs[n:1]
  
  
  A[N]=vN
  return(A)
}

#' @title GatelyConcept 
#' @noRd
#' @include PointSolutionConcept.R
#' #@exportClass GatelyConcept

setClass(
  "GatelyConcept",
  contains = "PointSolutionConcept"
)

#' @title Constructor for GatelyConcept
#' @noRd
#' @template author/JA
#' @name GatelyConcept
#' #@export
GatelyConcept<-function(A){
  retGatelyConcept=methods::new("GatelyConcept",A)
  return(retGatelyConcept)
}

#' @rdname calculatePointSolution-methods
#' @aliases calculatePointSolution,GatelyConcept-method
setMethod(
  "calculatePointSolution",
  signature="GatelyConcept",
  definition=function(.Object){
    A<-.Object@A
    return(logicGatelyValue(A))
  }
)

#' @name drawGatelyValue
#' @title drawGatelyValue for 3 or 4 players
#' @description drawGatelyValue draws the Gately Value for 3 or 4 players.
#' @aliases drawGatelyValue
#' @export drawGatelyValue
#' @template author/JA
#' @template cites/GATELY_1974
#' @templateVar GATELY_1974_P pp. 195 -- 208
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P  p. 455 f.
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' #GATELY 1972
#' A=c(0,0,0,1170,770,210,1530)
#' drawGatelyValue(A)
drawGatelyValue<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Gately Value"){
  A=GameVector(A)
  gatelyValue=gatelyValue(A);
  visualize(A, pointsToDraw=gatelyValue, holdOn=holdOn, colour = colour , label=label, name = name)
}



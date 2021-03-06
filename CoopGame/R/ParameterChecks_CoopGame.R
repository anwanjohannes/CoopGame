#' @name ParameterChecks_CoopGame
#' @family ParameterChecks_CoopGame
#' @title Parameter checks which are used for initial parameter checks
#' @export stopOnInvalidParameterDummy_CoopGame
#' @description ParameterChecks_CoopGame aims at offering global functions 
#' for often used general parameter checks therefore
#' central definition of unique error codes and messages can be supplied.
#' @template param/paramCheckResult
#' @param anyParameterChecked exemplary parameter for showing concept
#' @template ErrorCodes/Ranges
#'
#' @examples
#' #Create a parameter check object
#' paramCheckResult<-getEmptyParamCheckResult()
#'
#' #Execute parameter check
#' #  in this case with dummy function stopOnInvalidParameterDummy_CoopGame which
#' #  causes stop when number passed is either not in the range
#' # between 0 and 1000 or not numeric.
#' #stopOnInvalidParameterDummy_CoopGame(paramCheckResult, -1)
#' #Error in stopOnParamCheckError(paramCheckResult) :
#' #   Error Code 9991: Parameter anyParameterChecked is smaller than 0
#'
#' #Afterwards the encountered error is still accessible by the paramCheckResult object
#' #Error code
#' cat("errCode: ",paramCheckResult$errCode,"errMessage: ",paramCheckResult$errMessage)
#' #errCode:  9991 errMessage:  Parameter anyParameterChecked is smaller than 0
#'
#' #Analogous for anyNumber>1000 and anyNumber is not numeric (e.g. anyNumber="200")
#'
#' #anyNumber>1000
#' #stopOnInvalidParameterDummy_CoopGame(paramCheckResult,2000)
#' #Error in stopOnParamCheckError(paramCheckResult) :
#' #  Error Code 9992: Parameter anyParameterChecked is bigger than 1000
#'
#' #anyNumber is not numeric as anyNumber="200"
#' #stopOnInvalidParameterDummy_CoopGame(paramCheckResult,"200")
#' #Error in stopOnParamCheckError(paramCheckResult) :
#' #  Error Code 9990: Parameter anyParameterChecked is not numeric


stopOnInvalidParameterDummy_CoopGame<-function(paramCheckResult, anyParameterChecked){
  checkResult=getEmptyParamCheckResult();
  print("stopOnInvalidParameterDummy_CoopGame is a dummy function for a simple parameter check. In case of Parameter anyParameterChecked it checks if it is both in the range between 0 and 1000 as well as numeric.");
  #Begin of checks
  #The errCode and the errMessage get set according to first error encounterd when checking
  if(!is.numeric(anyParameterChecked)){
    checkResult$errCode=9990;
    checkResult$errMessage="Parameter anyParameterChecked is not numeric"
  }else if(anyParameterChecked<0){
    checkResult$errCode=9991;
    checkResult$errMessage="Parameter anyParameterChecked is smaller than 0"
  }else if(anyParameterChecked>1000){
    checkResult$errCode=9992;
    checkResult$errMessage="Parameter anyParameterChecked is bigger than 1000"
  }
  #End of checks

  #The parameter check result substitutes the passed parameter check object
  eval.parent(substitute(paramCheckResult<-checkResult));
  #If  an error was encountered, stopOnParamCheckError causes stop and errMessage is shown to user
  stopOnParamCheckError(paramCheckResult);
}



#First Section of ParameterChecks_CoopGame.R contains general parameter check functions:
  # -stopOnInvalidGameVectorA (errCode range: 1000-1009)
  # -stopOnInvalidGrandCoalitionN (errCode range: 1010-1019)
  # -stopOnInvalidCoalitionS (errCode range: 1020-1029)
  # -stopOnInvalidCoalitionFunctionV (errCode range: 1040-1049)
  # -stopOnInvalidNumberOfPlayers (errCode range: 1050-1059)
  # -stopOnInvalidIndex (errCode range: 1070-1079)
  # -stopOnInvalidNChooseB (errCode range: 1080-1089)
  # -stopOnInvalitPartitioningP (errCode range: 1090-1099) (errcode range: 1110-1119)         #JA20161209 #AK20161228
  # -stopOnInvalidBoolean (errCode range: 1120-1129)
  # -stopOnInvalidNumber (errCode range: 1130-1139)
#Second section of ParameterChecks_CoopGame.R contains check helper functions:
  # -getEmptyParamCheckResult
  # -stopOnParamCheckError

##First Section: START
#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidGameVectorA
#' @description stopOnInvalidGameVectorA checks if game vector A 
#'  is specified as parameter correctly.
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes stop otherwise.
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/A
#' @template param/n
#' @export stopOnInvalidGameVectorA
#' @examples
#' #Create empty object for check result:
#'  #paramCheckResult=getEmptyParamCheckResult()
#' #Define invalid game vector A (not numeric):
#'  #A=c("0","0","0","60","60","60","72")
#' #Check if game vector A is valid:
#'  #stopOnInvalidGameVectorA(paramCheckResult,A)
#' #Result:
#' # Error in stopOnParamCheckError(paramCheckResult):
#' # Error Code 1002: Type of game vector is not numeric
#'
#' #Define valid game vector A:
#'  #A=c(0,0,0,60,60,60,72)
#' #Check if game vector A is invalid:
#'  #stopOnInvalidGameVectorA(paramCheckResult,A)
#' #Result - no stop was caused therefore no error with game vector A was detected.

stopOnInvalidGameVectorA=function(paramCheckResult, A, n=NULL){
  checkResult=getEmptyParamCheckResult()
  numberOfPlayersA=log2(length(A)+1)
  #Check if A is null
  if(is.null(A)){
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1000)
    #Check if number of players deduced from A is valid integer
  }else if(numberOfPlayersA%%1!=0){
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1001)
    #Check if game vector A is numeric
  }else if(!is.numeric(A)){
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1002)
    #Check if n is specified if game vector A has n players
  }else if(!is.null(n)){
    if(numberOfPlayersA!=n){
      SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1003)
    }
  }else if( (length(unique(A)) == 1 & A[1]==0) ){
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1004)
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}


#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidGrandCoalitionN
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/GrandCoalitionN
#' @export stopOnInvalidGrandCoalitionN
#' @description stopOnInvalidGrandCoalitionN checks if grand coalition N is specified 
#' correctly and causes stop otherwise.
#' @examples
#' #Create empty object for check result:
#'  #paramCheckResult=getEmptyParamCheckResult()
#' #Execute check with invalid grand coalition N=c("1","2","3","4","5") (as not numeric):
#'  #stopOnInvalidGrandCoalitionN(paramCheckResult, N=c("1","2","3","4","5"))
#' #Result:
#'  #Fehler in stopOnParamCheckError(paramCheckResult) :
#'  #Error Code 1011: Grand coalition vector N is invalid as not numeric
#'
#' #Execute check with valid grand coalition N=c(1,2,3,4,5) (as not numeric):
#'  #stopOnInvalidGrandCoalitionN(paramCheckResult, N=c(1,2,3,4,5))
#' #Result - no stop was executed as no error detected at check of grand coalition N

stopOnInvalidGrandCoalitionN=function(paramCheckResult, N){
  checkResult=getEmptyParamCheckResult()
  numberOfPlayersN=length(N)
  if(is.null(N)){
    # @templateVar ERRCODE01 1010
    # @templateVar ERRMSG01 Grand coalition vector N is invalid as 'NULL'
    #checkResult$errMessage="Grand coalition vector N is invalid as 'NULL'"
    #checkResult$errCode=1010
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1010)
    #Check if coalition vector S is numeric
  }else if(!is.numeric(N)){
    # checkResult$errMessage="Grand coalition vector N is invalid as not numeric"
    # checkResult$errCode=1011
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1011)
    #In case grand coalition N is specified, check ...
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}


#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidCoalitionS
#' @description stopOnInvalidCoalitionS checks if coalition S as subset of grand coalition N 
#'              is specified correctly and causes stop otherwise.
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/S
#' @template param/GrandCoalitionN
#' @template param/n
#' @template param/A
#' @export stopOnInvalidCoalitionS
#' @examples
#' #Create empty object for check result:
#'  #paramCheckResult=getEmptyParamCheckResult()
#' #Execute check with invalid coalition S=c("1","2","3") (as not numeric):
#'  #stopOnInvalidCoalitionS(paramCheckResult, S=c("1","2","3"))
#' #Result:
#'  #Fehler in stopOnParamCheckError(paramCheckResult) :
#'  #Error Code 1021: Coalition vector S is invalid as not numeric
#'
#' #Execute check with invalid coalition S=c(1,2,3,4,5) and N=c(1,2,3) (as S no subset of N):
#'  #stopOnInvalidCoalitionS(paramCheckResult, S=c(1,2,3,4,5), N=c(1,2,3))
#' #Result:
#'  #Fehler in stopOnParamCheckError(paramCheckResult) :
#'  #Error Code 1022: Coalition vector S is no subset of grand coalition N'
#' #Execute check with valid coalition S=c(1,2,3) and N=c(1,2,3,4,5):
#'  #stopOnInvalidCoalitionS(paramCheckResult, S=c(1,2,3), N=c(1,2,3,4,5))
#' #Result - no stop was executed as no error detected at check of coalition
#' # S as subset of grand coalition N

stopOnInvalidCoalitionS=function(paramCheckResult, S,N=NULL,n=NULL,A=NULL){
  checkResult=getEmptyParamCheckResult()
  numberOfPlayersS=length(S)
  numberOfPlayersN=length(N)
  #Check if S is 'NULL'
  if(is.null(S)){
    # checkResult$errMessage="Coalition vector S is invalid as 'NULL'"
    # checkResult$errCode=1020
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1020)
    #Check if coalition vector S is numeric
  }else if(!is.numeric(S)){
    # checkResult$errMessage="Coalition vector S is invalid as not numeric"
    # checkResult$errCode=1021
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1021)
    #In case grand coalition N is specified, check ...
  }else if(!is.null(N)){
    #if S subset of N
    if(!all(S %in% N)){
      # checkResult$errMessage="Coalition vector S is no subset of grand coalition N'"
      # checkResult$errCode=1022
      SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1022)
    }
  }else if(!is.null(n)){
    if((length(S)>n)){
      # checkResult$errCode <- 1023
      # checkResult$errMessage <- "The number of players in S cannot be greater than the number of players in N"
      SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1023)
    }
  }else if(!is.null(A)){
    if(is.numeric(A) && (max(S)>getNumberOfPlayers(A))){
      SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1024)
    }
  }

  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}



#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidNumberOfPlayers
#' @description stopOnInvalidNumberOfPlayers checks if number of players is 
#' specified correctly and causes stop otherwise.
#' @export stopOnInvalidNumberOfPlayers
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/n
#' @examples
#' #Define invalid number of players n=1 (as too low needs to be at least 2):
#'  #n=1
#' #Create empty object for check result:
#'  #paramCheckResult=getEmptyParamCheckResult()
#' #Execute check with invalid number of players n=1 (as too low needs to be at least 2):
#'  #stopOnInvalidNumberOfPlayers(paramCheckResult, n=1)
#' #Result:
#'  #Fehler in stopOnParamCheckError(paramCheckResult) :
#'  #Error Code 1050: Number of players is invalid as below 2.
#'
#' #Execute check with invalid number of players n=1 (as too big must not be bigger than 20):
#'  #stopOnInvalidNumberOfPlayers(paramCheckResult, n=21)
#' #Result:
#'  #Fehler in stopOnParamCheckError(paramCheckResult) :
#'  #Error Code 1051: Number of players is invalid as above 20.
#'
#' #'#Execute check with valid number of players n=10 (as 1<n<21):
#'  #stopOnInvalidNumberOfPlayers(paramCheckResult, n=10)
#' #Result - no stop was executed as no error detected at check of number of players n

stopOnInvalidNumberOfPlayers=function(paramCheckResult, n){
  checkResult=getEmptyParamCheckResult()
  if(n<2){
    # checkResult$errMessage="Number of players is invalid as below 2."
    # checkResult$errCode=1050
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1050)
  }else if(n>20){
    # checkResult$errMessage="Number of players is invalid as above 20."
    # checkResult$errCode=1051
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1051)
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}


#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidIndex
#' @description stopOnInvalidIndex checks if coalition function 
#' (in the form of either v or A) is specified correctly and causes stop otherwise.
#' @export stopOnInvalidIndex
#' @template author/JA
#' @template param/paramCheckResult
#' @param index index which is proved to be valid index
#' @template param/n
#' @examples
#' #Create empty object for check result:
#'  #paramCheckResult=getEmptyParamCheckResult()
#' #Execute check with invalid index=8 specified for game vector 
#' #A=c(1:7) (as number of players derived from A creates conflict):
#'  #stopOnInvalidIndex(paramCheckResult, index=8, n=3)
#' #Result:
#'  #Fehler in stopOnParamCheckError(paramCheckResult) :
#'  #Error Code 1072: Index is within the wrong range according to specified number of players n.
#'
#' #Execute check with valid index=5 specified for game vector A=c(1:7):
#'  #stopOnInvalidIndex(paramCheckResult, index=5, n=3)
#' #Result - no stop was executed as no error detected at check of parameter index.
#'
#' #Execute check with invalid index="5" (as not numeric) specified for game vector A=c(1:7):
#'  #stopOnInvalidIndex(paramCheckResult, index="5", n=3)
#' #Result:
#'  #Fehler in stopOnParamCheckError(paramCheckResult) :
#'  #Error Code 1071: Index is 'not numeric'.

stopOnInvalidIndex=function(paramCheckResult, index, n=NULL){
  checkResult=getEmptyParamCheckResult()
  if(is.null(index)){
    # checkResult$errMessage="Index is 'NULL'."
    # checkResult$errCode=1071
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1070)
  }else if(!is.numeric(index)){
    # checkResult$errMessage="Index is 'not numeric'."
    # checkResult$errCode=1071
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1071)
  }else if(!is.null(n)&is.numeric(n)){
    numberOfCoalitions=(2^n)-1
    if(!(1<=index & index<=numberOfCoalitions)){
      # checkResult$errMessage="Index is within the wrong range according to specified number of players n."
      # checkResult$errCode=1072
      SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1072)
    }
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}

#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidNChooseB
#' @description stopOnInvalidNChooseB checks if definition of n choose b is 
#' specified correctly and causes stop otherwise.
#' @export stopOnInvalidNChooseB
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/n
#' @param b number of players in subset
#' @examples
#' #Create empty object for check result:
#'  #paramCheckResult=getEmptyParamCheckResult()
#' #Execute check with valid definition of n=3 choose b=2:
#'  #stopOnInvalidNChooseB(paramCheckResult, n=3, b=2)
#' #Result - no stop was executed as no error detected
#' #at definition check of n choose b no error was detected.
#'
#' #Execute check with invalid definition of n=2 choose b=3 (as n<b):
#'  #stopOnInvalidNChooseB(paramCheckResult, n=2, b=3)
#' #Result:
#'  #Fehler in stopOnParamCheckError(paramCheckResult) :
#'  #Error Code 1084: Number of involved players 'b' is greater than of players 'n'

stopOnInvalidNChooseB=function(paramCheckResult, n, b){
  checkResult=getEmptyParamCheckResult()
  if(is.null(n)){
    # @templateVar ERRCODE01 1080
    # @templateVar ERRMSG01 Number of players 'n' is 'NULL'
    #  checkResult$errMessage="Number of players 'n' is 'NULL'"
    # checkResult$errCode=1080
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1080)
  }else if(is.null(b)){
    # checkResult$errMessage="Number of players 'b' is 'NULL'"
    # checkResult$errCode=1081
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1081)
  }else if(!is.numeric(n)){
    # checkResult$errMessage="Number of players 'n' is 'not numeric'"
    # checkResult$errCode=1082
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1082)
  }else if(!is.numeric(b)){
    # checkResult$errMessage="Number of players 'b' is 'not numeric'"
    # checkResult$errCode=1083
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1083)
  }else if(n<b){
    # checkResult$errMessage="Number of involved players 'b' is greater than of players 'n'"
    # checkResult$errCode=1084
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1084)
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}


#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidBoolean
#' @title stopOnInvalidBoolean - check definition is a boolean
#' @description stopOnInvalidBoolean checks definition is the parameter a boolean
#' @export stopOnInvalidBoolean
#' @template author/FM
#' @template param/paramCheckResult
#' @param boolean parameter which is checked to prove it is valid boolean.
#' @examples
#' #Create empty object for check result:
#'  #paramCheckResult=getEmptyParamCheckResult()
#'
#' #Execute check with invalid partition boolean=c(0,1,1,1,0,0,0):
#'  #stopOnInvalidBoolean(paramCheckResult, boolean)


stopOnInvalidBoolean<-function(paramCheckResult, boolean){
  checkResult=getEmptyParamCheckResult();
  if(!is.logical(boolean)){
    # checkResult$errMessage="Parameter is not a boolean value";
    # checkResult$errCode=1120
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1120)
  }else if(length(boolean)>1){
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1121)
  }
  
  eval.parent(substitute(paramCheckResult<-checkResult));
  stopOnParamCheckError(paramCheckResult);
}

#  Version 1.0
#  Date: 20170303
#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidNumber
#' @title stopOnInvalidNumber - check definition is a number
#' @description stopOnInvalidNumber checks definition is the parameter a number
#' @export stopOnInvalidNumber
#' @template author/FM
#' @template param/paramCheckResult
#' @param number which is proved to be valid number
#' @examples
#' #Create empty object for check result:
#'  #paramCheckResult=getEmptyParamCheckResult()
#'
#' #Execute check with invalid partition number=c(0,1,1,1,0,0,0):
#'  #stopOnInvalidNumber(paramCheckResult, number)
#' #Result:
#'  #Error in stopOnParamCheckError(paramCheckResult) :
#'  #Error Code 1120: parameter number is invalid because it is a vector
#'
#' #Execute check with invalid partition number=1:
#'  
#'  #stopOnInvalidNumber(paramCheckResult, number)
#' #Result - no stop was executed as no error detected at check of parameter number
#'
stopOnInvalidNumber<-function(paramCheckResult, number){
  checkResult=getEmptyParamCheckResult();
  
  if(!is.numeric(number)){
    # checkResult$errMessage="Parameter P for partitioning is not numeric";
    # checkResult$errCode=1130
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1130)
  }else if(length(number)>1){
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1131)
  }
  
  eval.parent(substitute(paramCheckResult<-checkResult));
  stopOnParamCheckError(paramCheckResult);
}


#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidCoalitionFunctionV
#' @description stopOnInvalidCoalitionFunctionV checks if coalition function v is specified correctly and causes stop otherwise.
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/v
#' @export stopOnInvalidCoalitionFunctionV
#' @examples
#' #Create valid dummy coalition function 'cFuncDummy':
#'  #cFuncDummy=function(S){
#'  #  return(length(S))
#'  #}
#' #Create empty object for check result:
#'  #paramCheckResult=getEmptyParamCheckResult()
#' #Execute check with valid coalition function 'cFuncDummy':
#'  #stopOnInvalidCoalitionFunctionV(paramCheckResult, v=cFuncDummy)
#'  #Result - no stop was executed as no error detected at check of coalition 
#'  #function 'cFuncDummy' was detected.
#'
#' #Execute check with invalid coalition function v=c(1:4) (as v no function but vector):
#'  #stopOnInvalidCoalitionFunctionV(paramCheckResult, v=c(1:4))
#' #Result:
#' #Fehler in stopOnParamCheckError(paramCheckResult) :
#' #Error Code 1040: Coalition function v is no function

stopOnInvalidCoalitionFunctionV=function(paramCheckResult, v){
  checkResult=getEmptyParamCheckResult()
  if(!is.function(v)){
    # checkResult$errMessage="Coalition function v is no function"
    # checkResult$errCode=1040
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1040)
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}

#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidLeftRightCFuncGlove
#' @description stopOnInvalidLeftRightCFuncGlove checks if L (left gloves) 
#'  and R (right gloves) are specified as parameter correctly (also regarding grand coalition).
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes stop otherwise.
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/L
#' @template param/R
#' @template param/GrandCoalitionN
#' @export stopOnInvalidLeftRightCFuncGlove
#' @examples
#' #paramCheckResult=getEmptyParamCheckResult()
#' 
#' #valid quota
#' #stopOnInvalidLeftRightCFuncGlove(paramCheckResult, L=c(1,3),R=c(2),N=c(1,2,3))
#' 
#' #invalid quota
#' #stopOnInvalidLeftRightCFuncGlove(paramCheckResult, L=c(1,4),R=c(2),N=c(1,2,3))

stopOnInvalidLeftRightCFuncGlove=function(paramCheckResult, L, R, N){
  
  checkResult=getEmptyParamCheckResult()
  #check whether given numer of players in L und R is correct
  #and whether L und R are disjoint
  unionLR = sort(union(L,R))
  if(!setequal(unionLR, N)){
    # checkResult$errMessage = "Not all players in L and R included."
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1138)
  }else if(!(length(L) > 0)){
    # checkResult$errMessage = "L must have size > 0."
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1139)
  }else if(!(length(R)) > 0){
    # checkResult$errMessage = "R must have size > 0."
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1140)
  }else if(length(intersect(L, R)) > 0){
    # checkResult$errMessage = "L and R have to be disjoint sets."
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1141)
  }
  
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}

#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidVetoPlayer
#' @description stopOnInvalidQuota checks if vetoPlayer
#'  is specified as parameter correctly.
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes stop otherwise.
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/vetoPlayer
#' @export stopOnInvalidVetoPlayer
#' @examples
#' #paramCheckResult=getEmptyParamCheckResult()
#' 
#' #valid quota
#' #stopOnInvalidVetoPlayer(paramCheckResult, vetoPlayer=3)
#' 
#' #invalid quota
#' #stopOnInvalidVetoPlayer(paramCheckResult, vetoPlayer="3")
stopOnInvalidVetoPlayer=function(paramCheckResult, vetoPlayer) {
  checkResult=getEmptyParamCheckResult()
  if (is.null(vetoPlayer)) {
    # checkResult$errMessage="At least one veto player has to be specified"
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1143)
  } else if (length(vetoPlayer) > 1) {
    # checkResult$errMessage="Only a single veto player is allowed for this game"
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1144)
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}

#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidQuota
#' @description stopOnInvalidQuota checks if qutoa
#'  is specified as parameter correctly.
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes stop otherwise.
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/q
#' @export stopOnInvalidQuota
#' @examples
#' #paramCheckResult=getEmptyParamCheckResult()
#' 
#' #valid quota
#' #stopOnInvalidQuota(paramCheckResult, q=3)
#' 
#' #invalid quota
#' #stopOnInvalidQuota(paramCheckResult, q="3")
#' 
stopOnInvalidQuota=function(paramCheckResult,q){
  checkResult=getEmptyParamCheckResult()
  if(is.null(q)){
    checkResult$errCode=1030
    checkResult$errMessage="Invalid quota as q is NULL"
  }else if (q < 0) {
    checkResult$errCode=1031
    checkResult$errMessage="Quota must be greater than zero!"
  }else if(!is.numeric(q)){
    checkResult$errCode=1032
    checkResult$errMessage="Quota must be numeric!"
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}

#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidWeightVector
#' @description stopOnInvalidWeightVector checks if dictator
#'  is specified as parameter correctly.
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes stop otherwise.
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/w
#' @template param/n
#' @export stopOnInvalidWeightVector
#' @examples
#' #paramCheckResult=getEmptyParamCheckResult()
#' #stopOnInvalidWeightVector(paramCheckResult, n=3, w=c(1,2,3))
stopOnInvalidWeightVector=function(paramCheckResult,n,w){
  checkResult=getEmptyParamCheckResult()
  if(n > length(w)){
    # checkResult$errCode=1110
    # checkResult$errMessage="Number of weights must be equal or greater than number of players in coalition!"
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1110)
  }else if(!is.numeric(w)){
    # checkResult$errCode=1111
    # checkResult$errMessage="Invalid weight vector as w is numeric"
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1111)
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}

#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidEstate
#' @description stopOnInvalidBankruptcy checks if estate
#'  is specified as parameter correctly.
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes stop otherwise.
#' @template author/JS
#' @template param/paramCheckResult
#' @template param/E
#' @export stopOnInvalidEstate
#' @examples
#' #paramCheckResult=getEmptyParamCheckResult()
#' 
#' #valid estate
#' #stopOnInvalidEstate(paramCheckResult, E=3)
#' 
#' #invalid estate
#' #stopOnInvalidEstate(paramCheckResult, E="3")
#' 
stopOnInvalidEstate=function(paramCheckResult,E){
  checkResult=getEmptyParamCheckResult()
  if (E < 0) {
    # checkResult$errCode=1150
    # checkResult$errMessage="Estate must be nonnegative!"
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1150)
  }else if(!is.numeric(E)){
    # checkResult$errCode=1151
    # # checkResult$errMessage="Estate must be numeric!"
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1151)
  }else if(is.null(E)){
    # checkResult$errCode=1152
    # checkResult$errMessage="Invalid estate as E is NULL"
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1152)
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}

#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidClaimsVector
#' @description stopOnInvalidWeightVector checks if claims vector
#'  in a bankruptcy game is specified correctly.
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes stop otherwise.
#' @template author/JS
#' @template param/paramCheckResult
#' @template param/d
#' @template param/n
#' @export stopOnInvalidClaimsVector
#' @examples
#' #paramCheckResult=getEmptyParamCheckResult()
#' #stopOnInvalidClaimsVector(paramCheckResult, n=3, d=c(1,2,3))
stopOnInvalidClaimsVector=function(paramCheckResult,n,d){
  checkResult=getEmptyParamCheckResult()
  if(n != length(d)){
    checkResult$errCode=1160
    checkResult$errMessage="Number of claims must equal the number of players in the bankruptcy game!"
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1160)
  }else if(!is.numeric(d)){
    # checkResult$errCode=1161
    # checkResult$errMessage="Invalid claims vector as d must be numeric"
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1161)
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}


#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidDictator
#' @description stopOnInvalidDictator checks if dictator
#'  is specified as parameter correctly.
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes stop otherwise.
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/dictator
#' @template param/n
#' @export stopOnInvalidDictator
#' @examples
#' #paramCheckResult=getEmptyParamCheckResult()
#' 
#' #valid
#' #stopOnInvalidDictator(paramCheckResult,dictator=3,n=3)
#' 
#' #invalid as number of players is only 3 but dictator has number 4.
#' #stopOnInvalidDictator(paramCheckResult,dictator=4,n=3)
stopOnInvalidDictator=function(paramCheckResult,dictator,n=NULL){
  checkResult=getEmptyParamCheckResult()
  #Check if G is igraph object
  if(is.null(dictator)){
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1093)
  }else if(!is.numeric(dictator)){
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1091)
  }else if(length(dictator)!=1){
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1090)
  }else if(!is.null(n)){
    if(length(intersect(dictator,c(1:n)))!=1){
      SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1092)
    }
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}


#' @template Templates/ParameterCheck
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidAllocation
#' @description stopOnInvalidAllocation checks if allocation
#'  is specified as parameter correctly.
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes stop otherwise.
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/x
#' @template param/n
#' @template param/A
#' @export stopOnInvalidAllocation
#' @examples
#' #paramCheckResult=getEmptyParamCheckResult()
#' 
#' #valid
#' #stopOnInvalidAllocation(paramCheckResult,x=c(1,2,3),n=3)
#' 
#' #invalid as number of players is 4 but allocation only has 3 elements.
#' #stopOnInvalidAllocation(paramCheckResult,x=c(1,2,3),n=4)
stopOnInvalidAllocation=function(paramCheckResult,x,n=NULL,A=NULL){
  checkResult=getEmptyParamCheckResult()
  if(is.null(x)){
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1100)
  }else if(!is.numeric(x)){
    SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1101)
  }else if(!is.null(n)){
    if(length(x)!=n){
      SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1102)
    }
  }else if(!is.null(A)){
    if(is.numeric(A)&&(length(x)!=getNumberOfPlayers(A))){
      SYSDATA_OBJECTS$fillParamCheckResult(checkResult,1103)
    }
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}

##First Section: END

##Second Section: START

#' @name getEmptyParamCheckResult
#' @title getEmptyParamCheckResult for generating stucture according to parameter check results
#' @description Returns a defined data structure which is intended to store an error code 
#' and a message after the check of function parameters was executed.
#'  In case parameter check was successfull the error code has the value '0'
#'  and the message is 'NULL'.
#' @family ParameterChecks_CoopGame
#' @aliases getEmptyParamCheckResult
#' @export getEmptyParamCheckResult
#' @template author/JA
#' @return list with 2 elements named errCode which contains an integer
#' representing the error code ('0' if no error) and errMessage for the error message ('NULL' if no error)
#' @examples
#' initParamCheck_example=function(numberOfPlayers){
#'  paramCheckResult=getEmptyParamCheckResult()
#'  if(numberOfPlayers!=3){
#'    paramCheckResult$errMessage="The number of players is not 3 as expected"
#'    paramCheckResult$errCode=1
#'  }
#'  return(paramCheckResult)
#' }
#'
#' initParamCheck_example(3)
#' #Output:
#' #$errCode
#' #[1] 0
#' #$errMessage
#' #NULL
#'initParamCheck_example(4)
#' #Output:
#' #$errCode
#' #[1] 1
#' #
#' #$errMessage
#'
getEmptyParamCheckResult=function(){
  #Return code which gives further information for testing
  #Initialized with 0 means no error
  retCode=0
  #Message which is shown user for error
  #Initialized with NULL as no error
  errMsg=NULL
  return(list(errCode=retCode,errMessage=errMsg))
}


#' @name stopOnParamCheckError
#' @title stopOnParamCheckError - stop and create error message on error
#' @description stopOnParamCheckError causes and creates error message 
#' on base of paramCheckResult parameter where 'errCode' <> '0' in case error occured.
#' @family ParameterChecks_CoopGame
#' @aliases stopOnParamCheckError
#' @export stopOnParamCheckError
#' @template author/JA
#' @template param/paramCheckResult
#' @examples
#' #Create object for storing parameter check result:
#'  paramCheckResult=getEmptyParamCheckResult()
#' #Define invalid game vector A which is not of right length:
#' #A=c(1:8)
#' #Check result with error gets stored to paramCheckResult and stop gets caused:
#' #stopOnInvalidGameVectorA(paramCheckResult,A=A,n=3)
#' #check result with error was stored to paramCheckResult:
#' #paramCheckResult
#' #Result:
#' # $errCode
#' # [1] 1001
#' #
#' # $errMessage
#' # [1] "Number of elements in A is invalid"

stopOnParamCheckError=function(paramCheckResult){
  #boolean variable: TRUE => stop is executed, FALSE => no stop is executed
  boolStop=FALSE

  if(paramCheckResult$errCode!=0){
    #in case error occured set boolStop to true
    boolStop=TRUE
  }

  #Execute stop in case boolStop was set to TRUE in previous steps and generate adequate error message
  if(boolStop){
    errMsg=paste("Error Code ",paramCheckResult$errCode,": ",paramCheckResult$errMessage, sep="")
    stop(errMsg)
  }
}

##Second Section: END



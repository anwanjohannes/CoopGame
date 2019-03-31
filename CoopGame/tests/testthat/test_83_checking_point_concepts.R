boolSkip=T

valueConcepts<-c(
  "banzhafValue",
  "rawBanzhafValue",
  "disruptionNucleolus",
  "gatelyValue",
  "lexicalGatelyValue",
  "modiclus",
  "nucleolus",
  "perCapitaNucleolus",
  "prenucleolus",
  "proportionalNucleolus",
  "publicGoodValue",
  "publicHelpChiValue",
  "shapleyValue",
  "simplifiedModiclus",
  "tauValue"
)

indexConcepts<-c(
  "rawBanzhafIndex",
  "normalizedBanzhafIndex",
  "nonNormalizedBanzhafIndex",
  "baruaChakravartySarkarIndex",
  "colemanInitiativePowerIndex",
  "colemanPreventivePowerIndex",
  "deeganPackelIndex",
  "johnstonIndex",
  "koenigBraeuningerIndex",
  "nevisonIndex",
  "publicGoodIndex",
  "publicHelpChiIndex",
  "raeIndex"
)

setConcepts<-c(
  "centroidCore",
  "centroidCoreCover",
  "centroidImputationSet",
  "centroidReasonableSet",
  "centroidWeberSet",
  "coreVertices",
  "coreCoverVertices",
  "imputationsetVertices",
  "reasonableSetVertices",
  "webersetVertices"
  
  
)


gameProperties<-c(
  "is1ConvexGame",
  "isAdditiveGame",
  "isBalancedGame",
  "isConvexGame",
  "isDegenerateGame",
  "isEssentialGame",
  "isMonotonicGame",
  "isNonnegativeGame",
  "isQuasiBalancedGame",
  "isSemiConvexGame",
  "isSimpleGame",
  "isSuperadditiveGame",
  "isSymmetricGame",
  "isWeaklySuperadditiveGame"
)

execRandomTests<-function(vectorWithFunctions,n,expectedType){
  for(i in 1:length(vectorWithFunctions)){
    for(j in 1:100){
     tryCatch({
      P=sample(1:20,2^n-1,replace = TRUE)
      vFun=match.fun(vectorWithFunctions[i])
      res=vFun(P)
      if(typeof(res)!=expectedType){
        print(paste(c("Unexpected return:",vectorWithFunctions[i],":",res,P),collapse = ","))
      }
     }, error=function(e){
       print(paste(c("Error with:",vectorWithFunctions[i],":",P),collapse = ","))
       NULL
     }, warning=function(){
        NULL
     })
    }
  }
}

test_that("Check 83.1 - Check game properties" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  execRandomTests(gameProperties,3,"logical")
  execRandomTests(gameProperties,4,"logical")
  execRandomTests(gameProperties,5,"logical")
  execRandomTests(gameProperties,6,"logical")
  
})

test_that("Check 83.2 - Value concepts" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  execRandomTests(valueConcepts,3,"numeric")
  
})

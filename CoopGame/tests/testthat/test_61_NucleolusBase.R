boolSkip=F


test_that("Check 61.1 - testing method initLPRows of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  A=c(1, 1, 1, 2, 3, 4, 5)
  nb=new("Nucleolus",A=A)
  initLPRows(nb)
  
  rlb=c(1, 1, 1, 2, 3, 4, 5)
  rub=c(Inf, Inf, Inf, Inf, Inf, Inf,5)
  rtype=c(2, 2, 2, 2, 2, 2, 5)
  
  expect_equal(as.numeric(getRlb(nb@LPCoopGameUtils)),rlb)
  expect_equal(as.numeric(getRub(nb@LPCoopGameUtils)),rub)
  expect_equal(as.numeric(getRtype(nb@LPCoopGameUtils)),rtype)

})

test_that("Check 61.2 - testing method initLPMatrix of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  A=c(1, 1, 1, 2, 3, 4, 5)
  nb=new("Nucleolus",A=A)
  initLPMatrix(nb)
  
  excess=c(rep(1,6),0)
  coeffMat=createBitMatrix(n=3,excess)
  expect_equal(getMatrix(nb@LPCoopGameUtils),coeffMat)
  
  
})

test_that("Check 61.3 - testing method initLPBndsObjCoefs of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  A=c(1, 1, 1, 2, 3, 4, 5)
  nb=new("Nucleolus",A=A)
  initLPBndsObjCoefs(nb)
  
  clb=c(1,1,1,-Inf)
  cub=c(Inf, Inf, Inf, Inf)
  ctype=c(4,4,4, 1)
  
  expect_equal(getClb(nb@LPCoopGameUtils),clb)
  expect_equal(getCub(nb@LPCoopGameUtils),cub)
  expect_equal(getCtype(nb@LPCoopGameUtils),ctype)
  
})

test_that("Check 61.4 - testing constructor function Nucleolus of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A=c(1, 1, 1, 2, 3, 4, 5)
  nb=new("Nucleolus",A)
  
  clb=c(1,1,1,-Inf)
  cub=c(Inf, Inf, Inf, Inf)
  ctype=c(4,4,4, 1)
  
  rlb=c(1, 1, 1, 2, 3, 4, 5)
  rub=c(Inf, Inf, Inf, Inf, Inf, Inf,5)
  rtype=c(2, 2, 2, 2, 2, 2, 5)

  
  excess=c(rep(1,6),0)
  coeffMat=createBitMatrix(n=3,excess)
  
  
  expect_equal(getClb(nb@LPCoopGameUtils),clb)
  expect_equal(getCub(nb@LPCoopGameUtils),cub)
  expect_equal(getCtype(nb@LPCoopGameUtils),ctype)
  
  
  expect_equal(as.numeric(getRlb(nb@LPCoopGameUtils)),rlb)
  expect_equal(as.numeric(getRub(nb@LPCoopGameUtils)),rub)
  expect_equal(as.numeric(getRtype(nb@LPCoopGameUtils)),rtype)
  
  expect_equal(getMatrix(nb@LPCoopGameUtils),coeffMat)
  
})


test_that("Check 61.5 - testing method getLPDualSolutionPos of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A=c(1, 1, 1, 2, 3, 4, 5)
  nb=new("Nucleolus",A)
  solveLP(nb@LPCoopGameUtils)
  expect_equal(getLPDualSolutionPos(nb),c(1,6))
})

test_that("Check 61.6 - testing method updateLPRows of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A=c(1, 1, 1, 2, 3, 4, 5)
  nb=new("Nucleolus",A)
  solveLP(nb@LPCoopGameUtils)
  updateLPRows(nb)
  
  rlb=c(1, 1, 1, 2, 3, 4, 5)
  rub=c(1, Inf, Inf, Inf, Inf, 4,5)
  rtype=c(5, 2, 2, 2, 2, 5, 5)
  
  expect_equal(as.numeric(getRlb(nb@LPCoopGameUtils)),rlb)
  expect_equal(as.numeric(getRub(nb@LPCoopGameUtils)),rub)
  expect_equal(as.numeric(getRtype(nb@LPCoopGameUtils)),rtype)
})




test_that("Check 61.7 - testing method updateLPMatrix of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A=c(1, 1, 1, 2, 3, 4, 5)
  nb=new("Nucleolus",A)
  solveLP(nb@LPCoopGameUtils)
  updateLPMatrix(nb)
  
  excess=rep(1,7)
  excess[c(1,6,7)]=0
  coeffMat=createBitMatrix(n=3,excess)
  
  expect_equal(getMatrix(nb@LPCoopGameUtils),coeffMat)
})

test_that("Check 61.7 - testing method updateLPBndsObjCoefs of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  expect_equal(TRUE,TRUE)
})

test_that("Check 61.8 - testing method updateNucleolusBase of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A=c(1, 1, 1, 2, 3, 4, 5)
  nb=new("Nucleolus",A)
  solveLP(nb@LPCoopGameUtils)
  updateNucleolusBase(nb)
  
  rlb=c(1, 1, 1, 2, 3, 4, 5)
  rub=c(1, Inf, Inf, Inf, Inf, 4,5)
  rtype=c(5, 2, 2, 2, 2, 5, 5)
  
  excess=rep(1,7)
  excess[c(1,6,7)]=0
  coeffMat=createBitMatrix(n=3,excess)
  
  expect_equal(getMatrix(nb@LPCoopGameUtils),coeffMat)
  
  expect_equal(as.numeric(getRlb(nb@LPCoopGameUtils)),rlb)
  expect_equal(as.numeric(getRub(nb@LPCoopGameUtils)),rub)
  expect_equal(as.numeric(getRtype(nb@LPCoopGameUtils)),rtype)
})


test_that("Check 61.9 - testing method getLPRowsBoundsFunc of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  A=c(1, 1, 1, 2, 3, 4, 5)
  nb=new("Nucleolus",A)
  solveLP(nb@LPCoopGameUtils)
  primal=getLPPrimalSolution(nb@LPCoopGameUtils)
  
  coeffMat=createBitMatrix(n=3,c(rep(1,6),1))
  result=apply(coeffMat,1,FUN = getLPRowsBoundsFunc,.Object=nb)
  
  expect_equal(result,c(1, 2, 2, 3, 3, 4, 5) )
})

test_that("Check 61.10 - testing method determineExcessCoefficients of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  A=c(1, 1, 1, 2, 3, 4, 5)
  nb=new("Nucleolus",A)
  excess=determineExcessCoefficients(nb)
  
  expect_equal(excess,c(rep(1,6),0))
})

test_that("Check 61.9 - testing method checkAbort of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  nb=new("Nucleolus",A=c(1, 1, 1, 2, 3, 4, 5))
  solveLP(nb@LPCoopGameUtils)
  expect_equal(isLPFeasible(nb@LPCoopGameUtils),TRUE)
  expect_equal(checkAbort(nb,1),FALSE)#1 only set for testing
  
  nb=new("Nucleolus",A=c(1, 1, 1, 2, 3, 4, 5))
  solveLP(nb@LPCoopGameUtils)
  expect_equal(checkAbort(nb,0),TRUE)
  
})

test_that("Check 61.9 - testing method calculateNucleolus of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  A=c(1, 1, 1, 2, 3, 4, 5)
  nb=new("Nucleolus",A)
 
  expect_equal(nucleolus(A),calculateNucleolus(nb))
})


test_that("Check 61.10 - testing method setA and getA of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  A=c(1, 1, 1, 2, 3, 4, 5)
  nb=new("Nucleolus",A)
  expect_equal(as.numeric(getA(nb)),A)
  
  A_new<-GameVector(rep(9,7))
  setA(nb)<-A_new
  
  expect_equal(getA(nb),A_new)
})

test_that("Check 61.11 - testing method getLPCoopGameUtils of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  A=c(1, 1, 1, 2, 3, 4, 5)
  nb=new("Nucleolus",A)
  lpCoopGameUtils<-getLPCoopGameUtils(nb)
  expect_equal(as.numeric(getRlb(lpCoopGameUtils)),A)
})

test_that("Check 61.12 - testing method getLPCoopGameUtils of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  A<-c(46125, 17437.5, 5812.5, 69187.5, 53812.5,30750,90000)
  expected_c=c(52687.5, 24468.8,12843.8)
  expect_equal(round(nucleolus(A),1),expected_c)
})

test_that("Check 61.13 - testing method getLPCoopGameUtils of class Nucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A<-c(0,0,0,0,0.68,0.24,0.75,0.26,0.51,0.07,1.03,1.53,1.02,0.75,1.89)
  expected_c=c(0.75, 0.48,0.18,0.47)
  expect_equal(round(nucleolus(A),2),expected_c)
})
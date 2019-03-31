boolSkip=F




test_that("Check 65.1 - testing init of S4 class LPCoopGameUtils" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A=c(0,0,0,0,9,10,12)
  tfac<-c(1,1,1,2,2,2,0)
  coeffMat<-createBitMatrix(3,tfac)
  lpMatrix<-new("LPMatrix",matrix=coeffMat)
  
  rlb<-A
  rub<-c(Inf,Inf,Inf,Inf,Inf,Inf, A[7])
  rtype<-c(2,2,2,2,2,2,5)
  lpRows<-new("LPRows",rlb = rlb,rub=rub,rtype = rtype)
  
  clb<-c(0,0,0,-Inf)
  ctype<-c(2,2,2,1)
  cub<-c(Inf,Inf,Inf,Inf)
  obj<-c(0,0,0,1)
  lpBndsObjCoefs<-new("LPBndsObjCoefs",clb=clb,cub=cub,ctype = ctype,obj=obj)
  
  
  lpCoopGameUtils=new("LPCoopGameUtils",LPBndsObjCoefs = lpBndsObjCoefs,LPRows = lpRows,LPMatrix = lpMatrix)
})

test_that("Check 65.2 - testing changeLPCoopGameUtilsObjDirToMin and changeLPCoopGameUtilsObjDirToMax" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  
  lpUtils<-new("LPCoopGameUtils")
  
  lpUtils@LPObjDir=999
  expect_equal(lpUtils@LPObjDir,999)
  
  changeLPCoopGameUtilsObjDirToMin(lpUtils)
  expect_equal(lpUtils@LPObjDir,GLP_MIN)
  
  lpUtils@LPObjDir=999
  expect_equal(lpUtils@LPObjDir,999)
  
  changeLPCoopGameUtilsObjDirToMax(lpUtils)
  expect_equal(lpUtils@LPObjDir,GLP_MAX)
})

test_that("Check 65.4 - testing getter and setter of class LPCoopGameUtils" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  A=c(0,0,0,0,9,10,12)
  tfac<-c(1,1,1,2,2,2,0)
  coeffMat<-createBitMatrix(3,tfac)
  lpMatrix<-new("LPMatrix",matrix=coeffMat)
  
  rlb<-A
  rub<-c(Inf,Inf,Inf,Inf,Inf,Inf, A[7])
  rtype<-c(2,2,2,2,2,2,5)
  lpRows<-new("LPRows",rlb = rlb,rub=rub,rtype = rtype)
  
  clb<-c(0,0,0,-Inf)
  ctype<-c(2,2,2,1)
  cub<-c(Inf,Inf,Inf,Inf)
  obj<-c(0,0,0,1)
  lpBndsObjCoefs<-new("LPBndsObjCoefs",clb=clb,cub=cub,ctype = ctype,obj=obj)
  
  lpUtils<-new("LPCoopGameUtils")
  
  setLPBndsObjCoefs(lpUtils)<-lpBndsObjCoefs
  setLPRows(lpUtils)<-lpRows
  setLPMatrix(lpUtils)<-lpMatrix
  
  expect_equal(lpMatrix,getLPMatrix(lpUtils))
  expect_equal(lpRows,getLPRows(lpUtils))
  expect_equal(lpBndsObjCoefs,getLPBndsObjCoefs(lpUtils))
  
})

test_that("Check 65.5 - testing getter and setter of class LPCoopGameUtils" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  A=c(0,0,0,0,9,10,12)
  tfac<-c(1,1,1,2,2,2,0)
  coeffMat<-createBitMatrix(3,tfac)
  lpMatrix<-new("LPMatrix",matrix=coeffMat)
  
  rlb<-A
  rub<-c(Inf,Inf,Inf,Inf,Inf,Inf, A[7])
  rtype<-c(2,2,2,2,2,2,5)
  lpRows<-new("LPRows",rlb = rlb,rub=rub,rtype = rtype)
  
  clb<-c(0,0,0,-Inf)
  ctype<-c(2,2,2,1)
  cub<-c(Inf,Inf,Inf,Inf)
  obj<-c(0,0,0,1)
  lpBndsObjCoefs<-new("LPBndsObjCoefs",clb=clb,cub=cub,ctype = ctype,obj=obj)
  
  
  
  .Object<-new("LPCoopGameUtils")
  
  setLPBndsObjCoefs(.Object)<-lpBndsObjCoefs
  setLPRows(.Object)<-lpRows
  setLPMatrix(.Object)<-lpMatrix
  updateLPCoopGameUtils(.Object)
  solveLP(.Object)
  
  prob <- initProbGLPK()
  setObjDirGLPK(prob, GLP_MIN)
  ia <- rep(1:7, each = 4)
  ja <- rep(1:(4), 7)
  
  addRowsGLPK(prob, 7)
  addColsGLPK(prob, 4)
  setRowsBndsGLPK(prob, 1:7, rlb, rub, rtype)
  setColsBndsObjCoefsGLPK(prob, 1:(4), clb, cub, obj, ctype)
  loadMatrixGLPK(prob, 7 * (4), ia, ja, as.vector(t(coeffMat)))
  solveSimplexGLPK(prob)
  
  expect_equal(getRowsDualGLPK(prob),getLPDualSolution(.Object))
  expect_equal(getRowsDualGLPK(prob),getLPDualSolution(.Object))
  expect_equal(getRowsDualGLPK(prob),getLPDualSolution(.Object))
  
})

test_that("Check 65.6 - testing getter and setter of class LPCoopGameUtils" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  lpUtils<-new("LPCoopGameUtils")
  
  A=c(0,0,0,0,9,10,12)
  tfac<-c(1,1,1,2,2,2,0)
  coeffMat<-createBitMatrix(3,tfac)
  setMatrix(lpUtils)<-coeffMat
  expect_equal(coeffMat,getMatrix(lpUtils))
  
  
  rlb<-A
  setRlb(lpUtils)<-rlb
  expect_equal(rlb,getRlb(lpUtils))
  
  rub<-c(Inf,Inf,Inf,Inf,Inf,Inf, A[7])
  setRub(lpUtils)<-rub
  expect_equal(rub,getRub(lpUtils))
  
  rtype<-c(2,2,2,2,2,2,5)
  setRtype(lpUtils)<-rtype
  expect_equal(rtype,getRtype(lpUtils))
  
  clb<-c(0,0,0,-Inf)
  setClb(lpUtils)<-clb
  expect_equal(clb,getClb(lpUtils))
  
  ctype<-c(2,2,2,1)
  setCtype(lpUtils)<-ctype
  expect_equal(ctype,getCtype(lpUtils))
  
  cub<-c(Inf,Inf,Inf,Inf)
  setCub(lpUtils)<-cub 
  expect_equal(rlb,getRlb(lpUtils))
  
  
  obj<-c(0,0,0,1)
  setObj(lpUtils)<-obj
  expect_equal(obj,getObj(lpUtils))
})





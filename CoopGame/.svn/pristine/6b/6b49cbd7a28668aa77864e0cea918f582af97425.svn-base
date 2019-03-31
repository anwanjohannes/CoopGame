boolSkip=F




test_that("Check 64.1 - testing init of S4 class LPBndsObjCoefs" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  clb<-c(rep(0,3),-Inf)
  cub<-rep(Inf,4)
  ctype<-c(rep(GLP_LO,3),GLP_FR)
  obj<-c(rep(0,3),1)
  
  lpBndsObjCoefs<-new("LPBndsObjCoefs",clb=clb,cub=cub,ctype=ctype,obj=obj)
  expect_equal(lpBndsObjCoefs@clb,clb)
  expect_equal(lpBndsObjCoefs@cub,cub)
  expect_equal(lpBndsObjCoefs@ctype,ctype)
  expect_equal(lpBndsObjCoefs@obj,obj)
})

test_that("Check 64.2 - testing getter and setter of class LPMatrix" ,{
  if(boolSkip){
    skip("Test was skipped")
  }

  lpBndsObjCoefs<-new("LPBndsObjCoefs")
  
  clb<-c(rep(0,3),-Inf)
  cub<-rep(Inf,4)
  ctype<-c(rep(GLP_LO,3),GLP_FR)
  obj<-c(rep(0,3),1)
  
  setClb(lpBndsObjCoefs)<-clb
  setCub(lpBndsObjCoefs)<-cub
  setCtype(lpBndsObjCoefs)<-ctype
  setObj(lpBndsObjCoefs)<-obj
  
  expect_equal(getClb(lpBndsObjCoefs),clb)
  expect_equal(getCub(lpBndsObjCoefs),cub)
  expect_equal(getCtype(lpBndsObjCoefs),ctype)
  expect_equal(getObj(lpBndsObjCoefs),obj)
})

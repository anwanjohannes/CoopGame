boolSkip=F



# test_that("Check 62.1 - testing init of S4 class LPRows" ,{
#   if(boolSkip){
#     skip("Test was skipped")
#   }
#   A=c(0,0,0,4,0,3,6)
#   lpRows<-new("LPRows",A)
#   expect_equal(lpRows@rlb,c(0,0,0,4,0,3,6))
#   expect_equal(lpRows@rub, c(Inf,Inf,Inf,Inf,Inf,Inf,6))
#   expect_equal(lpRows@rtype,c(2,2,2,2,2,2,5))
# })


test_that("Check 62.1 - testing init of S4 class LPRows" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  lpRows<-new("LPRows",rlb=c(0,0,0,4,0,3,6),rub=c(Inf,Inf,Inf,Inf,Inf,Inf,6),rtype=c(2,2,2,2,2,2,5))
  expect_equal(lpRows@rlb,c(0,0,0,4,0,3,6))
  expect_equal(lpRows@rub, c(Inf,Inf,Inf,Inf,Inf,Inf,6))
  expect_equal(lpRows@rtype,c(2,2,2,2,2,2,5))
})

test_that("Check 62.2 - testing wrapper function LPRows" ,{
  if(boolSkip){
    skip("Test was skipped")
  }

  rlb=c(0,0,0,4,0,3,6)
  rub=c(Inf,Inf,Inf,Inf,Inf,Inf,6)
  rtype=c(2,2,2,2,2,2,5)
  lpRows<-LPRows(rlb,rub,rtype)
  expect_equal(lpRows@rlb,c(0,0,0,4,0,3,6))
  expect_equal(lpRows@rub, c(Inf,Inf,Inf,Inf,Inf,Inf,6))
  expect_equal(lpRows@rtype,c(2,2,2,2,2,2,5))
})

test_that("Check 62.3 - testing validator of class LPRows when invalid" ,{
  if(boolSkip){
    skip("Test was skipped")
  }

  
  #length is invalid
  rlb=c(0,0,0,4,3) 
  rub=c(Inf,Inf,Inf,Inf,Inf,6) 
  rtype=c(2,2,2,2,2,5)
  expect_error(lpRows<-LPRows(rlb,rub,rtype))
})

test_that("Check 62.3 - testing getter and setter of class LPRows" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  
  rlb=c(0,0,0,4,0,3,6)
  rub=c(Inf,Inf,Inf,Inf,Inf,Inf,6)
  rtype=c(4,4,4,4,4,4,5)
  
  lpRows<-new("LPRows")
  
  setRlb(lpRows)<-rlb
  setRub(lpRows)<-rub
  setRtype(lpRows)<-rtype
  
  expect_equal(as.numeric(getRlb(lpRows)),rlb)
  expect_equal(as.numeric(getRub(lpRows)),rub)
  expect_equal(as.numeric(getRtype(lpRows)),rtype)

})

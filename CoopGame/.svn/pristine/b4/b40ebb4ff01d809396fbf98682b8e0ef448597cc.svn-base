boolSkip=F




test_that("Check 63.1 - testing init of S4 class LPMatrix" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  coeffMat<-createBitMatrix(n=3,1:7)
  lpMatrix<-new("LPMatrix",matrix=coeffMat)
  expect_equal(lpMatrix@matrix,coeffMat)
})



boolSkip=F







test_that("Check 69.1 - testing calculation of class PerCapitaNucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  A<-c( 0,  0,  0,  0,  9, 10, 12)
  result=perCapitaNucleolus(A)
  
  expect_equal(result,c(4/6,7/6,61/6),tolerance=1e-3)
  
})

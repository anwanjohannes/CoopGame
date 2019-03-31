boolSkip=F


test_that("Check 70.1 - testing calculation of class SimplifiedModiclus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  expected_result=c(2/3,1/6,1/6)
  A<-c( 0,  0,  0,  1,  1, 0, 1)
  result=simplifiedModiclus(A)
  expect_equal(result,expected_result,tolerance=1e-3)
  
  sm=new("SimplifiedModiclus",A)
  result2=calculateNucleolus(sm)
  expect_equal(result2,expected_result,tolerance=1e-3)
})

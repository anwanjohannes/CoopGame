boolSkip=T






test_that("Check 68.1 - testing calculation of class ProportionalNucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  A<-c(20,200,0,210,40,200,350)
  
  
  expect_equal(round(proportionalNucleolus(A),3),c(36.992,291.667,21.341),tolerance=1e-3)
  
})

test_that("Check 68.2 - testing calculation of class ProportionalNucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A<-c(rep(0,5),
       10,13,15,13,14,19,8,15,12,20,
       18,27,17,32,19,25,30,18,24,32,
       40,33,38,42,41,
       54)
  expected_x<-c(8.35,7.24,11.69,17.26, 9.46)
  
  
  expect_equal(round(proportionalNucleolus(A),2),expected_x,tolerance=1e-3)
  
})


test_that("Check 68.3 - testing calculation of class ProportionalNucleolus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  # https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=6533531
  A<-c(2.6997,45.5326,6.0747,11.2879,51.5066,7.7809,13.2551,59.2932,75.2844,19.5341,66.9071,83.3113,23.0674,94.8386,137.9590)
  
  A<-c(1000,900,500,1100,1500,1300,1500)
  
  A<-c(rep(0,5),
       10,13,15,13,14,19,8,15,12,20,
       18,27,17,32,19,25,30,18,24,32,
       40,33,38,42,41,
       54)
  expected_x<-c(8.35,7.24,11.69,17.26, 9.46)
  
  
  expect_equal(round(proportionalNucleolus(A),2),expected_x,tolerance=1e-3)
  
})
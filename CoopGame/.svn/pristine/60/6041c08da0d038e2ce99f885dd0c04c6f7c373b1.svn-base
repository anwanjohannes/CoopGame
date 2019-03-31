boolSkip=F

#' A=c(0,0,0,40,50,20,100) #return 1
#' A=c(1,1,1,2,3,4,3) #return 0
#' A= c(0,0,0,0,7,7,7,7,7,7,12,12,12,12,22) #return 1
#' A= c(0,0,0,0,1,0,0,0,0,3,3,3,3,3,4) #return 0

test_that("Check 46.1 - isBalancedGame with 3 players, return TRUE" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A=c(0,0,0,40,50,20,100)
  result=isBalancedGame(A)
  expect_equal(result, TRUE)
})


test_that("Check 46.2 - isBalancedGame with 4 players, return TRUE" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A= c(0,0,0,0,7,7,7,7,7,7,12,12,12,12,22)
  result=isBalancedGame(A)
  expect_equal(result, TRUE)
})

test_that("Check 46.3 - isBalancedGame with 3 players, return FALSE" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A=c(1,1,1,2,3,4,3)
  result=isBalancedGame(A)
  expect_equal(result, FALSE)
})

test_that("Check 46.4 - isBalancedGame with 4 players, return FALSE" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A= c(0,0,0,0,1,0,0,0,0,3,3,3,3,3,4)
  result=isBalancedGame(A)
  expect_equal(result, FALSE)
})
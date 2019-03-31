boolSkip=F

test_that("Check 10.1 - cFuncApexValue with S={1,2}, apex-player = 1, number of players = 3",{
  if(boolSkip){
    skip("Test was skipped")
  }
  S=c(1,2)
  result=cFuncApexValue(S,n=3, apexPlayer = 1)
  expect_equal(result,1)
})

test_that("Check 10.2 - cFuncApexValue with S={3}, apex-player = 1, number of players = 3",{
  if(boolSkip){
    skip("Test was skipped")
  }
  S=c(3)
  result=cFuncApexValue(S, n=3,apexPlayer =1)
  expect_equal(result,0)
})

test_that("Check 10.3 - cFuncApexValue with S={2,3}, apex-player = 1, number of players = 3",{
  if(boolSkip){
    skip("Test was skipped")
  }
  S=c(2,3)
  result=cFuncApexValue(S,n=3, apexPlayer = 1)
  expect_equal(result,1)
})

test_that("Check 10.4 - cFuncApexValue with S={1,2,3}, apex-player = 1, number of players = 3",{
  if(boolSkip){
    skip("Test was skipped")
  }
  S=c(1,2,3)
  result=cFuncApexValue(S, n=3,apexPlayer = 1)
  expect_equal(result,1)
})

test_that("Check 10.5 - cFuncApexValue with S={2,3}, apex-player = 1, number of players = 10",{
  if(boolSkip){
    skip("Test was skipped")
  }
  S=c(2,3)
  apexplayer = 1
  result=cFuncApexValue(S, n = 10, apexplayer)
  expect_equal(result,0)
})

test_that("Check 10.6 - cFuncApexValue with S={2,3,4,5,6,7,8,9,10}, apex-player = 1, number of players = 10",{
  if(boolSkip){
    skip("Test was skipped")
  }
  S=c(2,3,4,5,6,7,8,9,10)
  apexplayer = 1
  result=cFuncApexValue(S, n = 10, apexplayer)
  expect_equal(result,1)
})

test_that("Check 10.7 - cFuncApexValue with S={1,2,3,4,5,6,7,8,9}, apex-player = 10, number of players = 10",{
  if(boolSkip){
    skip("Test was skipped")
  }
  S=c(1,2,3,4,5,6,7,8,9)
  apexplayer = 10
  result=cFuncApexValue(S, n = 10, apexplayer)
  expect_equal(result,1)
})

test_that("Check 10.7 - cFuncApexValue with S={1,2,3,4,5,6,7,8,9,10}, apex-player = 10, number of players = 10",{
  if(boolSkip){
    skip("Test was skipped")
  }
  S=c(1,2,3,4,5,6,7,8,9,10)
  apexplayer = 10
  result=cFuncApexValue(S, n = 10, apexplayer)
  expect_equal(result,1)
})


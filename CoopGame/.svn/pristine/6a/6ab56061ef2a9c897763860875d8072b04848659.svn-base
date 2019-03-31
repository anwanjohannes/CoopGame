boolSkip=F

test_that("Check 08.2 - generateGameVector for cFuncMajoritySingleVeto with N = {1,2,3} and veto player = 2" ,{
  if(boolSkip){
    skip("Test was skipped")
  }

  result=generateGameVector(cFuncMajoritySingleVeto, n = 3, vetoPlayer = 2)
  expect_equal(result, GameVector(c(0,0,0,1,0,1,1)))
})

test_that("Check 08.3 - generateGameVector for cFuncDivideTheDollar with N = {1,2,3,4}" ,{
  if(boolSkip){
    skip("Test was skipped")
  }

  result=generateGameVector(cFuncDivideTheDollar, n = 4)
  expect_equal(result, GameVector(c(0,0,0,0,1,1,1,1,1,1,1,1,1,1,1)))
})

test_that("Check 08.4 - generateGameVector for cFuncDivideTheDollar with N = {1,2,3}" ,{
  if(boolSkip){
    skip("Test was skipped")
  }

  result=generateGameVector(cFuncDivideTheDollar, n =3)
  expect_equal(result, GameVector(c(0,0,0,1,1,1,1)))
})

test_that("Check 08.5 - generateGameVector for cFuncGlove with N = 4" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  L=c(1,3,4)
  R=c(2)
  result=generateGameVector(cFuncGlove, n = 4, L, R)
  expect_equal(result, GameVector(c(0,0,0, 0,1,0, 0,1,1, 0,1,1, 0,1,1)))
})

boolSkip=F

test_that("Check 02.1 - cFuncCardinalityValue with S={1}",{
  if(boolSkip){
    skip("Test was skipped")
  }
  S=c(1)
  result=cFuncCardinalityValue(S)
  expect_equal(result,1)
})

test_that("Check 02.2 - cFuncCardinalityValue with S={1,2}",{
  if(boolSkip){
    skip("Test was skipped")
  }
  S=c(1,2)
  result=cFuncCardinalityValue(S)
  expect_equal(result,2)
})

test_that("Check 02.3 - cFuncCardinalityValue with S={1,..,10}",{
  if(boolSkip){
    skip("Test was skipped")
  }
  S=c(1:10)
  result=cFuncCardinalityValue(S)
  expect_equal(result,10)
})

boolSkip=F

test_that("Check 14.1 - cFuncQuotaValue ",{
  if(boolSkip){
    skip("Test was skipped")
  }
   S=c(1,2)
   w=c(1,2,2)
   q = 4
  result=cFuncQuotaValue(S, w, q)
  expect_equal(result,0)
})



test_that("Check 14.2 - cFuncQuotaValue ",{
  if(boolSkip){
    skip("Test was skipped")
  }
  S=c(1,2)
  w=c(1:5)
  q = 4
  result=cFuncQuotaValue(S, w, q)
  expect_equal(result,0)
})

test_that("Check 14.3 - cFuncQuotaValue ",{
  if(boolSkip){
    skip("Test was skipped")
  }
  S=c(1,2)
  w=c(1:5)
  q = 4
  result=cFuncQuotaValue(S, w, q)
  expect_equal(result,0)
})

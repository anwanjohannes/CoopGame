boolSkip=F

test_that("Check 35.1 - publicGoodIndex ",{
  if(boolSkip){
    skip("Test was skipped")
  }
  result=publicGoodIndex(c(0,0,0,0,1,1,1,0,0,0,1,1,1,0,1))

  expect_equal(result,c(0.5,1/6,1/6,1/6))
})



test_that("Check 35.2 - publicGoodIndex4cVector example Holler Illing chapter 6.3.4 ",{
  if(boolSkip){
    skip("Test was skipped")
  }
  w=c(35,20,15,15,15)
  q=51
  hv=c(16/60,8/60,12/60,12/60,12/60)
  A=generateGameVector(v=cFuncQuota, n=5,w,q)
  result=publicGoodIndex(A)
  expect_equal(hv,result)
})

#Reference BERTINI, GAMBARELLI, STACH 
#A Public Help Index
#http://download.springer.com/static/pdf/406/chp%253A10.1007%252F978-3-540-73382-9_5.pdf?originUrl=http%3A%2F%2Flink.springer.com%2Fchapter%2F10.1007%2F978-3-540-73382-9_5&token2=exp=1487584377~acl=%2Fstatic%2Fpdf%2F406%2Fchp%25253A10.1007%25252F978-3-540-73382-9_5.pdf%3ForiginUrl%3Dhttp%253A%252F%252Flink.springer.com%252Fchapter%252F10.1007%252F978-3-540-73382-9_5*~hmac=601a3649d43838b8b22d3b4e01a7a51abaf028cc005c63263a3654cb7000ebec
test_that("Check 35.3 - publicGoodIndex4cVector example Holler Illing chapter 6.3.4 ",{
  if(boolSkip){
    skip("Test was skipped")
  }
  result=publicGoodIndex(A=c(0,0,0,1,1,0,1))
  expect_equal(result,c(1/2,1/4,1/4))
})

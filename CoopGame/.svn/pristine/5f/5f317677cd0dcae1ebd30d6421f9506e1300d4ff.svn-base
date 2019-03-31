boolSkip=F

test_that("Check 44.1 - check cFuncCostSharingValue on appartment game for 3 players" ,{

  if(boolSkip){
    skip("Test was skipped")
  }
  
  C=c(300,270,280,410,410,410,550)
  expect_equal(0,cFuncCostSharingValue(S = c(1),C))
  expect_equal(0,cFuncCostSharingValue(S = c(2),C))
  expect_equal(0,cFuncCostSharingValue(S = c(3),C))
  expect_equal(160,cFuncCostSharingValue(S = c(1,2),C))
  expect_equal(170,cFuncCostSharingValue(S = c(1,3),C))
  expect_equal(140,cFuncCostSharingValue(S = c(2,3),C))
  expect_equal(300,cFuncCostSharingValue(S = c(1,2,3),C))
})


test_that("Check 44.2 - check cFuncCostSharingValue on appartment game for 3 players" ,{

  if(boolSkip){
    skip("Test was skipped")
  }
  
  C=c(300,270,280,410,410,410,550)
  expectedA=GameVector(c(0,0,0,160,170,140,300))
  actualA=generateGameVector(cFuncCostSharing, n=3, C)
  expect_equivalent(actualA,expectedA)
})


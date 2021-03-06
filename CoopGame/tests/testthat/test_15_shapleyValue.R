boolSkip=F

test_that("Check 15.1 - shapley with 3 players, return (9.5, 8.0, 6.5)" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A=c(2,4,5,18,14,9,24)
  result=shapleyValue(A)
  expect_equal(result, c(9.5, 8.0, 6.5))
})

test_that("Check 15.2 - shapley with 3 players, return (9.5, 8.0, 6.5)" ,{
  if(boolSkip){
    skip("Test was skipped")
  }

  A=generateGameVector(cFuncGlove, n=3, 1, c(2,3))
  result=shapleyValue(A)
  expect_equal(result, c(2/3, 1/6, 1/6))
})

test_that("Check 15.3 - shapley with 10 players" ,{
  if(boolSkip){
    skip("Test was skipped")
  }

  A=generateGameVector(cFuncGlove, n=10, 1, c(2,3,4,5,6,7,8,9,10))
  result=shapleyValue(A)
  expect_equal(result, c(9/10, 1/90, 1/90, 1/90, 1/90, 1/90, 1/90, 1/90, 1/90, 1/90))
})

test_that("Check 15.4 - shapley with 3 players, glove game: L={1,2}, R={3},
          Wiese p. 65" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A=generateGameVector(v = cFuncGlove, n = 3, L = c(1,2), R=3)
  result=shapleyValue(A)
  expect_equal(result, c(1/6, 1/6, 2/3))
})

test_that("Check 15.5 - shapley with 4 players, glove game: L={1,2,4}, R={3}, Wiese p. 65" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A=generateGameVector(v = cFuncGlove, n = 4, L = c(1,2,4), R=3)
  result=shapleyValue(A)
  expect_equal(result, c(1/12, 1/12, 3/4, 1/12))
})


test_that("Check 15.6 - shapley with 5 players, glove game: L={1,2,4,5}, R={3}, Wiese p. 65" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A=generateGameVector(v = cFuncGlove, n = 5, L = c(1,2,4,5), R=3)
  result=shapleyValue(A)
  expect_equal(result, c(0.05, 0.05, 0.8, 0.05, 0.05))
})

test_that("Check 15.7 - shapley with 5 players, glove game: L={1,2,4,5}, R={3}, Wiese p. 65" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A=generateGameVector(v = cFuncGlove, n = 5, L = c(1,2,4,5), R=3)
  result=shapleyValue(A)
  expect_equal(result, c(0.05, 0.05, 0.8, 0.05, 0.05))
})


test_that("Check 15.8 - Shaplay with 3 players (election)",{
  if(boolSkip){
    skip("Test was skipped")
  }
  #look at example https://de.wikipedia.org/wiki/Machtindex#cite_note-Banzhaf65-3

  #N=c(1,2,3), w=(50,49,1), q=1
  A1=generateGameVector(cFuncQuota,n=3, w=c(50,49,1),q=1)
  sv1=shapleyValue(A1)
  expect_equal(sv1,c(1/3,1/3,1/3))

  #N=c(1,2,3), w=(50,49,1), q=51
  A51=generateGameVector(cFuncQuota,n=3, w=c(50,49,1),q=51)
  sv51=shapleyValue(A51)
  expect_equal(sv51,c(2/3,1/6,1/6))

  #N=c(1,2,3), w=(50,49,1), q=52
  A52=generateGameVector(cFuncQuota, n=3, w=c(50,49,1),q=52)
  sv52=shapleyValue(A52)
  expect_equal(sv52,c(1/2,1/2,0.0))

  #N=c(1,2,3), w=(50,49,1), q=100
  A100=generateGameVector(cFuncQuota, n=3,w=c(50,49,1),q=100)
  sv100=shapleyValue(A100)
  expect_equal(sv100,c(1/3,1/3,1/3))

})





boolSkip=T

test_that("Check 74.1 - coreVertices - 3 players",{
  if(boolSkip){
    skip("Test was skipped")
  }
  A <- c(2, 4, 5, 18, 24, 9, 24)
  result = coreVertices(A)
  expect_equal(nrow(result), 0)
})

test_that("Check 74.2 - coreVertices - 3 players",{
  if(boolSkip){
    skip("Test was skipped")
  }
  A <- c(0, 0, 0, 40, 50, 20, 100) 
  V1 <- c(80, 20, 0)
  V2 <- c(80, 0, 20)
  V3 <- c(40, 0, 60)
  V4 <- c(0, 40, 60)
  V5 <- c(0, 50, 50)
  V6 <- c(50, 50, 0)
  result = coreVertices(A)
  expect_equal(result[1, ], V1)
  expect_equal(result[2, ], V2)
  expect_equal(result[3, ], V3)
  expect_equal(result[4, ], V4)
  expect_equal(result[5, ], V5)
  expect_equal(result[6, ], V6)
})

test_that("Check 74.3 - coreVertices - 4 players",{
  if(boolSkip){
    skip("Test was skipped")
  }
  A <- c(0, 0, 0, 0, 7, 7, 7, 7, 7, 7, 12, 12, 12, 12, 22)
  V1 <- c(10, 2, 5, 5)
  V2 <- c(8, 0, 7, 7)
  V3 <- c(7, 0, 8, 7)
  V4 <- c(5, 2, 10, 5)
  V5 <- c(0, 7, 8, 7)
  V6 <- c(0, 8, 7, 7)
  V7 <- c(2, 10, 5, 5)
  V8 <- c(5, 10, 5, 2)
  V9 <- c(10, 5, 5, 2)
  result = coreVertices(A)
  expect_equal(result[1, ], V1)
  expect_equal(result[2, ], V2)
  expect_equal(result[3, ], V3)
  expect_equal(result[4, ], V4)
  expect_equal(result[5, ], V5)
  expect_equal(result[6, ], V6)
  expect_equal(result[7, ], V7)
  expect_equal(result[8, ], V8)
  expect_equal(result[9, ], V9)
})

test_that("Check 74.4 - coreVertices - 4 players",{
  if(boolSkip){
    skip("Test was skipped")
  }
  A <- c(1, 2, 3, 4, 5, 3, 6, 7, 7, 8, 10, 16, 32, 12, 66)
  V1 <- c(54, 4, 3, 5)
  V2 <- c(54, 2, 5, 5)
  V3 <- c(3, 2, 50, 11)
  V4 <- c(1, 4, 50, 11)
  V5 <- c(1, 34, 26, 5)
  V6 <- c(24, 34, 4, 4)
  result = coreVertices(A)
  expect_equal(result[1, ], V1)
  expect_equal(result[2, ], V2)
  expect_equal(result[3, ], V3)
  expect_equal(result[4, ], V4)
  expect_equal(result[5, ], V5)
  expect_equal(result[6, ], V6)
})

test_that("Check 74.5 - coreVertices - 5 players",{
  if(boolSkip){
    skip("Test was skipped")
  }
  A <- generateGameVector(v = cFuncQuota, n = 5, w = c(4,2,2,1,1), q = 6)
  result = coreVertices(A)
  expect_equal(nrow(result), 0)
})
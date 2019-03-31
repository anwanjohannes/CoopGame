boolSkip=T

test_that("Check 78.1 - webersetVertices - 3 players",{
  if(boolSkip){
    skip("Test was skipped")
  }
  A <- c(2, 4, 5, 18, 24, 9, 24)
  V1 <- c(15, 4, 5)
  V2 <- c(19, 0, 5)
  V3 <- c(2, 0, 22)
  V4 <- c(2, 16, 6)
  result = webersetVertices(A)
  expect_equal(result[1, ], V1)
  expect_equal(result[2, ], V2)
  expect_equal(result[3, ], V3)
  expect_equal(result[4, ], V4)
})

test_that("Check 78.2 - webersetVertices - 3 players",{
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
  result = webersetVertices(A)
  expect_equal(result[1, ], V1)
  expect_equal(result[2, ], V2)
  expect_equal(result[3, ], V3)
  expect_equal(result[4, ], V4)
  expect_equal(result[5, ], V5)
  expect_equal(result[6, ], V6)
})

test_that("Check 78.3 - webersetVertices - 4 players",{
  if(boolSkip){
    skip("Test was skipped")
  }
  A <- c(0, 0, 0, 0, 7, 7, 7, 7, 7, 7, 12, 12, 12, 12, 22)
  V1 <- c(10, 0, 5, 7)
  V2 <- c(5, 0, 10, 7)
  V3 <- c(0, 5, 7, 10)
  V4 <- c(0, 10, 5, 7)
  V5 <- c(7, 10, 5, 0)
  V6 <- c(10, 7, 0, 5)
  result = webersetVertices(A)
  expect_equal(result[1, ], V1)
  expect_equal(result[2, ], V2)
  expect_equal(result[3, ], V3)
  expect_equal(result[4, ], V4)
  expect_equal(result[5, ], V5)
  expect_equal(result[6, ], V6)
})

test_that("Check 78.4 - webersetVertices - 4 players",{
  if(boolSkip){
    skip("Test was skipped")
  }
  A <- c(1, 2, 3, 4, 5, 3, 6, 7, 7, 8, 10, 16, 32, 12, 66)
  V1 <- c(54, 4, 3, 5)
  V2 <- c(54, 2, 5, 5)
  V3 <- c(3, 2, 5, 56)
  V4 <- c(1, 4, 50, 11)
  V5 <- c(0, 7, 3, 56)
  V6 <- c(0, 34, 3, 29)
  V7 <- c(24, 34, 3, 5)
  result = webersetVertices(A)
  expect_equal(result[1, ], V1)
  expect_equal(result[2, ], V2)
  expect_equal(result[3, ], V3)
  expect_equal(result[4, ], V4)
  expect_equal(result[5, ], V5)
  expect_equal(result[6, ], V6)
  expect_equal(result[7, ], V7)
})

test_that("Check 78.5 - webersetVertices - 5 players",{
  if(boolSkip){
    skip("Test was skipped")
  }
  A <- generateGameVector(v = cFuncQuota, n = 5, w = c(4,2,2,1,1), q = 6)
  V1 <- c(1, 0, 0, 0, 0)
  V2 <- c(0, 0, 1, 0, 0)
  V3 <- c(0, 1, 0, 0, 0)
  result = webersetVertices(A)
  expect_equal(result[1, ], V1)
  expect_equal(result[2, ], V2)
  expect_equal(result[3, ], V3)
})
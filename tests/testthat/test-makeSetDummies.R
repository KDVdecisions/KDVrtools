test_that("base case, one column, one value per observations", {
  testDf <- data.frame(spp01 = c("bat", "cat", "rat", "crab"))
  setColumns = c("spp01")
  setValues = c("bat", "cat", "rat", "crab")
  
  expected <- data.frame(spp01 = c("bat", "cat", "rat", "crab"),
                         bat = c(TRUE, FALSE, FALSE, FALSE),
                         cat = c(FALSE, TRUE, FALSE, FALSE),
                         rat = c(FALSE, FALSE, TRUE, FALSE),
                         crab = c(FALSE, FALSE, FALSE, TRUE))

  output <- makeSetDummies(testDf, setColumns, setValues)
  
  expect_identical(expected, output)
})


test_that("base case, one value per observations, as numeric", {
  testDf <- data.frame(spp01 = c("bat", "cat", "rat", "crab"))
  setColumns = c("spp01")
  setValues = c("bat", "cat", "rat", "crab")
  
  expected <- data.frame(spp01 = c("bat", "cat", "rat", "crab"),
                         bat = c(1, 0, 0, 0),
                         cat = c(0, 1, 0, 0),
                         rat = c(0, 0, 1, 0),
                         crab = c(0, 0, 0, 1))
  
  output <- makeSetDummies(testDf, setColumns, setValues, numeric = TRUE)
  
  expect_identical(expected, output)
})

test_that("One column, multiple values per observation", {
  testDf <- data.frame(spp01 = c("bat, cat", "cat, crab, rat", "rat", "crab, bat"))
  setColumns = c("spp01")
  setValues = c("bat", "cat", "rat", "crab")
  
  expected <- data.frame(spp01 = c("bat, cat", "cat, crab, rat", "rat", "crab, bat"),
                         bat = c(TRUE, FALSE, FALSE, TRUE),
                         cat = c(TRUE, TRUE, FALSE, FALSE),
                         rat = c(FALSE, TRUE, TRUE, FALSE),
                         crab = c(FALSE, TRUE, FALSE, TRUE))
  
  output <- makeSetDummies(testDf, setColumns, setValues)
  
  expect_identical(expected, output)
})


test_that("multiple columns, multiple strings per cell", {
  testDf <- data.frame(spp01 = c("bat", "cat", "rat", "crab"),
                       spp02 = c("bat", "cat, rat", "crab", "bat, rat"))
  setColumns = c("spp01", "spp02")
  setValues = c("bat", "cat", "rat", "crab")
  
  expected <- data.frame(spp01 = c("bat", "cat", "rat", "crab"),
                         spp02 = c("bat", "cat, rat", "crab", "bat, rat"),
                         bat = c(TRUE, FALSE, FALSE, TRUE),
                         cat = c(FALSE, TRUE, FALSE, FALSE),
                         rat = c(FALSE, TRUE, TRUE, TRUE),
                         crab = c(FALSE, FALSE, TRUE, TRUE))
  
  output <- makeSetDummies(testDf, setColumns, setValues)
  
  expect_identical(expected, output)
})

test_that("One column, one value per observations, anticipate substring warning", {
  testDf <- data.frame(spp01 = c("bat", "cat", "tiny bat", "crab"))
  setColumns = c("spp01")
  setValues = c("bat", "cat", "tiny bat", "crab")

  
  expect_warning(makeSetDummies(testDf, setColumns, setValues), 
  "setValue level 'bat' is a substring of 'tiny bat'. This will cause false positives.")
  
})





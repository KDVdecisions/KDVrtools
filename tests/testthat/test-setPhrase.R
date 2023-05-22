# Eliot Dixon
# KDV Decision Analysis LLC
# 2023-05-19

test_that("setPhrase works with a length 2 character input, default args", {

  expect_equal(setPhrase(c("This", "that")), "This and that")

})


test_that("setPhrase works with a length 3 character input, default args", {

  expect_equal(setPhrase(c("This", "that", "the other")), "This, that, and the other")

})

test_that("setPhrase works with a length 3 character input, no oxford", {

  expect_equal(setPhrase(c("This", "that", "the other"), oxford = FALSE), "This, that and the other")

})


test_that("setPhrase works with a length 2 character input, no oxford", {

  expect_equal(setPhrase(c("This", "that"), oxford = FALSE), "This and that")

})

test_that("setPhrase works with a length 2 character input, vectorize", {

    expect_equal(setPhrase(c("This", "that"), vectorize = TRUE), c("This and", "that"))

})


test_that("setPhrase works with a length 3 character input, vectorize", {

  expect_equal(setPhrase(c("This", "that", "the other"), vectorize = TRUE), c("This,", "that, and", "the other"))

})


test_that("setPhrase works with a length 2 character input, vectorize, no oxford", {

  expect_equal(setPhrase(c("This", "that"), vectorize = TRUE), c("This and", "that"))

})


test_that("setPhrase works with a length 3 character input, vectorize, no oxford", {

  expect_equal(setPhrase(c("This", "that", "the other"), oxford = FALSE, vectorize = TRUE),
               c("This,", "that and", "the other"))

})


test_that("setPhrase works with a length 2 character input, vectorize", {

  expect_equal(setPhrase(c("This", "that"), vectorize = TRUE), c("This and", "that"))

})


test_that("setPhrase works with a length 3 numeric input", {

  expect_equal(setPhrase(c(1,2,3)), "1, 2, and 3")

})


test_that("setPhrase works with a length 3 numeric input, no oxford", {

  expect_equal(setPhrase(c(1,2,3), oxford = FALSE), "1, 2 and 3")

})


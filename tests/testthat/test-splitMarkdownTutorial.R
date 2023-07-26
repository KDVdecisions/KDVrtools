test_that("Test to ensure that RMD with one header is split", {
  splitMarkdownTutorial(test_path("testdata", "testsplitMarkdownTutorial.Rmd"))
  expect_true(file.exists("CORE_ELEMENTS.R"))
  # cleaning up: 
  file.remove("CORE_ELEMENTS.R")
  file.remove("testsplitMarkdownTutorial.R")
})

test_that("Test to ensure that RMD with one header is split", {
  splitMarkdownTutorial(test_path("testdata", "testsplitMarkdownSmall.Rmd"))
  expect_true(file.exists("CORE_ELEMENTS.R"))
  # cleaning up: 
  file.remove("CORE_ELEMENTS.R")
  file.remove("testsplitMarkdownSmall.R")
})

test_that("Test to ensure that RMD with two headers is split", {
  splitMarkdownTutorial(test_path("testdata", "testsplitMarkdownLarge.Rmd"))
  expect_true(file.exists("R_Markdown.R"))
  expect_true(file.exists("Including_Plots.R"))
  
  # cleaning up: 
  file.remove("R_Markdown.R")
  file.remove("Including_Plots.R")
  file.remove("testsplitMarkdownLarge.R")
})

test_that("Test to ensure that lowercase headers are split", {
  splitMarkdownTutorial(test_path("testdata", "testsplitMarkdownLowHeaders.Rmd"))
  expect_true(file.exists("r_Markdown.R"))
  expect_true(file.exists("including_Plots.R"))
  
  # cleaning up: 
  file.remove("r_Markdown.R")
  file.remove("including_Plots.R")
  file.remove("testsplitMarkdownLowHeaders.R")
})

test_that("Test to ensure that error is thrown for RMD without proper headers", {
  expect_error(splitMarkdownTutorial(test_path("testdata", "testsplitMarkdownNoHeaders.Rmd")), 
               "No headers detected. Make sure they are denoted with `# Section Name`")
  
  # cleaning up: 
  file.remove("testsplitMarkdownNoHeaders.R")
})
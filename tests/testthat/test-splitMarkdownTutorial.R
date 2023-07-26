test_that("Test to ensure that RMD with one header is split", {
  splitMarkdownTutorial(test_path("testdata", "testsplitMarkdownTutorialSmall.Rmd"))
  expect_true(file.exists("CORE_ELEMENTS.R"))
  # cleaning up: 
  file.remove("CORE_ELEMENTS.R")
  file.remove("testsplitMarkdownTutorialSmall.R")
})

test_that("Test to ensure that RMD with two headers is split", {
  splitMarkdownTutorial(test_path("testdata", "testsplitMarkdownTutorialLarge.Rmd"))
  expect_true(file.exists("R_Markdown.R"))
  expect_true(file.exists("Including_Plots.R"))
  
  # cleaning up: 
  file.remove("R_Markdown.R")
  file.remove("Including_Plots.R")
  file.remove("testsplitMarkdownTutorialLarge.R")
})

# Eliot Dixon
# KDV Decision Analysis LLC
# 2023-05-24

test_that("creating single folder works", {
  generateFolders(path = "./", "P_testProj")
  
  expect_true(dir.exists("./P_testProj"))
  
  # Clean up
  unlink("./P_testProj", recursive = TRUE)
})


test_that("creating single folder with one subfolder works", {
  
  generateFolders(path = "./", "P_testProj", c("Code"))
  
  expect_equal(list.files("./P_testProj"), c("Code"))
  
  # Clean up
  unlink("./P_testProj", recursive = TRUE)
})

test_that("creating single folder with two subfolders works", {
  
  generateFolders(path = "./", "P_testProj", c("Code", "Data"))
  
  expect_equal(list.files("./P_testProj"), c("Code", "Data"))
  
  # Clean up
  unlink("./P_testProj", recursive = TRUE)
})

test_that("creating multiple folders with no subfolders works", {
  generateFolders("./", c("P_testProjA", "P_testProjB"))
  
  expect_true(all(c(dir.exists("./P_testProjA"), 
                    dir.exists("./P_testProjB"))
                  )
              )
  # Clean up
  unlink("./P_testProjA", recursive = TRUE)
  unlink("./P_testProjB", recursive = TRUE)
})

test_that("creating multiple folders with single subfolder each works", {
  
  generateFolders(path = "./", c("P_testProjA", "P_testProjB"), c("Code"))
  
  # Ensure both created folders contain specified subfolders
  expect_true(all(c(list.files("./P_testProjA") == c("Code"), 
                    list.files("./P_testProjB") == c("Code"))
                  )
              )
  # Clean up
  unlink("./P_testProjA", recursive = TRUE)
  unlink("./P_testProjB", recursive = TRUE)
  
})

test_that("creating multiple folders with multiple subfolders works", {
  
  generateFolders(path = "./", c("P_testProjA", "P_testProjB"), c("Code", "Data"))

  # Ensure both created folders contain specified subfolders
  expect_true(all(c(list.files("./P_testProjA") == c("Code", "Data"), 
                    list.files("./P_testProjB") == c("Code", "Data"))
                  )
              )
  # Clean up
  unlink("./P_testProjA", recursive = TRUE)
  unlink("./P_testProjB", recursive = TRUE)
  
})

test_that("Error is thrown when non-existent base path", {
  expect_error(generateFolders("./P_testProjZ"), "Unable to find directory './P_testProjZ'")
})

test_that("Writing to empty folder works", {
  dir.create("./P_testProj")
  generateFolders("./", "P_testProj", c("Code", "Data"))
  
  expect_equal(list.files("./P_testProj"), c("Code", "Data"))
  
  # Clean up
  unlink("./P_testProj", recursive = TRUE)
})


test_that("Warning provided when folder already exists, with single folder", {
  dir.create("./P_testProj/Data", recursive = TRUE)
  expect_warning(generateFolders("./", "P_testProj"), 
                 "Directory 'P_testProj' already exists, skipping.")
  
  # Clean up
  unlink("./P_testProj", recursive = TRUE)
})


test_that("Warning provided when folder already exists and not empty, with multiple folders", {
  dir.create("./P_testProjA/Data", recursive = TRUE)
  
  
  expect_warning(generateFolders("./", c("P_testProjA", "P_testProjB")), 
                 "Directory 'P_testProjA' already exists, skipping.")
  
  # Clean up
  unlink("./P_testProjA", recursive = TRUE)
  unlink("./P_testProjB", recursive = TRUE)
})


test_that("Writes other folder when one of folder arguments already exists and is not empty", {
  dir.create("./P_testProjA/Data", recursive = TRUE)
  
  # Run and silence warning regarding non-empty folder
  suppressWarnings(generateFolders("./", c("P_testProjA", "P_testProjB"), c("Code", "Data")))
  
  expect_equal(list.files("./P_testProjB"), c("Code", "Data"))
  # Clean up
  unlink("./P_testProjA", recursive = TRUE)
  unlink("./P_testProjB", recursive = TRUE)
})




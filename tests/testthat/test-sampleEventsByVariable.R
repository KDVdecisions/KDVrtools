test_that("Simple plots are produced", {
  # prepping test data: 
  airqualLong <- tidyr::pivot_longer(airquality, 1:4,
                                     names_to = "vars", 
                                     values_to = "val")
  airqualLong$DATE <- paste(airqualLong$Month, airqualLong$Day, "2010", 
                            sep = "/")
  airqualLong$DATE <- as.Date(airqualLong$DATE, tryFormats = c("%m/%d/%Y"))
  airqualLong$Month <- as.factor(airqualLong$Month)
  
  # tests for printed ggplots was adopted from this stack overflow question: 
  # https://stackoverflow.com/questions/65954246/how-to-test-if-ggplot-was-printed
  cap <- ragg::agg_capture()
  
  has_printed <- function(x = cap()) {
    !all(x == "white")
  }
  
  # calling function: 
  sampleEventsByVariable(df = airqualLong, 
                         setCol = "Month", 
                         varCol = "vars", 
                         var = unique(airqualLong$vars), 
                         dateBreaks = "1 day")
  
  # checking to ensure that the plot was printed: 
  expect_true(has_printed())
})

test_that("Data must be a data frame", {
  # creating a list to pass to function: 
  airqualList <- as.list(airquality)
  # checking that error is thrown: 
  expect_error(sampleEventsByVariable(df = airqualList, 
                                      setCol = "Month", 
                                      varCol = "vars", 
                                      var = unique(airqualLong$vars), 
                                      dateBreaks = "1 day"),
               "df must be a data.frame or tibble")
})

test_that("varCol column and DATE", {
  # prepping test data: 
  airqualLong <- tidyr::pivot_longer(airquality, 1:4,
                                     names_to = "vars", 
                                     values_to = "val")
  # changing DATE such that it is no longer all caps: 
  airqualLong$Date <- paste(airqualLong$Month, airqualLong$Day, 
                            "2010", sep = "/")
  airqualLong$Date <- as.Date(airqualLong$Date, tryFormats = c("%m/%d/%Y"))
  airqualLong$Month <- as.factor(airqualLong$Month)
  
  # calling function and checking for error thrown: 
  expect_error(sampleEventsByVariable(df = airqualLong, 
                         setCol = "Month", 
                         varCol = "variables", 
                         var = unique(airqualLong$vars), 
                         dateBreaks = "1 day"),
               "The df object must include columns named: 'variables' and 'DATE'.")
  
})

test_that("varCol is character", {
  # prepping test data: 
  airqualLong <- tidyr::pivot_longer(airquality, 1:4,
                                     names_to = "vars", 
                                     values_to = "val")
  airqualLong$DATE <- paste(airqualLong$Month, airqualLong$Day, 
                            "2010", sep = "/")
  airqualLong$DATE <- as.Date(airqualLong$DATE, tryFormats = c("%m/%d/%Y"))
  airqualLong$Month <- as.factor(airqualLong$Month)
  # changing varCol to factor: 
  airqualLong$vars <- as.factor(airqualLong$vars)
  
  # calling function and checking for error thrown: 
  expect_error(sampleEventsByVariable(df = airqualLong, 
                                      setCol = "Month", 
                                      varCol = "vars", 
                                      var = unique(airqualLong$vars), 
                                      dateBreaks = "1 day"),
               "The column 'vars' must be a character vector.")
  
})

test_that("DATE colum must be Date format", {
  # prepping test data: 
  airqualLong <- tidyr::pivot_longer(airquality, 1:4,
                                     names_to = "vars", 
                                     values_to = "val")
  airqualLong$DATE <- paste(airqualLong$Month, airqualLong$Day, 
                            "2010", sep = "/")
  airqualLong$DATE <- as.Date(airqualLong$DATE, tryFormats = c("%m/%d/%Y"))
  # changing DATE to character: 
  airqualLong$DATE <- as.character(airqualLong$DATE)
  airqualLong$Month <- as.factor(airqualLong$Month)
  
  # calling function and checking for error thrown: 
  expect_error(sampleEventsByVariable(df = airqualLong, 
                                      setCol = "Month", 
                                      varCol = "vars", 
                                      var = unique(airqualLong$vars), 
                                      dateBreaks = "1 day"),
               "The column 'DATE' must be Date format.")
  
})

test_that("Extra vars in var vector", {
  # prepping test data: 
  airqualLong <- tidyr::pivot_longer(airquality, 1:4,
                                     names_to = "vars", 
                                     values_to = "val")
  airqualLong$DATE <- paste(airqualLong$Month, airqualLong$Day, 
                            "2010", sep = "/")
  airqualLong$DATE <- as.Date(airqualLong$DATE, tryFormats = c("%m/%d/%Y"))
  # changing DATE to character: 
  airqualLong$Month <- as.factor(airqualLong$Month)
  
  # calling function and checking for error thrown: 
  expect_error(sampleEventsByVariable(df = airqualLong, 
                                      setCol = "Month", 
                                      varCol = "vars", 
                                      var = c(unique(airqualLong$vars), 
                                              "penguins"), 
                                      dateBreaks = "1 day"),
               "Check your var vector. Some requested values are not found in the 'vars' column.")
})



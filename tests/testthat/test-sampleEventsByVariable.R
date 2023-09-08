test_that("Simple plots are produced", {
  # preppigng test data: 
  airqualLong <- tidyr::pivot_longer(airquality, 1:4,
                                     names_to = "vars", 
                                     values_to = "val")
  airqualLong$DATE <- paste(airqualLong$Month, airqualLong$Day, "2010", sep = "/")
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

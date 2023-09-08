# Ashton Drew, modified by EmmaLi Tsai 
# KDV Decision Analysis, LLC 
# 2023-09-04
################################################################################

#' Plot the frequency of sample data for different factor levels over time.
#' 
#' This function takes a data frame and creates a plot with time on the x-axis 
#' and variables on the y-axis to show the spread of sampling events over time. 
#' This function also takes a column containing subset options as different 
#' factor levels to generate a new plot for each subset. 
#' 
#' @param df Data frame or tibble in long format with a column with the variables 
#' that will appear on y-axis, and a factored column containing options to 
#' subset the data. 
#' @param setCol Name of the column containing subset options as factor levels. 
#' @param varCol Name of the column containing variables to plot on the y-axis.
#' @param var The character vector of names of variables to be used. 
#' All variables should be present within the varCol column. 
#' @param dateBreaks The date breaks that will be used on the x axis. 
#' @param dateLimits The date limits on the y-axis. 
#' @return NULL, plots are printed for each factor level of setCol. 
#' @importFrom dplyr filter 
#' @import ggplot2
#' @examples
#' \dontrun{
#'  airqualLong <- tidyr::pivot_longer(airquality, 1:4,names_to = "vars", values_to = "val")
#'  airqualLong$DATE <- paste(airqualLong$Month, airqualLong$Day, "2010", sep = "/")
#'  airqualLong$DATE <- as.Date(airqualLong$DATE, tryFormats = c("%m/%d/%Y"))
#'  airqualLong$Month <- as.factor(airqualLong$Month)
#'  
#'  sampleEventsByVariable(df = airqualLong, setCol = "Month", varCol = "vars", var = unique(airqualLong$vars), dateBreaks = "1 day")
#'  }
#' @export
################################################################################
sampleEventsByVariable <- function(df, setCol, varCol, var,
                                   dateBreaks, dateLimits = NULL) {
  
  # These lines check that the input data meet requirements.  
  # If an assert fails, the code will stop and provide a helpful message.
  assertthat::assert_that(is.data.frame(df), 
                          msg = "df must be a data.frame or tibble")
  assertthat::assert_that(all(c(varCol, "DATE") %in% names(df)), 
                          msg = paste0("the df object must include columns named: '", varCol, "' and 'DATE'."))
  assertthat::assert_that(is.character(df[[varCol]]), 
                          msg = paste0("The column '", varCol, " ' must be a character vector."))
  assertthat::assert_that(lubridate::is.Date(df[["DATE"]]), 
                          msg = "The column 'DATE' must be Date format.")
  assertthat::assert_that(all(var %in% unique(df[[varCol]])), 
                          msg = paste0("Check your var vector. Some requested values are not found in the ",  varCol , " column."))

  # Filtering out the selected variables
  subData <- dplyr::filter(df, !!dplyr::sym(varCol) %in% var)
  
  # Grabbing factor levels in setCol to loop through: 
  setFactors <- levels(df[[setCol]])
  
  # Looping through factor levels
  for (i in 1:length(setFactors)){
    
    # Grabbing factor level for loop iteration: 
    setI <- setFactors[i]
    
    # Filtering for factor level: 
    subDataI <- dplyr::filter(subData, !!dplyr::sym(setCol) %in% setI)
    
    # Going to next iteration of the loop if subDataI is empty:
    if(nrow(subDataI) == 0) {
      message(paste0("Factor level `", 
                     setI, "` does not contain any data. Skipping."))
      next
    }
    
    # Generate plot
    outPlot <- ggplot2::ggplot() +
      ggplot2::geom_point(data = subDataI, ggplot2::aes(x = DATE, y = !!dplyr::sym(varCol), 
                                     color = !!dplyr::sym(varCol)), size = 1)
    
    # Apply custom date limits if provided as an argument
    if (!is.null(dateLimits)) {
      outPlot <- outPlot +
        ggplot2::scale_x_date(date_breaks = dateBreaks, limits = dateLimits, 
                     expand = c(0,0))
    } else {
      outPlot <- outPlot +
        ggplot2::scale_x_date(date_breaks = dateBreaks, expand = c(0,0))
    }
    
    outPlot <- outPlot +
      
      ggplot2::guides(color = "none") +
      ggplot2::labs(title = paste0("Unit: ", setI), x = "", y = "",
           subtitle = paste0("Dates: ", min(subDataI$DATE, na.rm = T),
                             " to ", max(subDataI$DATE, na.rm = T))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 6),
            plot.title = ggplot2::element_text(hjust = 0.5),
            plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey30"))
    
    # Printing for each loop iteration: 
    print(outPlot)
    
  }
  
  return(invisible(NULL))
}




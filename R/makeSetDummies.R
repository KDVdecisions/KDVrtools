# Eliot Dixon & Ashton Drew
# KDV Decision Analysis LLC
# 2023-05-19


#' Generates a data.frame of dummy columns from a data.frame containing specified
#' columns and values.  Often input is from from Arc Collector when
#' user entered values from a multi-select widget
#'
#' @param df data.frame containing column(s) from which to generate dummy columns
#' @param setColumns character vector specifying which columns in df contain values from which to generate dummy columns
#' @param setValues character vector specifying which values in setColumns to use to generate dummy columns
#' @param numeric logical, if FALSE dummy columns will be of class logical, if TRUE dummy columns will be numeric
#' @return data.frame
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom tidyr unite
#'
#' @export
makeSetDummies <- function(df, setColumns, setValues, numeric = FALSE){

  checkSubstrings(setValues)

  # Account for cases when set is spread across rows
  df <- unite(df, "mergeSet", all_of(setColumns), sep = ", ",
                     na.rm = TRUE, remove = FALSE)
  # Create the dummy columns
  for (i in seq(setValues)){
    df[setValues[i]] <- NA
    df[setValues[i]] <- str_detect(df[["mergeSet"]] , setValues[i])
  }
  # Remove the temporary merged data column
  df <- dplyr::select(df, -mergeSet)

  if(numeric){
    df <- dplyr::mutate(df, across(all_of(setValues), ~as.numeric(.x)))
  }

  return(df)

}



#' Helper function for makeSetDummies, assists in generating warning if any levels
#' within setValues are substrings or duplicates of other levels
#' @param strings character vector, contains setValues argument from makeSetDummies
#'
checkSubstrings <- function(strings){
  for(i in 1:length(strings)){
    thisStr <- strings[i]
    others <- strings[-i]
    for(j in 1:length(others)){
      if(grepl(thisStr, others[j], fixed = TRUE)){
        warning(sprintf("setValue level '%s' is a substring of '%s'. This will cause false positives.", 
                        thisStr, others[j]))
      }
    }
  }
  
  return(NULL)
  
}



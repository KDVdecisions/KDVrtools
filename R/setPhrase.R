# Eliot Dixon & Ashton Drew
# KDV Decision Analysis LLC
# 2023-05-19


#' Convert vector into sentence style list
#' 
#' Turns a vector of values into a sentence style list, inserting "and" prior to 
#' the last item, keeps each item separate within vector
#'
#' @param setVector character vector, vector of values to be converted to sentence style
#' @param oxford logical, if TRUE adds an oxford comma when applicable
#' @param vectorize logical, indicates whether to split phrase items into seperate
#'        indices within output vector.  This may be useful if output is being used within HTML bulleted list.
#' @return a character vector containing input items formatted as a sentance
#' @importFrom stringr str_remove
#' @examples
#' items <- c("This", "that", "the other")
#' # As single string
#' setPhrase(items)
#' # as vector
#' setPhrase(items, vectorize = TRUE)
#' # No oxford
#' setPhrase(items, oxford = FALSE)
#' 
#'@export
setPhrase <- function(setVector, oxford = TRUE, vectorize = FALSE){
  items <- length(setVector)
  out <- setVector
  # If n items > 2, append commas to each
  if(items > 2){
    out <- paste0(setVector, ",")
  }

  # add 'and' to second to last item
  out[(items-1)] <- paste0(out[(items-1)], " and")

  if(oxford){
    # If oxford drop last comma
    dropCommaIndex <- 0
  } else{
    # If not oxford drop last 2
    dropCommaIndex <- 1
  }

  # Remove unnecessary commas
  out[(items - dropCommaIndex):items] <- str_remove(out[(items - dropCommaIndex):items], ",")

  #
  if(!vectorize){
    out <- paste(out, collapse = " ")
  }

  return(out)

}

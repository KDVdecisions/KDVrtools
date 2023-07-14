# Eliot Dixon & Ashton Drew
# KDV Decision Analysis LLc
# 2023-07-14

#' Check if data values can be log transformed
#' 
#' Check if all columns within a data.frame can be converted to logscale
#' 
#' @param regData data.frame to be checked for compatibility with log scale
#' @return NULL
#' @examples
#'     checkPriorToLog(iris)
#'     testf <- data.frame(a = c(1, 2, 3, 0))
#'     checkPriorToLog(testDf)
#'  
#' @export 
checkPriorToLog <- function(regData) {
  allVars <- names(regData)
  for (i in seq(allVars)) {
    if (any(!is.numeric(regData[[allVars[i]]]))) {
      stop(paste0("Check the ", allVars[i], " column; Data must be numeric."))
    }
    if (any(is.infinite(log10(regData[[allVars[i]]])))) {
      stop(paste0("Check the ", allVars[i], " column; Data contain values that cannot be converted to log transform."))
    }
  }
  
  return(invisible(NULL))
}




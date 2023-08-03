# Splitting Shiny Lesson RMD Files into Multiple R files
# Author: EmmaLi Tsai
# Date: 7/3/2023
################################################################################

#' Generate multiple R files from one R markdown file
#'
#' This function takes the path to a single RMD document that contains headers 
#' (headers MUST be denoted by "# ", for example "# CORE ELEMENTS" or 
#' "# core elements") and breaks all sections into separate R files in your 
#' working directory. R files are named by the section title separated by 
#' an underscore (i.e., CORE_ELEMENTS.R). In the R files that are produced, 
#' text outside of code chunks appear as Roxygen comments, and chunks of code 
#' are denoted with "## ----chunkName--".
#'
#' @param file The path to the RMD file you'd like to split up. 
#' @return Multiple R files from each section of the RMD. 
#' @importFrom knitr purl 
#' @importFrom dplyr lead
#' @examples 
#'   file <- "Walkthrough_01_CombinedExercises.Rmd"
#'   splitMarkdownTutorial(file)
#'
#' @export 
################################################################################
splitMarkdownTutorial <- function(file = "Walkthrough_01_CombinedExercises.Rmd"){
  # transforming RMD document into an R file, while keeping in-line text as 
  # ROxygen comments:
  purl_file <- knitr::purl(file, documentation = 2)
  
  # scraping the file for code: 
  lessonRMD <- readLines(purl_file)
  
  # grabbing code lines of section breaks:
  sectionBreaks <- which(grepl("#' # [A-Z]", lessonRMD, ignore.case = TRUE))
  
  # guidance if markdown file does not contained specific headers: 
  if (length(sectionBreaks) == 0){
    stop("No headers detected. Make sure they are denoted with `# Section Name`")
  }
  
  # grabbing section titles:
  sectionTitles <- lessonRMD[grepl("#' #", lessonRMD)]
  # tidying section titles: 
  splitSectionTitles <- unlist(strsplit(sectionTitles, "#' # "))
  sectionTitles <- splitSectionTitles[seq(2, length(splitSectionTitles), by = 2)]
  
  # combining into a data frame: 
  sections <- data.frame(sectionTitle = sectionTitles, 
                         lineStart = sectionBreaks, 
                         lineEnd = dplyr::lead(sectionBreaks, 
                                               default = length(lessonRMD)) - 1)
  
  # looping to write multiple .R files of section breaks 
  for(i in 1:nrow(sections)){
    # grabbing loop row
    sectionI <- sections[i, ] 
    # grabbing section in R
    sectionCode <- lessonRMD[sectionI$lineStart:sectionI$lineEnd] 
    # finding file name
    fileName <- paste(sectionI$sectionTitle, ".R", sep = "") 
    # tidying file name
    fileNameTidy <- sub(" ", "_", fileName) 
    # writing
    write(sectionCode, fileNameTidy)
  }
  # return is NULL - data are written as R files to working environment 
  return(invisible(NULL))
}




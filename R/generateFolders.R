# Eliot Dixon
# KDV Decision Analysis LLC
# 2023-05-22


#' Create 1 or more folders with identical subfolder structure
#' 
#' Generates one or more folders within a given path, all of which will contain 
#' identical subfolders if specified.
#' 
#' @param path character, location in which to create folders
#' @param folders character vector of length 1 or more.  Folders to be created in location path
#' @param subfolders character vector of lenth 0 or more.  Subfolders to be created within path/Folders.
#' @return NULL
#' @examples
#'\dontrun{
#'# Create Folder structure with appropriate subfolders
#'generateFolders("C:/Users/KDV-Employee/Desktop/", 
#'                "P_testProj", c("Code", "Data"))
#'generateFolders("C:/Users/KDV-Employee/P_testProj/Data", 
#'                folders = c("Tidy", "Raw"), 
#'                subfolders = c("Docs", "Excel", "Figures", "Shapefle"))
#'}
#' @export
generateFolders <- function(path, folders, subfolders = NULL){
  
  if(!dir.exists(path)){
    stop(sprintf("Unable to find directory '%s'", path))
  }
  
  folderPaths <- sprintf("%s/%s", path, folders)
  #finalPaths <- sprintf("%s/%s", folders, subfolders)
  for(i in 1:length(folderPaths)){
    thisFolder <- folderPaths[i]
    
    # If folder already exists and is not empty
    if(length(list.files(thisFolder)) > 0){
      warning(sprintf("Directory '%s' already exists, skipping.", folders[i]))
    } else {
      # Create folder and subfolders
      dir.create(thisFolder, showWarnings = FALSE)
      subPaths <- sprintf("%s/%s", thisFolder, subfolders)
      sapply(subPaths, dir.create)
    }
  }
  
  return(NULL)
  
}



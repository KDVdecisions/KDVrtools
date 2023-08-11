# Eliot Dixon
# KDV Decision Analysis LLC
# 2023-05-22


#' Create 1 or more folders with specified subfolders
#' 
#' Generates one or more folders within a given path, all of which will contain 
#' identical subfolders if specified.
#' 
#' @param path character, location in which to create folders
#' @param folders character vector of length 1 or more.  Folders to be created in location path
#' @param subfolders character vector of length 0 or more.  Subfolders to be created within path/Folders.
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

  for(i in 1:length(folderPaths)){
    thisFolder <- folderPaths[i]
    
    # Grabbing all folder path components to identify parent folder for warning 
    # messages
    folderComponents <- unlist(strsplit(thisFolder, "/"))
    parentFolder <- folderComponents[length(folderComponents)]
    
    # Noting subfolders that are already present within parent folder: 
    allFolders <- list.files(thisFolder)
    present <- subfolders[which(subfolders %in% allFolders)]
    
    # Note extra subfolders that were not specified: 
    extra <- allFolders[!(allFolders %in% present)]
    if (length(extra) >= 1){
      extraString <- paste(extra, collapse = ", ")
      # Throw warning if extra subfolders are present but not specified: 
      warning(paste0("The following extra subfolders exist but were not specified: ", 
                     extraString, " - within parent folder ", parentFolder))
    }
    
    # Checking if any subfolders are already present: 
    if(any(subfolders %in% list.files(thisFolder))){
      # Throw warning if subfolders already present
      presentString <- paste(present, collapse = ", ")
      warning(paste0("The following subfolders already exist: ", presentString, 
                     " - skipping within parent folder ", parentFolder))
      
      # Locate missing subfolders that need to be added, if any: 
      missing <- subfolders[!(subfolders %in% present)]
      if(length(missing) != 0){
        # Add missing subfolders to parent folder: 
        dir.create(thisFolder, showWarnings = FALSE)
        subPaths <- sprintf("%s/%s", thisFolder, missing)
        sapply(subPaths, dir.create)
      }  else {
        # Noting that folder and subfolder are already present
        warning("Folder and subfolder already present. Skipping ", parentFolder)
      }
    } else {
      # If the whole folder doesn't exist, then create folder and subfolders:
      dir.create(thisFolder, showWarnings = FALSE)
      subPaths <- sprintf("%s/%s", thisFolder, subfolders)
      sapply(subPaths, dir.create)
    }
    
  }
  
  return(invisible(NULL))
  
}



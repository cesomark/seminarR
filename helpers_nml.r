#Recursively traverses subnodes and updates information if necessary
traverseNode <- function(node, songDataContainer) {
  subnodes <- xml_find_first(node, "SUBNODES")
  playlist <- xml_find_all(node, "PLAYLIST")
  
  if(length(xml_children(playlist)) > 0) {
    for(i in 1:length(xml_children(playlist))) {
      pk <- xml_find_first(xml_children(playlist)[[i]], "PRIMARYKEY")
      
      keySplitted <- splitNMLKey(xml_attr(pk, "KEY"))
      updatedSongData <- getUpdatedSongData(keySplitted$key, songDataContainer)
      if(!is.na(updatedSongData)[[1]]) {
        xml_attr(pk, "KEY")  <- paste(keySplitted$prefix, "/:", gsub(org_itemHeader, "", updatedSongData$header), sep="")
      }
      
    }
  }
  
  if(length(xml_children(subnodes)) > 0) {
    for(i in 1:length(xml_children(subnodes))) {
      traverseNode(xml_children(subnodes)[[i]], songDataContainer)
      
    }
  }
}

splitNMLKey <- function(key) {
  split <- str_locate_all(key[[1]], "/:")
  splitLocation <- split[[1]][[length(split[[1]]) / 2]]
  
  result <- list()
  result$prefix <- str_sub(key, 0, splitLocation-1)
  result$key <- str_sub(key, splitLocation + 2)
  
  result
}

getFileLocationForNML <- function(folderName) {
  paste(nml_seperator, SONGS_FOLDER, 
        nml_seperator, folderName, 
        nml_seperator, sep="")
}

#Searches for an updated song. If one is found, returns it with all needed information to update the nml
getUpdatedSongData <- function(songTitleNML, songDataContainer) {
  for(i in 1:length(songDataContainer)) {
    currentTitleORG = gsub(org_itemHeader, "", songDataContainer[[i]]$header)
    if(songDataContainer[[i]]$oldTitle == songTitleNML
       && songDataContainer[[i]]$oldTitle != currentTitleORG) {
      return(songDataContainer[[i]])
    }
  }
  return(NA)
}

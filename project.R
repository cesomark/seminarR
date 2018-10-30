#-------- Helper Functions ----------
initializeSongList <- function(folderNames) {
  tempList <- list()
  
  for(i in 1:length(folderNames)){
    tempList[[i]] <- list(list.files(folderNames[i]))
    tempList[[i]]$folderName <- folderNames[[i]]
  }
  
  tempList
}  

sortSongs <- function() {
  #creating the destination folder once at the start
  dir.create(file.path(getwd(), "sorted"), showWarnings = FALSE)
  
  for(i in 1:length(songList)) {
    for(j in 1:length(songList[[i]][[1]])) {
      
      copySongToSortedFolder(songList[[i]][[1]][[j]], songList[[i]]$folderName)
    
    }
  }
}

copySongToSortedFolder <- function(title, folder) {
  createFolderForSortedSong(title, folder)

  file.copy(from=file.path(getwd(), getOldSongPath(title, folder)), 
            to=file.path(getwd(), getNewSongPath(title, folder)), 
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
}

createFolderForSortedSong <- function(title, folder) {
  year <- getYearOfTrack(title, folder)
  location <- paste("sorted", year, sep="/")
  dir.create(file.path(getwd(), location), showWarnings = FALSE)
}

getOldSongPath <- function(title, folder) {
  paste(folder, title, sep="/")
}

getNewSongPath <- function(title, folder) {
  year <- getYearOfTrack(title, folder)
  paste("sorted", year, title, sep="/")
}

getYearOfTrack <- function(title, folder) {
  titleIndex <- regexpr('[0-9][0-9][0-9][0-9]', folder)[1]
  if(titleIndex != -1) {
    substr(folder, titleIndex, titleIndex + 3)
    
  } else {
    yearIndex <- regexpr('\\.[0-9][0-9][0-9][0-9]\\.', title)[1]
    
    if(yearIndex != -1) {
      substr(title, yearIndex+1, yearIndex + 4)
    } else {
      3000 
    }
  }
}
#-------------------------------------------------------


folderNames <- list.files("./tracks", full.names = TRUE)
songList <- initializeSongList(folderNames)

sortSongs()




#---------- Stuff -------------------
#--Get folder name for 30th list:
#songList[[30]]$folderName
#--Get folder content for 30th list:
#songList[[30]]
#--Get 5th song in list 30:
#songList[[30]][[1]][[5]]




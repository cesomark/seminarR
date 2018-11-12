if("stringr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("stringr", repos='http://cran.us.r-project.org')
  library("stringr")
}

#-------- Helper Functions ----------
initializeData <- function (folderNames) {
  songList <- initializeSongList(folderNames)
  
  songData <- list()
  songNumber = 1;
  for(i in 1:length(songList)) {
    for(j in 1:length(songList[[i]][[1]])) {
      songData[[songNumber]] <- getSongData(songList[[i]][[1]][[j]], songList[[i]]$folderName)
      songNumber <- songNumber + 1
    }
  }
  
  songData
}
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

getPrefix <- function(title) {
  prefix <- str_locate(title, regex("^[a-z]_", ignore_case = TRUE))
  if(is.na(prefix[1])) {
    NA
  } else {
    str_sub(title, prefix[1], prefix[2])
  }
}

getAuthor <- function(title) {
  if(is.na(str_locate(title, " - ")[1])) {
    NA
  } else {
    author <- str_split(title, " - ")[[1]]
    author <- str_remove(author, "^._")
    author[[1]]
  }
}

getSongTitle <- function(title) {
  if(!is.na(str_locate(title, " - ")[1])) {
    title <- str_split(title, " - ")[[1]][[2]]
  }
  #if(!is.na(str_locate(title, " \\(")[1])) {
  #  title <- str_split(title, " \\(")[[1]][[1]]
  #}
  #Remove version (if it exists)
  if(!is.na(str_locate(title, "\\(([^)]*)\\)[^(]*$")[1])) {
    title <- str_sub(title, 1, str_locate(title, "\\(([^)]*)\\)[^(]*$")[1]-2)
  }
    
  #If there is no version, cut off file extension from title
  if(!is.na(str_locate(title, "\\.[0-9]{4}\\..{3}$")[1])) {
    title <- str_split(title, "\\.[0-9]{4}\\..{3}$")[[1]][[1]]
  } else  if(!is.na(str_locate(title, "\\..{3}$")[1])) {
    title <- str_split(title, "\\..{3}$")[[1]][[1]]
  }
  
  title
}

getVersion <- function(title) {
  if(!is.na(str_locate(title, "\\(([^)]*)\\)[^(]*$")[1])) {
    version <- str_locate(title, "\\(([^)]*)\\)[^(]*$")
    version <- str_sub(title, version[1]+1, version[2])
    version <- str_split(version, "\\)\\.")[[1]][[1]]
    version
  } else {
    NA
  }
  
  
}

getExtension <- function(title) {
  ext = str_split(title, "\\.")
  ext[[1]][[length(ext[[1]])]]
}

getSongData <- function(title, folder) {
  tempList <- list()
  tempList$fullName <- title
  tempList$year <- getYearOfTrack(title, folder)
  tempList$prefix <- getPrefix(title)
  tempList$author <- getAuthor(title)
  tempList$title <- getSongTitle(title)
  tempList$version <- getVersion(title)
  tempList$extension <- getExtension(title)
  
  tempList
}
#------------------------------------------------------

folderNames <- list.files("./tracks", full.names = TRUE)

songDataContainer <- initializeData(folderNames)
#songDataContainer

#sortSongs()




#---------- Stuff -------------------
#--Get folder name for 30th list:
#songList[[30]]$folderName
#--Get folder content for 30th list:
#songList[[30]]
#--Get 5th song in list 30:
#songList[[30]][[1]][[5]]




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
    title <- str_sub(title, str_locate(title, " - ")[1] + 3)
  }
  #Remove version (if it exists)
  if(!is.na(str_locate(title, "\\(([^)]*)\\)[^(]*$")[1])) {
    title <- str_sub(title, 1, str_locate(title, "\\(([^)]*)\\)[^(]*$")[1]-1)
  }
    
  #If there is no version, cut off file extension from title
  if(!is.na(str_locate(title, "\\.[0-9]{4}\\..{3}$")[1])) {
    title <- str_split(title, "\\.[0-9]{4}\\..{3}$")[[1]][[1]]
  } else  if(!is.na(str_locate(title, "\\..{3}$")[1])) {
    title <- str_split(title, "\\..{3}$")[[1]][[1]]
  }

  title <- str_trim(title)
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

getShortAuthor <- function(author) {
  if(is.na(author)) {
    return(NA)
  } 
 
  keywords <- c(" feat", " ft", " presents", " pres", " with", " introduce")
  result <- NA
  for(i in 1:length(keywords)) {
    if(!is.na(str_locate(author, keywords[i])[1])) {
      result <- (paste(str_split(author, keywords[i])[[1]][[1]], "_", sep=""))
      break
    }
  }
  
  #Check, if name is too long and then remove vs and &.
  #Check both "result" and "autor", depending on if the author string had a keyword match
  if(is.na(result)) {
    result <- author
  }
  if(str_length(result) > 30) {
    keywords <- c(" vs", " \\&")
    for(i in 1:length(keywords)) {
      if(!is.na(str_locate(result, keywords[i])[1])) {
        result <- (paste(str_split(author, keywords[i])[[1]][[1]], "_", sep=""))
      }
    }
  } 

  if(author == result) {
    NA
  } else {
    result
  }
  
}

getShortTitle <- function(title) {
  if(is.na(title)) {
    return(NA)
  } 
  
  fullTitle <- title
  
  hasPars <- str_locate(title, " \\(")
  
  if(!is.na(hasPars[1])) {
    title <- paste(str_split(title, " \\(")[[1]][[1]], "_", sep="")
  }
  
  limit <- 20
  if(str_length(title) > limit) {
    title <- str_sub(title, 1, limit)
    title <- str_replace(title, " \\S*$", "")
    title <- paste(title, "_", sep="")
  }
  
  if(fullTitle == title) {
    NA
  } else {
    title
  }
  
}

getShortVersion <- function(version) {
  if(is.na(version)) {
    return(NA)
  } 
  
  fullVersion <- version
  
  hasPars <- str_locate(version, " \\(")
  if(!is.na(hasPars[1])) {
    version <- paste(str_split(version, " \\(")[[1]][[1]], "_", sep="")
  }
  
  limit <- 30
  if(str_length(version) > limit) {
    version <- str_sub(version, 1, limit)
    version <- str_replace(version, " \\S*$", "")
    version <- paste(version, "_", sep="")
  }
  
  if(fullVersion == version) {
    NA
  } else {
    version
  }
}

getSongData <- function(title, folder) {
  tempList <- list()
  tempList$fullName <- title
  tempList$year <- getYearOfTrack(title, folder)
  tempList$prefix <- getPrefix(title)
  tempList$author <- getAuthor(title)
  tempList$authorShort <- getShortAuthor(tempList$author)
  tempList$title <- getSongTitle(title)
  tempList$titleShort <- getShortTitle(tempList$title)
  tempList$version <- getVersion(title)
  tempList$versionShort <- getShortVersion(tempList$version)
  tempList$extension <- getExtension(title)
  
  tempList
}
#------------------------------------------------------

folderNames <- list.files("./tracks", full.names = TRUE)

songDataContainer <- initializeData(folderNames)
#songDataContainer

#sort songs not working atm!
#sortSongs()

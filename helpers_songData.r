initializeSongList <- function(folderNames) {
  tempList <- list()
  
  for(i in 1:length(folderNames)){
    tempList[[i]] <- list(list.files(folderNames[i]))
    tempList[[i]]$folderName <- folderNames[[i]]
  }
  
  tempList
}  

reportDuplicateSongs <- function(songs) {
  for(i in 1:length(songs)) {
    currentSong <- songs[[i]]
    
    for(j in 1:length(songs)) {
      if((currentSong$header == songs[[j]]$header) && (currentSong$year != songs[[j]]$year)) {
        print("DUPLICATE SONG DETECTED:")
        print(currentSong$header)
        print(paste("Occurrence 1 is from year: ", currentSong$year, sep=""))
        print(paste("Occurrence 2 is from year: ", songs[[j]]$year, sep=""))
        print("---------------")
      }
    }
  }
}

#Based on song title and folder, creates a new song and adds information
getSongData <- function(title, folder) {
  tempList <- list()
  tempList$year <- getYearOfTrack(title, folder)
  tempList$prefix <- getPrefix(title)
  
  tempList <- getAuthorInfo(tempList, title)
  tempList <- getTitleInfo(tempList, title)
  tempList <- getVersionInfo(tempList, title)
  
  tempList$extension <- getExtension(title)
  
  tempList$header <- getHeader(tempList)
  
  tempList$filePath <- paste(folder, title, sep="/")
  
  #the current folder where the song is located
  tempList$folder <- gsub(paste("^", SONGS_LOCATION, "/", sep=""), "", folder)
  tempList$oldTitle <- title
  
  
  tempList
}

#--- Helper functions for the getSongData function
getYearOfTrack <- function(title, folder) {
  titleIndex <- regexpr('[0-9]{4}', folder)[1]
  if(titleIndex != -1) {
    substr(folder, titleIndex, titleIndex + 3)
  } else {
    yearIndex <- regexpr('\\.[0-9]{4}\\.', title)[1]
    
    if(yearIndex != -1) {
      substr(title, yearIndex+1, yearIndex + 4)
    } else {
      SONG_NO_YEAR 
    }
  }
}

getPrefix <- function(title) {
  prefix <- str_locate(title, regex("^[a-z]_", ignore_case = TRUE))
  
  if(is.na(prefix[1])) { NA } 
  else {
    str_sub(title, prefix[1], prefix[2])
  }
}

getAuthorInfo <- function(list, title) {
  author <- NA
  authorPlus <- NA
  
  if(!is.na(str_locate(title, " - ")[1])) {
    author <- str_split(title, " - ")[[1]]
    author <- str_remove(author, "^._")[[1]]
    
    keywords <- c(" feat", " ft", " presents", " pres", " with", " introduce")
    for(i in 1:length(keywords)) {
      split <- str_locate(author, keywords[i])[1]
      if(!is.na(split)) {
        authorPlus <- str_sub(author, start=split+1)[[1]][[1]]
        author <- str_sub(author, 1, split)[[1]][[1]]
        
        break
      }
    }
    
    if(str_length(author) > SONG_PART_LIMIT + SONG_PART_LIMIT_BUFFER) {
      
      keywords <- c(" vs", " \\&")
      for(i in 1:length(keywords)) {
        split <- str_locate(author, keywords[i])[1]
        
        if(!is.na(split)) {
          if(is.na(authorPlus)) {
            authorPlus <- str_sub(author, start=split+1)[[1]][[1]]
          } else {
            authorPlus <- paste(str_sub(title, start=split+1), authorPlus, sep="")
          }
          author <- str_sub(author, 1, split)[[1]][[1]]
          
          break
        }
      }
    } 
  }
  
  list$author <- author
  list$authorPlus <- authorPlus
  list
}

getTitleInfo <- function(list, title) {
  fullTitle <- getFullTitle(title)
  list <- addTitleInfo(list, fullTitle)
  list
}

addTitleInfo <- function(list, title) {
  if(is.na(title)) {
    list$title <- NA
    list$titlePlus <- NA
    return(list)
  } 
  
  titlePlus <- NA
  
  hasPars <- str_locate(title, " \\(")
  
  if(!is.na(hasPars[1])) {
    splitTitle <- str_split(title, " \\(")
    
    title <- paste(splitTitle[[1]][[1]], "", sep="")
    titlePlus <- paste("(", splitTitle[[1]][[2]], sep="")
    
  }
  
  #if the title is (still) too long, remove part of the title
  if(str_length(title) > SONG_PART_LIMIT + SONG_PART_LIMIT_BUFFER) {
    if(is.na(titlePlus)) {
      titlePlus <- str_sub(title, start=SONG_PART_LIMIT + 1)
    } else {
      titlePlus <- paste(str_sub(title, start=SONG_PART_LIMIT + 1), titlePlus, sep="")
    }
    
    title <- str_sub(title, 1, SONG_PART_LIMIT)
  }
  
  list$title <- title
  list$titlePlus <- titlePlus
  list
}

getFullTitle <- function(title) {
  #Remove author (If exists)
  if(!is.na(str_locate(title, " - ")[1])) {
    title <- str_sub(title, str_locate(title, " - ")[1] + 3)
  }
  #Remove version (If exists)
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

getVersionInfo <- function(list, title) {
  version <- NA
  versionPlus <- NA
  if(!is.na(str_locate(title, "\\(([^)]*)\\)[^(]*$")[1])) {
    #regexes to extract version
    version <- str_locate(title, "\\(([^)]*)\\)[^(]*$")
    version <- str_sub(title, version[1]+1, version[2])
    version <- str_split(version, "\\)\\.")[[1]][[1]]
    
    hasPars <- str_locate(version, " \\(")
    if(!is.na(hasPars[1])) {
      split <- str_split(version, " \\(")
      version <- paste(split[[1]][[1]], "", sep="")
      versionPlus <- paste("(", split[[1]][[2]], sep="")
    }
    
    if(str_length(version) > SONG_PART_LIMIT + SONG_PART_LIMIT_BUFFER) {
      if(is.na(versionPlus)) {
        versionPlus <- str_sub(version, start=SONG_PART_LIMIT + 1)
      } else {
        versionPlus <- paste(str_sub(version, start=SONG_PART_LIMIT + 1), versionPlus, sep="")
      }
      version <- str_sub(version, 1, SONG_PART_LIMIT)
    }
  }
  
  list$version <- version
  list$versionPlus <- versionPlus
  list
}

getExtension <- function(title) {
  ext = str_split(title, "\\.")
  ext[[1]][[length(ext[[1]])]]
}

#Creates the header that is needed in the ORG file
getHeader <- function(list) {
  title <- "** "
  
  # --- Prefix ---
  if(!is.na(list$prefix)) {
    title <- paste(title, list$prefix, sep="")
  }
  
  # --- Author ---
  if(!is.na(list$author)) {
    title <- paste (title, list$author, sep="")
  }
  if(!is.na(list$authorPlus)) {
    title <- paste (title, "_", sep="")
  }
  if(!is.na(list$author)) {
    title <- paste (title, " - ", sep="")
  }
  
  # --- Title ---
  if(!is.na(list$title)) {
    title <- paste (title, list$title, sep="")
  }
  if(!is.na(list$titlePlus)) {
    title <- paste (title, "_", sep="")
  }
  
  # --- Version ---
  if(!is.na(list$version)) {
    title <- paste (title, " (", sep="")
    title <- paste (title, list$version, sep="")
    
    if(!is.na(list$versionPlus)) {
      title <- paste (title, "_", sep="")
    }
    title <- paste (title, ")", sep="")
  }
  
  # --- Extension ---
  if(!is.na(list$extension)) {
    title <- paste (title, ".", sep="")
    title <- paste (title, list$extension, sep="")
  }
  title
}
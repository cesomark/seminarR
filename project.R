if("stringr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("stringr", repos='http://cran.us.r-project.org')
  library("stringr")
}

#-------- Helper Functions ----------
parseORG <- function(fileLocation){
  
}

updateORG <- function () {
  if(file.exists("tracks.org")) {
    
   
    
    
  } else {
    file.create("tracks.org")
  }
  
}

checkIfSongInORG <- function(song) {
  #write("test", "tracks.org", append = TRUE)
  con = file("tracks.org", "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    print(line)
  }
  
  close(con)
  
  updateORGForSong(song)
}

updateORGForSong <- function(song) {
  
}

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

# sortSongs <- function() {
#   #creating the destination folder once at the start
#   dir.create(file.path(getwd(), "sorted"), showWarnings = FALSE)
# 
#   for(i in 1:length(songList)) {
#     for(j in 1:length(songList[[i]][[1]])) {
# 
#       copySongToSortedFolder(songList[[i]][[1]][[j]], songList[[i]]$folderName)
#     }
#   }
# }
# 
# copySongToSortedFolder <- function(title, folder) {
#   createFolderForSortedSong(title, folder)
# 
#   file.copy(from=file.path(getwd(), getOldSongPath(title, folder)),
#             to=file.path(getwd(), getNewSongPath(title, folder)),
#             overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
# }
# 
# createFolderForSortedSong <- function(title, folder) {
#   year <- getYearOfTrack(title, folder)
#   location <- paste("sorted", year, sep="/")
#   dir.create(file.path(getwd(), location), showWarnings = FALSE)
# }
# 
# getOldSongPath <- function(title, folder) {
#   paste(folder, title, sep="/")
# }
# 
# getNewSongPath <- function(title, folder) {
#   year <- getYearOfTrack(title, folder)
#   paste("sorted", year, title, sep="/")
# }

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
    
    limit <- 5
    if(str_length(author) > limit) {
    
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
  limit <- 20
  if(str_length(title) > limit+5) {
    if(is.na(titlePlus)) {
      titlePlus <- str_sub(title, start=limit+1)
    } else {
      titlePlus <- paste(str_sub(title, start=limit+1), titlePlus, sep="")
    }
    
    title <- str_sub(title, 1, limit)

    #remove the last word of a string
    #title <- str_replace(title, " \\S*$", "")
    #title <- paste(title, "_", sep="")
  }
  
  list$title <- title
  list$titlePlus <- titlePlus
  list
}

getFullTitle <- function(title) {
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
    
    limit <- 20
    if(str_length(version) > limit+5) {
      if(is.na(versionPlus)) {
        versionPlus <- str_sub(version, start=limit+1)
      } else {
        versionPlus <- paste(str_sub(version, start=limit+1), versionPlus, sep="")
      }
      version <- str_sub(version, 1, limit)
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

getSongData <- function(title, folder) {
  tempList <- list()
  tempList$fullName <- title
  tempList$year <- getYearOfTrack(title, folder)
  tempList$prefix <- getPrefix(title)
  
  tempList <- getAuthorInfo(tempList, title)
  tempList <- getTitleInfo(tempList, title)
  tempList <- getVersionInfo(tempList, title)
  
  tempList$extension <- getExtension(title)
  
  tempList
}
#------------------------------------------------------

folderNames <- list.files("./tracks", full.names = TRUE)

songDataContainer <- initializeData(folderNames)
orgContainer <- parseORG("tracks.org")
updateORG()
#songDataContainer

#sort songs not working atm!
#sortSongs()

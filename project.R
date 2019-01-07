if("stringr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("stringr", repos='http://cran.us.r-project.org')
  library("stringr")
}
if("xml2" %in% rownames(installed.packages()) == FALSE) {
  install.packages("xml2")
  library(xml2)
}

#-------- Global Vars ---------------
org_itemHeader <- "\\*\\* "
org_itemEnd <- ":END:"
org_itemStart <- ":PROPERTIES:"
org_itemAuthor <- ":Author:"    
org_itemAuthorPlus <- ":Author\\+:"
org_itemTitle <- ":Title:"     
org_itemTitlePlus <- ":Title\\+:"
org_itemVersion <- ":Version:"
org_itemVersionPlus <- ":Version\\+:"
org_itemYear <- ":Year:" 

nml_seperator <- "/:"

SONGS_LOCATION <- "./tracks"
SONGS_FOLDER <- gsub("^\\.\\/", "", SONGS_LOCATION)

orgFILE <- "tracks.org"
nmlFILE <- "collection.nml"

#-------- Helper Functions ----------
parseORG <- function(){
  #write("test", orgFILE, append = TRUE)
  
  org <- list()
  currentItem <- list()
  i = 1
  
  con <- NA
  if(file.exists(orgFILE)) {
    con <- file(orgFILE, "r")
  } else {
    file.create(orgFILE)
    return(con)
  }
  
  lineNumber = 0;
  line = NULL;
  while ( TRUE ) {
    #Read lines
    lineNumber = lineNumber + 1;
    line = readLines(con, n = 1, warn = FALSE, encoding="UTF-8")
    if ( length(line) == 0 ) {
      break
    }
    #create new song object
    if(str_detect(line, org_itemHeader)) {
      currentItem$fullName <- str_trim(str_remove(line, org_itemHeader))
      currentItem$start <- lineNumber
    }
    #add song to list
    else if(line == org_itemEnd) {
      currentItem$end <- lineNumber
      org[[i]] <- currentItem
      i = i + 1
      currentItem <- list()
      
    }
    #add content to current song object
    else {
      currentItem <- parseORGLine(currentItem, line)
    }
  }
  
  close(con)
  
  org
}

parseORGLine <- function(item, line) {
  if(str_detect(line, org_itemAuthor)) {
    item$author <- str_trim(str_remove(line, org_itemAuthor))
  }
  else if(str_detect(line, org_itemAuthorPlus)) {
    item$authorPlus <- str_trim(str_remove(line, org_itemAuthorPlus))
  }
  else if(str_detect(line, org_itemTitle)) {
    item$title <- str_trim(str_remove(line, org_itemTitle))
  }
  else if(str_detect(line, org_itemTitlePlus)) {
    item$titlePlus <- str_trim(str_remove(line, org_itemTitlePlus))
  }
  else if(str_detect(line, org_itemVersion)) {
    item$version <- str_trim(str_remove(line, org_itemVersion))
  }
  else if(str_detect(line, org_itemVersionPlus)) {
    item$versionPlus <- str_trim(str_remove(line, org_itemVersionPlus))
  }
  else if(str_detect(line, org_itemYear)) {
    item$year <- str_trim(str_remove(line, org_itemYear))
  }
  
  item
}

updateORG <- function (orgData, songData) {
  output <- readLines(orgFILE,-1, encoding="UTF-8")
  indexShift <- 0
  
  if(length(orgData) > 0){
    #Remove unnecessary entries in org
    for(i in 1:length(orgData)) {
      if(!orgEntryMatchesSong(orgData[[i]], songData)) {
        print("The following org entry has no matching song:")
        print(orgData[[i]]$fullName)
        print("---------------")
        
        # #Delete ORG entry if no matching sound track
        # start <- orgData[[i]]$start - indexShift
        # end <- orgData[[i]]$end - indexShift
        # output <- output[-(start : end)]
        # indexShift <- indexShift + (orgData[[i]]$end - orgData[[i]]$start) + 1
        
      } else {
        #ORG entry matches a song, do nothing
      }
    }
    writeLines(output, orgFILE, useBytes=T)
  }
  
  #Add new songs to ORG
  for(i in 1:length(songData)) {
    if(length(orgData) == 0 || !songInORG(orgData, songData[[i]])) {
      updateORGForSong(songData[[i]])
    } else {
      #song already in ORG, do nothing
    }
  }
}

songInORG<- function(orgData, song) {
  if(is.na(song$header)) {
    return(FALSE)
  }
  
  for(i in 1:length(orgData)) {
    if(paste("**", orgData[[i]]$fullName) == song$header) {
      return(TRUE)
    }
  }
  return(FALSE)
}

orgEntryMatchesSong <- function(orgEntry, songData) {
  if(length(songData) == 0) {
    return(FALSE)
  }
  
  for(i in 1:length(songData)) {
    if(paste("**", orgEntry$fullName) == songData[[i]]$header) {
      return(TRUE)
    }
  }
  return(FALSE)
}

updateORGForSong <- function(song) {
  #write("test", orgFILE, append = TRUE)
  # tempList <- list()
  # tempList$year <- getYearOfTrack(title, folder)
  # tempList$prefix <- getPrefix(title)
  # 
  # tempList <- getAuthorInfo(tempList, title)
  # tempList <- getTitleInfo(tempList, title)
  # tempList <- getVersionInfo(tempList, title)
  # 
  # tempList$extension <- getExtension(title)
  
  write(song$header, orgFILE, append = TRUE)
  write(org_itemStart, orgFILE, append = TRUE)
  
  if(!is.na(song$author)) {
    line <- paste(org_itemAuthor, song$author, sep=" ")
    write(line, orgFILE, append = TRUE)
  }
  if(!is.na(song$authorPlus)) {
    line <- paste(":Author+:", song$authorPlus, sep=" ")
    write(line, orgFILE, append = TRUE)
  }
  if(!is.na(song$title)) {
    line <- paste(org_itemTitle, song$title, sep=" ")
    write(line, orgFILE, append = TRUE)
  }
  if(!is.na(song$titlePlus)) {
    line <- paste(":Title+:", song$titlePlus, sep=" ")
    write(line, orgFILE, append = TRUE)
  }
  if(!is.na(song$version)) {
    line <- paste(org_itemVersion, song$version, sep=" ")
    write(line, orgFILE, append = TRUE)
  }
  if(!is.na(song$versionPlus)) {
    line <- paste(":Version+:", song$versionPlus, sep=" ")
    write(line, orgFILE, append = TRUE)
  }
  if(!is.na(song$year)) {
    line <- paste(org_itemYear, song$year, sep=" ")
    write(line, orgFILE, append = TRUE)
  }
  write(org_itemEnd, orgFILE, append = TRUE)
  
  
}

initializeData <- function (folderNames) {
  songList <- initializeSongList(folderNames)
  
  songData <- list()
  songNumber = 1;
  
  for(i in 1:length(songList)) {
    for(j in 1:length(songList[[i]][[1]])) {
      if(length(songList[[i]][[1]]) > 0) {
        title <- songList[[i]][[1]][[j]]
        folder <- songList[[i]]$folderName
        # 
        # Encoding(title) <- "UTF-8"
        # Encoding(folder) <- "UTF-8"
        
        songData[[songNumber]] <- getSongData(title, folder)
        songNumber <- songNumber + 1 
      }
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
  tempList$folder <- gsub(paste(paste("^", SONGS_LOCATION, sep=""), "/", sep=""), "", folder)
  tempList$oldTitle <- title
  
  
  tempList
}

updateSongs <- function(songs) {
  currentDir <- paste(getwd(), gsub("^\\.", "", SONGS_LOCATION), sep="")
  
  for(i in 1:length(songs)) {
    newPath <- paste(SONGS_LOCATION, "/", songs[[i]]$year, "/", gsub(org_itemHeader, "", songs[[i]]$header), sep="")
    if(!newPath == songs[[i]]$filePath) {
      dir.create(file.path(currentDir, songs[[i]]$year), showWarnings = FALSE)
      newPath <- paste(getwd(), gsub("^\\.", "", newPath), sep="")
      oldPath <- paste(getwd(), gsub("^\\.", "", songs[[i]]$filePath), sep="")
      
      file.rename(from=oldPath, to=newPath)
      # file.rename(from=enc2utf8(oldPath), to=enc2utf8(newPath))
    }
  }
}

# ---------------------------------------------
# ---------------- NML METHODS ----------------
# ---------------------------------------------
updateNML <- function(songDataContainer) {
  nml <- list()
  
  if(!file.exists(nmlFILE)) {
    print(paste("WARNING: NML FILE \'", nmlFILE, "\' DOES NOT EXIST!", sep=""))
    return(nml)
  } 
  nml <- read_xml(file(nmlFILE))
  
  #Update "COLLECTION"
  collectionEntries <- xml_children(xml_child(nml, "COLLECTION"))

  for(i in 1:length(collectionEntries)) {
    if(xml_name(collectionEntries[[i]]) == "ENTRY") {
      entry <- collectionEntries[[i]]
      location <- xml_child(collectionEntries[[i]], "LOCATION")
      fileName <- xml_attr(location, "FILE")

      #first check, if the songtitle has changed. if it did, update data
      updatedSongData <- getUpdatedSongData(fileName, songDataContainer)
      if(!is.na(updatedSongData)[[1]]) {
        xml_attr(location, "FILE")  <- gsub(org_itemHeader, "", updatedSongData$header)
        xml_attr(location, "DIR")   <- getFileLocationForNML(updatedSongData$folder)
        xml_attr(entry, "TITLE")    <- if(!is.na(updatedSongData$titlePlus)) updatedSongData$title
                                        else paste(updatedSongData$title, "_", sep="")
        xml_attr(entry, "ARTIST")   <- if(!is.na(updatedSongData$authorPlus)) updatedSongData$author
                                        else paste(updatedSongData$author, "_", sep="")
      }
    }
  }
  
  #Update "PLAYLISTS"
  playlists <- xml_child(nml, "PLAYLISTS")
  nodes <- xml_find_all(playlists, "NODE")
  for(i in 1:length(nodes)) {
    traverseNode(nodes[[i]], songDataContainer)
  }

  write_xml(nml, "testfileOutput.nml")
  nml
}

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

#------------------------------------------------------

folderNames <- list.files(SONGS_LOCATION, full.names = TRUE)
if(length(folderNames) == 0) {
  stop(paste("ERROR: '", SONGS_FOLDER, "' folder does not exist or is empty", sep=""))
}

songDataContainer <- initializeData(folderNames)
updateSongs(songDataContainer)
orgContainer <- parseORG()
updateORG(orgContainer, songDataContainer)
# nmlContainer <- updateNML(songDataContainer)


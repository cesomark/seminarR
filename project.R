# Starting point of this program:
# Bottom of the file (line 223)

#--- Needed packages / sources ---
if("stringr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("stringr", repos='http://cran.us.r-project.org')
  library("stringr")
}
if("xml2" %in% rownames(installed.packages()) == FALSE) {
  install.packages("xml2")
  library(xml2)
}
source("helpers_songData.r")
source("helpers_org.r")
source("helpers_nml.r")

#----------- OPTIONS ----------------
#If enabled, deletes mismatches in the ORG file. (Mismatches are always reported)
DELETE_ORG_MISMATCH <- FALSE

orgFILE <- "tracks.org"
nmlFILE <- "collection.nml"
#Songs location folder should always start with ./
SONGS_LOCATION <- "./tracks"
#If a song's release year is not clear, SONG_NO_YEAR will be used
SONG_NO_YEAR <- 3000

#If a part of the file name (e.g. title or artist) is longer then limit + buffer,
#shorten it to SONG_PART_LIMIT
SONG_PART_LIMIT <- 16
SONG_PART_LIMIT_BUFFER <- 5

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

SONGS_FOLDER <- gsub("^\\.\\/", "", SONGS_LOCATION)

nml_seperator <- "/:"

#------------ Functions -----------------
#Parsing song info into the songDataContainer
#Helper functions can be found in: helpers_songData.r
initializeData <- function (folderNames) {
  songList <- initializeSongList(folderNames)
  songData <- list()
  
  songNumber = 1;
  for(i in 1:length(songList)) {
    for(j in 1:length(songList[[i]][[1]])) {
      if(length(songList[[i]][[1]]) > 0) {
        title <- songList[[i]][[1]][[j]]
        folder <- songList[[i]]$folderName
        
        songData[[songNumber]] <- getSongData(title, folder)
        songNumber <- songNumber + 1 
      }
    }
  }
  songData
}

#Updating songs (file name, file location)
#Also, reports duplicates (if they appear)
updateSongs <- function(songs) {
  currentDir <- paste(getwd(), gsub("^\\.", "", SONGS_LOCATION), sep="")
  
  for(i in 1:length(songs)) {
    newPath <- paste(SONGS_LOCATION, "/", songs[[i]]$year, "/", gsub(org_itemHeader, "", songs[[i]]$header), sep="")
    if(!newPath == songs[[i]]$filePath) {
      dir.create(file.path(currentDir, songs[[i]]$year), showWarnings = FALSE)
      newPath <- paste(getwd(), gsub("^\\.", "", newPath), sep="")
      oldPath <- paste(getwd(), gsub("^\\.", "", songs[[i]]$filePath), sep="")
      
      file.rename(from=oldPath, to=newPath)
    }
  }
  
  reportDuplicateSongs(songs)
}

#Parsing ORG data into the orgContainer
#Helper functions can be found in: helpers_org.r
parseORG <- function(){
  org <- list()
  #current ORG entry, that will be added to the orgContainer
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

#Update ORG with correct song data
#Helper functions in helpers_org.r
updateORG <- function (orgData, songData) {
  output <- readLines(orgFILE,-1, encoding="UTF-8")
  #indexShift is needed when the option to delete mismatches is enabled
  indexShift <- 0
  
  #Handle mismatches (print a message and if option enabled, also delete the entry from the org file)
  if(length(orgData) > 0){
    for(i in 1:length(orgData)) {
      if(!orgEntryMatchesSong(orgData[[i]], songData)) {
        print("THE FOLLOWING ORG ENTRY HAS NO MATCHING SONG:")
        print(orgData[[i]]$fullName)
        
        if(DELETE_ORG_MISMATCH) {
          start <- orgData[[i]]$start - indexShift
          end <- orgData[[i]]$end - indexShift
          output <- output[-(start : end)]
          indexShift <- indexShift + (orgData[[i]]$end - orgData[[i]]$start) + 1
          print("Deleting this org entry!")
        }
        print("---------------")
      } 
    }
    writeLines(output, orgFILE, useBytes=T)
  }
  
  #Add new songs to ORG
  for(i in 1:length(songData)) {
    if(length(orgData) == 0 || !songInORG(orgData, songData[[i]])) {
      updateSongInORG(songData[[i]])
    }
  }
}

#Updating NML with correct data
#Helper functions in helpers_nml.r
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
      info <- xml_child(collectionEntries[[i]], "INFO")
      fileName <- xml_attr(location, "FILE")

      #first check, if the songtitle has changed. if it did, update data
      updatedSongData <- getUpdatedSongData(fileName, songDataContainer)
      if(!is.na(updatedSongData)[[1]]) {
        xml_attr(location, "FILE")  <- gsub(org_itemHeader, "", updatedSongData$header)
        xml_attr(location, "DIR")   <- getFileLocationForNML(updatedSongData$folder)
        xml_attr(entry, "TITLE")    <- if(is.na(updatedSongData$titlePlus)) updatedSongData$title
                                        else paste(updatedSongData$title, "_", sep="")
        xml_attr(entry, "ARTIST")   <- if(is.na(updatedSongData$authorPlus)) updatedSongData$author
                                        else paste(updatedSongData$author, "_", sep="")
        
        if(!is.na(updatedSongData$version)) {
          xml_attr(info, "MIX")     <- if(is.na(updatedSongData$versionPlus)) updatedSongData$version
                                        else paste(updatedSongData$version, "_", sep="")
        }
      }
    }
  }
  
  #Update "PLAYLISTS"
  playlists <- xml_child(nml, "PLAYLISTS")
  nodes <- xml_find_all(playlists, "NODE")
  for(i in 1:length(nodes)) {
    traverseNode(nodes[[i]], songDataContainer)
  }

  write_xml(nml, nmlFILE)
  nml
}
#------------------------------------------------------

#creates a list out of all folder names in SONGS_LOCATION
folderNames <- list.files(SONGS_LOCATION, full.names = TRUE)
if(length(folderNames) == 0) {
  stop(paste("ERROR: '", SONGS_FOLDER, "' folder does not exist or is empty", sep=""))
}

songDataContainer <- initializeData(folderNames)
updateSongs(songDataContainer)
orgContainer <- parseORG()
updateORG(orgContainer, songDataContainer)
nmlContainer <- updateNML(songDataContainer)

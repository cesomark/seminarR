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

#Checks, if the current org entry has a matching song
orgEntryMatchesSong <- function(orgEntry, songData) {
  if(length(songData) == 0) { return(FALSE) }
  
  for(i in 1:length(songData)) {
    if(paste("**", orgEntry$fullName) == songData[[i]]$header) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#Checks, if the current song is already in the org
songInORG<- function(orgData, song) {
  if(is.na(song$header)) { return(FALSE) }
  
  for(i in 1:length(orgData)) {
    if(paste("**", orgData[[i]]$fullName) == song$header) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#Updates org with song data
updateSongInORG <- function(song) {
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

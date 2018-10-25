folderNames = list.files("./tracks", full.names = TRUE)
songList = list()

for(i in 1:length(folderNames)){
  songList[[i]] <- list(list.files(folderNames[i]))
  songList[[i]]$folderName <- folderNames[[i]]
  
}
#songList[[30]]$folderName
#songList[[30]]
#--access 5th song in list 30:
#songList[[30]][[1]][[5]]



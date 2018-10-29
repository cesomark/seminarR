getYearInTitle <- function(title, folderName) {
  titleIndex <- regexpr('[0-9][0-9][0-9][0-9]', folderName)[1]
  if(titleIndex != -1) {
    substr(folderName, titleIndex, titleIndex + 3)
    
  } else {
    yearIndex <- regexpr('\\.[0-9][0-9][0-9][0-9]\\.', title)[1]
    
    if(yearIndex != -1) {
      substr(title, yearIndex+1, yearIndex + 4)
    } else {
      NULL 
    }
  }
}

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

#x <- songList[[29]][[1]][[33]]
x <- songList[[30]][[1]][[6]]
x

lol <- getYearInTitle(x, songList[[30]]$folderName)
lol




library(stringr)
library(data.table)
library(foreach)
library(doParallel)

getLyricWordCount = function(songDT, wordExclude = NULL){
  
  # Use data.table syntax instead of dataframe syntax
  songDT[,lyrics := gsub('\\[(.*?)\\]', ' ', lyrics)]
  # Parse everything except alphanumeric out in one expression
  songDT[,lyrics := gsub('\'', '', lyrics)]
  songDT[,lyrics := gsub('[^a-zA-Z]', ' ', lyrics)]
  songDT[,lyrics := str_squish(lyrics)]
  songDT[,lyrics := tolower(lyrics)]
  
  # Use method to get one row per song/word occurrence, then use the by and .N to get count of each row
  # Look into quanteda package that returns frequency of string within string
  
  # Use iterators package to define iterator for foreach instead of idx
  lyricDT = foreach (idx = 1:nrow(songDT), .combine = rbind) %do% {
    
    sDT = songDT[idx,]
    
    data.table(name = songDT[idx,name], id.x = songDT[idx,id.x], word = strsplit(songDT[idx,lyrics], ' ')[[1]])
    
  }
  
  if (!(is.null(wordExclude))) {
    lyricDT = lyricDT[which(!(word %in% wordExclude)),]
  }
  lyricDT[,count := .N, by=c('name', 'id.x', 'word')]
  lyricDT = unique(lyricDT)
  return(lyricDT)
}

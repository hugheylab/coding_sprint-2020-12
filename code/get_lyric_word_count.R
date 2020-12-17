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

    wList = strsplit(songDT[idx,lyrics], ' ')[[1]]
    if (!(is.null(wordExclude))) {
      wList = wList[which(!(wList %in% wordExclude))]
    }

    data.table(track_name = songDT[idx, track_name], track_id = songDT[idx, track_id], word = wList, word_total = length(wList))

  }

  lyricDT[,count := .N, by=c('track_name', 'track_id', 'word')]
  lyricDT = unique(lyricDT)
  return(lyricDT)
}

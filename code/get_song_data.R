library(httr)
library(stringr)
library(glue)
library(jsonlite)
library(data.table)
library(rvest)
library(doParallel)
registerDoParallel(cores = 2)

getSpotifyData = function (u, ...) {
  
  dt = GET(url = u 
           , config = add_headers(authorization = authHeader)
           , encode = 'json'
           , ...)
  dt = fromJSON(content(dt, as = 'text', encoding = 'UTF-8')
                , flatten = TRUE)
  dt = as.data.table(dt)
  
  return(dt)}

searchGenius = function (param) {
  
  search = GET(url = glue('{rootAPI}/search')
               , config = add_headers(authorization = authHeader)
               , encode = 'json'
               , query = list(q = param))
  search = fromJSON(content(search, as = 'text', encoding = 'UTF-8')
                    , flatten = TRUE)
  
  res = as.data.table(search$response[[1]])
  
  if (nrow(res) > 0) {
    
    setnames(res, names(res), str_remove(names(res), 'result.'))
    
    res = res[
      , .(full_title
          , id
          , url)]
  }
  
  res[, q := param]
  
  return(res[1])
}

getLyrics = function (u) {
  
  song = read_html(u)
  lyrics = html_text(
    html_nodes(song, '.lyrics')
  )
  
  return(lyrics)
  
}

#spotify api 
sp_id = '92b29347bd714e259688e4f720be422e'
sp_secret = 'a81701a8e81c466abf5387f2aeaf0ebf'

response = POST(
  'https://accounts.spotify.com/api/token'
  , accept_json()
  , authenticate(sp_id, sp_secret)
  , body = list(grant_type = 'client_credentials')
  , encode = 'form'
  , verbose()
)

token = content(response)$access_token
authHeader = paste0('Bearer ', token)


rootAPI = 'https://api.spotify.com/v1'
playlistID = '37i9dQZF1DX0Yxoavh5qJV'


playlist = getSpotifyData(glue('{rootAPI}/playlists/{playlistID}/tracks'))
trackIDs = playlist$items.track.id

tracks = foreach (i = list(1:50, 51:94), .combine = rbind) %do% {
  
  tracks = getSpotifyData(glue('{rootAPI}/tracks')
                          , query = list(ids = glue_collapse(trackIDs[i]
                                                             , sep = ',')))}
tracks = tracks[
  , .(track_id = tracks.id, track_name = tracks.name, artists = tracks.artists
      , popularity = tracks.popularity)]
tracks = merge(tracks
               , tracks[, .(artist = artists[[1]]$name
                            , artist_id = artists[[1]]$id)
                        , by = track_id]
               , by = 'track_id')
tracks[, artists := NULL]

artistIDs = unique(tracks$artist_id)


artists = foreach(i = list(1:50, 51:length(artistIDs)), .combine = rbind) %do% {
  
  artists = getSpotifyData(glue('{rootAPI}/artists')
                           , query = 
                             list(ids = glue_collapse(artistIDs[i], sep = ',')))
  
}
artists = artists[, .(artist_id = artists.id
                      , genre = artists.genres)]


features = getSpotifyData(glue('{rootAPI}/audio-features')
                   , query = list(ids = glue_collapse(trackIDs, sep = ',')))
setnames(features
         , names(features)
         , str_remove(names(features), 'audio_features.'))
features[
  , track_id := id
  ][
    , `:=`(uri = NULL
           , track_href = NULL
           , analysis_url = NULL
           , type = NULL
           , id = NULL)]

spotifyData = merge(tracks, artists, by = 'artist_id')
spotifyData[, artist_id := NULL]
spotifyData = dcast(spotifyData
                    , track_id + track_name + popularity ~ rowid(track_id)
                    , value.var = c('artist', 'genre'))
spotifyData[
  , genre := list(list(Reduce(c, do.call(c, .SD))))
  , .SDcols = 8:11
  , by = track_id]
spotifyData[
  , `:=`(genre_1 = NULL
         , genre_2 = NULL
         , genre_3 = NULL
         , genre_4 = NULL)]

spotifyData[
  , `:=`(song_artist_1 = paste(artist_1, track_name)
         , song_artist_2 = paste(artist_2, track_name)
         , song_artist_3 = paste(artist_3, track_name)
         , song_artist_4 = paste(artist_4, track_name))
  ][
    , `:=`(song_artist_1 = str_remove(song_artist_1, ' -.*$')
           , song_artist_2 = str_remove(song_artist_2, ' -.*$')
           , song_artist_3 = str_remove(song_artist_3, ' -.*$')
           , song_artist_4 = str_remove(song_artist_4, ' -.*$'))
    ][
      , `:=`(song_artist_1 = str_remove(song_artist_1, '[ ]\\(.*')
             , song_artist_2 = str_remove(song_artist_2, '[ ]\\(.*')
             , song_artist_3 = str_remove(song_artist_3, '[ ]\\(.*')
             , song_artist_4 = str_remove(song_artist_4, '[ ]\\(.*'))]

spotifyData = merge(spotifyData, features, by = 'track_id')

#genius api 
ge_id = 'yb7_hw65sxtTeD2_7oBVJ2JVlegmYaxqwQiZkpPFpwXb1785wuNWc2Cz3mwogE6O'
ge_secret = 
  '90OOLGccpH5DyTEbCAruFF7hyfZwZ0AEvFCNPPLWXDUpDiGBIvujusWF9xcGKgPMUAMpTcEH0oD7_csIViMgwA'

response = POST(
  'https://api.genius.com/oauth/token'
  , accept_json()
  , authenticate(ge_id, ge_secret)
  , body = list(grant_type = 'client_credentials')
  , encode = 'form'
  , verbose()
)

token = content(response)$access_token
authHeader = paste0('Bearer ', token)

rootAPI = 'https://api.genius.com'

searchParams = c(spotifyData[, song_artist_1]
                 , spotifyData[!is.na(artist_2), song_artist_2]
                 , spotifyData[!is.na(artist_3), song_artist_3]
                 , spotifyData[!is.na(artist_4), song_artist_4])

geniusData = lapply(searchParams, searchGenius)
geniusData = rbindlist(geniusData, fill = TRUE)
geniusData[
  !is.na(url)
  , lyrics := mapply(getLyrics, url)]


songData = merge(spotifyData, geniusData, by.x  = 'song_artist_1', by.y = 'q')
songData = rbind(songData
                 , merge(spotifyData[track_id %in% songData[is.na(lyrics)
                                                            , track_id]]
                         , geniusData, by.x = 'song_artist_2', by.y = 'q'))
songData = songData[!is.na(lyrics)]

songData[, `:=`(url = NULL, full_title = NULL, id = NULL
                , song_artist_1 = NULL, song_artist_2 = NULL
                , song_artist_3 = NULL, song_artist_4 = NULL)]

pitchClass = data.table(keyVal = 0:11
                        , label = c('C', 'C#/Db', 'D', 'D#/Eb', 'E', 'F'
                                    , 'F#/Gb', 'G', 'G#/Ab', 'A', 'A#/Bb'
                                    , 'B'))
setnames(pitchClass, 'keyVal', 'key')

songData[, key := as.character(factor(key, labels = pitchClass[, label]))]

songFile = file.path('Documents', 'Work', 'spotify', 'song_data.csv.gz')
fwrite(songData, songFile)


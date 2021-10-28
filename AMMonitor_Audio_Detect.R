library(AMMonitor)
library(AMModels)
library(lubridate)
library(stringr)
library(soundecology)
library(ggplot2)

siteID <- 'NEW429' #Site ID
equipID <- '21A' #recorder ID
deployDate <- '2021-03-10' #date deployed

db.path <- paste0(getwd(),paste0('/database/',paste0(siteID,'.sqlite')))
conx <- RSQLite::dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
RSQLite::dbExecute(conn = conx, statement = "PRAGMA foreign_keys = ON;")

setwd('C:/Dropbox')


# file rename
## format 'yyyymmdd hhmmss.wav'
AudioFiles <- list.files(path = "recording_drop", pattern = ".WAV", all.files = TRUE,
                         full.names = TRUE, recursive = TRUE,
                         ignore.case = TRUE, include.dirs = TRUE)
AudioFiles2 <- parse_date_time(AudioFiles, "Ymd HMS", tz = 'UTC')
AudioFiles3 <- as.character(AudioFiles2)
AudioFiles4 <- str_replace_all(AudioFiles3,' ','_')
AudioFiles5 <- str_replace_all(AudioFiles4,':','-')
AudioFiles6 <- paste0('~',siteID,'_',AudioFiles5)
AudioFiles7 <- str_replace_all(AudioFiles6,'~','recording_drop/')
AudioFiles8 <- file.rename(AudioFiles,paste0(AudioFiles7,'.wav'))

# fill recordings table
dropboxMoveBatch(db.path = db.path,
                 table = 'recordings', 
                 dir.from = 'recording_drop', 
                 dir.to = 'recordings', 
                 token.path = 'settings/dropbox-token.RDS')

#run detections
ranscores <- scoresDetect(db.path = db.path, 
                          directory = 'recordings', 
                          recordingID = 'all',
                          templateID = 'all',
                          score.thresholds = c(13,12,12,13,0.4),
                          #listID = 'Target Species Templates',     
                          token.path = 'settings/dropbox-token.RDS', 
                          db.insert = TRUE) 

scores <- dbGetQuery(conn = conx, 
                     statement = "SELECT scoreID, recordingID, templateID, 
                                         time, scoreThreshold, score, 
                                         manualVerifyLibraryID, manualVerifySpeciesID
                                  FROM scores")
write.csv(scores,paste0(siteID,'_2021_scores.csv'))

#sound scape
dropboxGetOneFile(
  file = '.wav', 
  directory = 'recordings', 
  token.path = 'settings/dropbox-token.RDS', 
  local.directory = getwd())

wavSample <- tuneR::readWave(filename = '.wav')
soundecology::acoustic_complexity(soundfile = wavSample)

AMMonitor::soundscape(db.path = db.path,
                      recordingID = wavSample,
                      directory = 'recordings', 
                      token.path = 'settings/dropbox-token.RDS', 
                      db.insert = TRUE)

# export csv of scores
scores <- dbGetQuery(conn = conx, 
                     statement = "SELECT scoreID, recordingID, templateID, 
                                         time, scoreThreshold, score, 
                                         manualVerifyLibraryID, manualVerifySpeciesID
                                  FROM scores")
ggplot(scores) +
  geom_point(aes(x = time, y= recordingID))

write.csv(scores,paste0(siteID,'_2021_scores.csv'))

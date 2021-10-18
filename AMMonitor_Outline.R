library(AMMonitor)
library(AMModels)
library(lubridate)
library(stringr)
library(soundecology)

#set recorder ID
VPMonID <- 'SDF791' #Pool ID
equipID <- '21N' #recorder ID
depoyDay <- '2021-03-25' #date deployed

#create directories, resetWD
ammCreateDirectories(amm.dir.name = "AMMonitor", 
                     file.path = "C:/Dropbox")

setwd('C:/Dropbox')

# Create/save libraries and add metadata
activity <- AMModels::amModelLib(description = "This library stores models that predict species activity patterns.")
classifiers <- AMModels::amModelLib(description = "This library stores classification models (machine learning models) that can be used to predict the probability that a detected signal is from a target species.")
soundscape <- AMModels::amModelLib(description = "This library stores results of a soundscape analysis.")
do_fp <- AMModels::amModelLib(description = "This library stores results of dynamic occupancy analyses that can handle false positive detections.")
info <- list(PI = 'Steve Faccio',
             Coordinator = 'Kevin Tolan',
             Organization = 'Vermont Center for Ecostudies')
ammlInfo(activity) <- info
ammlInfo(classifiers) <- info
ammlInfo(soundscape) <- info
ammlInfo(do_fp) <- info
saveRDS(object = activity, file = "ammls/activity.RDS")
saveRDS(object = classifiers, file = "ammls/classifiers.RDS")
saveRDS(object = soundscape, file = "ammls/soundscape.RDS")
saveRDS(object = do_fp, file = "ammls/do_fp.RDS")

# create SQLite database
dbCreate(db.name = paste0(VPMonID,'.sqlite'), 
         file.path = paste0(getwd(),"/database")) 
#### ALWAYS RUN
db.path <- '_________.sqlite'
conx <- RSQLite::dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
RSQLite::dbExecute(conn = conx, statement = "PRAGMA foreign_keys = ON;")

#### Add necessary components ####
#people
add.people <- data.frame(personID = 'ktolan@vtecostudies.org',
                         firstName = 'Kevin',
                         lastName = 'Tolan',
                         projectRole = 'Coordinator',
                         email = 'ktolan@vtecostudies.org')
RSQLite::dbWriteTable(conn = conx, name = 'people', value = add.people,
                      row.names = FALSE, overwrite = FALSE,
                      append = TRUE, header = FALSE)
# species
new.species <- data.frame(speciesID = 'wofr', 
                          commonName = 'Wood Frog',
                          ITIS = '775117',
                          genus = 'Lithobates',
                          species = 'sylvaticus',
                          notes = NA, 
                          stringsAsFactors = FALSE)
RSQLite::dbWriteTable(conn = conx, name = 'species', value = new.species,
                      row.names = FALSE, overwrite = FALSE,
                      append = TRUE, header = FALSE)
new.species <- data.frame(speciesID = 'sppe', 
                          commonName = 'Spring Peeper',
                          ITIS = '207303',
                          genus = 'Pseudacris',
                          species = 'crucifer',
                          notes = NA, 
                          stringsAsFactors = FALSE)
RSQLite::dbWriteTable(conn = conx, name = 'species', value = new.species,
                      row.names = FALSE, overwrite = FALSE,
                      append = TRUE, header = FALSE)
new.species <- data.frame(speciesID = 'bado', 
                          commonName = 'Barred Owl',
                          ITIS = '177921',
                          genus = 'Strix',
                          species = 'varia',
                          notes = NA, 
                          stringsAsFactors = FALSE)
RSQLite::dbWriteTable(conn = conx, name = 'species', value = new.species,
                      row.names = FALSE, overwrite = FALSE,
                      append = TRUE, header = FALSE)
new.species <- data.frame(speciesID = 'ewpw', 
                          commonName = 'Eastern Whip-poor-will',
                          ITIS = '1077358',
                          genus = 'Antrostomus',
                          species = 'vociferus',
                          notes = NA, 
                          stringsAsFactors = FALSE)
RSQLite::dbWriteTable(conn = conx, name = 'species', value = new.species,
                      row.names = FALSE, overwrite = FALSE,
                      append = TRUE, header = FALSE)
new.species <- data.frame(speciesID = 'easo', 
                          commonName = 'Eastern Screech-Owl',
                          ITIS = '686658',
                          genus = 'Megascops',
                          species = 'asio',
                          notes = NA, 
                          stringsAsFactors = FALSE)
RSQLite::dbWriteTable(conn = conx, name = 'species', value = new.species,
                      row.names = FALSE, overwrite = FALSE,
                      append = TRUE, header = FALSE)
#library
new.library <- data.frame(libraryID = 'wofr', 
                          speciesID = 'wofr',
                          type = NA,
                          description = NA)
RSQLite::dbWriteTable(conn = conx, name = 'library', value = new.library,
                      row.names = FALSE, overwrite = FALSE,
                      append = TRUE, header = FALSE)
new.library <- data.frame(libraryID = 'sppe', 
                          speciesID = 'sppe',
                          type = NA,
                          description = NA)
RSQLite::dbWriteTable(conn = conx, name = 'library', value = new.library,
                      row.names = FALSE, overwrite = FALSE,
                      append = TRUE, header = FALSE)
new.library <- data.frame(libraryID = 'bado', 
                          speciesID = 'bado',
                          type = 'Hoot',
                          description = NA)
RSQLite::dbWriteTable(conn = conx, name = 'library', value = new.library,
                      row.names = FALSE, overwrite = FALSE,
                      append = TRUE, header = FALSE)
new.library <- data.frame(libraryID = 'ewpw', 
                          speciesID = 'ewpw',
                          type = 'Classic song',
                          description = NA)
RSQLite::dbWriteTable(conn = conx, name = 'library', value = new.library,
                      row.names = FALSE, overwrite = FALSE,
                      append = TRUE, header = FALSE)
new.library <- data.frame(libraryID = 'easo', 
                          speciesID = 'easo',
                          type = 'Hoot',
                          description = NA)
RSQLite::dbWriteTable(conn = conx, name = 'library', value = new.library,
                      row.names = FALSE, overwrite = FALSE,
                      append = TRUE, header = FALSE)
# location
new.location <- data.frame(locationID = VPMonID,
                           type = 'Vernal Pool ARU',
                           lat = '1',
                           long = '1',
                           datum = 'WGS84',
                           tz = 'UTC',
                           personID = 'ktolan@vtecostudies.org')
RSQLite::dbWriteTable(conn = conx, name = 'locations', value = new.location,
                      row.names = FALSE, overwrite = FALSE,
                      append = TRUE, header = FALSE)
# accounts
new.account <- data.frame(accountID = VPMonID)
RSQLite::dbWriteTable(conn = conx, name = 'accounts', value = new.account,
                      row.names = FALSE, overwrite = FALSE,
                      append = TRUE, header = FALSE)
# equipment
new.equipment <- data.frame(equipmentID = equipID,
                            accountID = VPMonID) 
RSQLite::dbWriteTable(conn = conx, name = 'equipment', value = new.equipment,
                      row.names = FALSE, overwrite = FALSE,
                      append = TRUE, header = FALSE)
# deployment
new.deployment <- data.frame(equipmentID = equipID,
                             locationID = VPMonID,
                             dateDeployed = depoyDay,
                             dateRetrieved = NA) # DO NOT FILL IN DATE RETRIEVED
RSQLite::dbWriteTable(conn = conx, name = 'deployment', value = new.deployment,
                      row.names = FALSE, overwrite = FALSE,
                      append = TRUE, header = FALSE)
# schedule
new.schedule <- data.frame(equipmentID = equipID,
                           locationID = VPMonID,
                           subject = 'Vernal Pool',
                           startDate = depoyDay,
                           startTime = '0000-00-00') 
RSQLite::dbWriteTable(conn = conx, name = 'schedule', value = new.schedule,
                      row.names = FALSE, overwrite = FALSE,
                      append = TRUE, header = FALSE)
# file rename
# format 'yyyymmdd hhmmss.wav'
AudioFiles <- list.files(path = "recording_drop", pattern = ".WAV", all.files = TRUE,
                         full.names = TRUE, recursive = TRUE,
                         ignore.case = TRUE, include.dirs = TRUE)
AudioFiles2 <- parse_date_time(AudioFiles, "Ymd HMS", tz = 'UTC')
AudioFiles3 <- as.character(AudioFiles2)
AudioFiles4 <- str_replace_all(AudioFiles3,' ','_')
AudioFiles5 <- str_replace_all(AudioFiles4,':','-')
AudioFiles6 <- paste0('~',VPMonID,'_',AudioFiles5)
AudioFiles7 <- str_replace_all(AudioFiles6,'~','recording_drop/')
AudioFiles8 <- file.rename(AudioFiles,paste0(AudioFiles7,'.wav'))

# fill recordings table
dropboxMoveBatch(db.path = db.path,
                 table = 'recordings', 
                 dir.from = 'recording_drop', 
                 dir.to = 'recordings', 
                 token.path = 'settings/dropbox-token.RDS')
#create templates 
setwd('E:/Dropbox/AudioTemplates')
WOFRITemplate1 <- makeBinTemplate("WOFRITemplate1.WAV", 
                                  t.lim = c(241.59,241.69),
                                  frq.lim = c(0,6),
                                  score.cutoff = 0,
                                  amp.cutoff = -29,  
                                  name="WOFRITemplate1")
WOFRITemplate2 <- makeBinTemplate("WOFRITemplate2.wav",
                                  t.lim = c(2.275,2.325),
                                  frq.lim = c(0,4), 
                                  amp.cutoff = -27,
                                  buffer = .5,
                                  score.cutoff = 0,
                                  name="WOFRITemplate2")
WOFRITemplate3 <- makeBinTemplate("WOFRITemplate3.wav",
                                  t.lim = c(0.06,.16),
                                  frq.lim = c(0,6),
                                  amp.cutoff =-33,
                                  dens = .75,
                                  score.cutoff = 0,
                                  name="WOFRITemplate3")
SPPEITemplate  <- makeBinTemplate("SPPEITemplate.wav",
                                  t.lim = c(.55,.9),
                                  rq.lim = c(2,6),
                                  amp.cutoff = -37,
                                  score.cutoff = 0,
                                  buffer = 1,
                                  name = "SPPEITemplate")
EWPWTemplate <- makeBinTemplate("EWPWTemplate.wav",
                                 amp.cutoff = -20, 
                                 score.cutoff = 0,
                                 frq.lim = c(1,5),
                                 name = "EWPWTemplate")
BinTemplateList <- combineBinTemplates(WOFRITemplate1,WOFRITemplate2,WOFRITemplate3,SPPEITemplate,EWPWTemplate,EASOTemplate)
templatesInsert(db.path = db.path, 
                template.list = combineBinTemplates(WOFRITemplate1,WOFRITemplate2,WOFRITemplate3,SPPEITemplate,EWPWTemplate), 
                libraryID = c('wofr','wofr','wofr','sppe','ewpw'),
                personID = 'ktolan@vtecostudies.org')

### add EASO template

BADOTemplate <- makeCorTemplate("BADOTemplate.wav",
                                t.lim = c(2.65,3.35),
                                frq.lim = c(0.25,2.5),
                                score.cutoff = 0,
                                name="BADOTemplate")
CorTemplateList <- combineCorTemplates(BADOTemplate)
templatesInsert(db.path = db.path, 
                template.list = BADOTemplate, 
                libraryID = 'bado',
                personID = 'ktolan@vtecostudies.org')

#calculate scores
ranscores <- scoresDetect(db.path = db.path, 
                          directory = 'recordings', 
                          recordingID = 'all',
                          templateID = 'all',
                          score.thresholds = c(13,16,12,10,0.4),
                          #listID = 'Target Species Templates',     
                          token.path = 'settings/dropbox-token.RDS', 
                          db.insert = TRUE) 

write.csv(ranscores,'Scores.csv', append = FALSE)



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


# classification






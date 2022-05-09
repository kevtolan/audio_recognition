# view table characteristics 
qry(db.path = db.path, 
    table = 'locations')


# view table
RSQLite::dbReadTable(conn = conx,
                     name = 'recordings')

# add column
RSQLite::dbExecute(conn = conx, 
                   statement = "ALTER TABLE people 
                                ADD COLUMN startDate varchar;") 

# delete records
RSQLite::dbExecute(conn = conx, 
                   statement = "DELETE FROM recordings 
                                WHERE locationID = '' ")

# update records
RSQLite::dbExecute(conn = conx, 
                   statement = "UPDATE recordings 
                                SET speciesID = 'wofr'
                                WHERE commonName = 'Wood Frog' ")


# rename Olympus files. time <- '_20-00-00' SET TIME IN EST
library(lubridate)
library(stringr)

siteID <- 'SDF791'
time1 <- "19-00-00"

AudioFiles11 <- list.files(pattern = ".WAV", all.files = TRUE,
                             full.names = TRUE, recursive = TRUE,
                             ignore.case = TRUE, include.dirs = TRUE)
AudioFiles21 <- str_sub(AudioFiles11, end=-10)
AudioFiles31 <- parse_date_time(AudioFiles21, "ymd", tz = 'EST')
AudioFiles41 <- as.character(AudioFiles31)
#CHANGE TIME ITEM
AudioFiles51 <- paste0(AudioFiles41,'_',time1)
AudioFiles61 <- paste0(siteID,'_',AudioFiles51)
AudioFiles71 <- paste0(AudioFiles61,'.wav')
AudioFiles71 <- file.rename(AudioFiles11,AudioFiles71)

time2 <- '00-00-00'

AudioFiles11 <- list.files(pattern = ".WAV", all.files = TRUE,
                           full.names = TRUE, recursive = TRUE,
                           ignore.case = TRUE, include.dirs = TRUE)
AudioFiles21 <- str_sub(AudioFiles11, end=-10)
AudioFiles31 <- parse_date_time(AudioFiles21, "ymd", tz = 'EST')
AudioFiles41 <- AudioFiles31 + 86400
AudioFiles51 <- as.character(AudioFiles41)
AudioFiles61 <- paste0(AudioFiles51,'_',time2)
AudioFiles71 <- paste0(siteID,'_',AudioFiles61)
AudioFiles81 <- paste0(AudioFiles71,'.wav')
AudioFiles91 <- file.rename(AudioFiles11,AudioFiles81)




# rename Audiomoth files ; 'yyyymmdd hhmmss.wav'
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

# create dataframe of scores
library(janitor)
scores <- dbGetQuery(conn = conx, 
                     statement = "SELECT scoreID, recordingID, templateID, 
                                         time, scoreThreshold, score, 
                                         manualVerifyLibraryID, manualVerifySpeciesID
                                  FROM scores")

detXcsv <- tabyl(scores, recordingID, templateID)

# export csv of scores
write.csv(scores,paste0(siteID,'_detx_pivot.csv'))



####parsing export
export <- dbGetQuery(conn = conx, 
                    statement = "SELECT recordingID, templateID, COUNT(*)
                                  FROM scores
                                  GROUP BY recordingID ")

parsed1 <- str_replace(export$recordingID,paste0(siteID,"_"),'')
parsed2 <- parse_date_time(parsed1, "y-m-d_H-M-S", tz = 'UTC')
parsed3 <- as.POSIXlt(parsed2,'America/New_York')
export$datetime <- parsed3


write.csv(export,paste0(siteID,"_detx.csv"))
          
     


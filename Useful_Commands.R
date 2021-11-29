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
AudioFiles11 <- list.files(path = "recording_drop", pattern = ".WAV", all.files = TRUE,
                         full.names = TRUE, recursive = TRUE,
                         ignore.case = TRUE, include.dirs = TRUE)
AudioFiles21 <- str_sub(AudioFiles11, end=-10)
AudioFiles31 <- parse_date_time(AudioFiles21, "ymd", tz = 'EST')
AudioFiles41 <- as.character(AudioFiles31)
AudioFiles51 <- paste0(AudioFiles41,time)
AudioFiles61 <- paste0(siteID,AudioFiles51)
AudioFiles71 <- paste0(AudioFiles61,'.wav')

dropboxMoveBatch(db.path = db.path,
                 table = 'recordings', 
                 dir.from = 'recording_drop', 
                 dir.to = 'recordings', 
                 token.path = 'settings/dropbox-token.RDS')





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



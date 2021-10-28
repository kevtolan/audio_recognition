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

# export csv of scores
scores <- dbGetQuery(conn = conx, 
                     statement = "SELECT scoreID, recordingID, templateID, 
                                         time, scoreThreshold, score, 
                                         manualVerifyLibraryID, manualVerifySpeciesID
                                  FROM scores")
ggplot(scores) +
  geom_point(aes(x = time, y= recordingID))

write.csv(scores,paste0(siteID,'_2021_scores.csv'))

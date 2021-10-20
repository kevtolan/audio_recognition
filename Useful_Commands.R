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
                                SET speciesID = 'phai'
                                WHERE commonName = 'Phainopepla' ")

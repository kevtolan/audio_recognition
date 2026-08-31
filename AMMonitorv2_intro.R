

remotes::install_github("kevtolan/AMMonitor_Tolan_Version")



#remotes::install_gitlab(
#  repo = "vtcfwru/ammonitor@AMMonitor2.2",
#  auth_token = Sys.getenv("GITLAB_PAT"),
#  host = "code.usgs.gov",
#  build_vignettes = FALSE,
#  dependencies = TRUE,
#  upgrade = "never")
##### if it times out during download, use this and add "# R_MAX_VSIZE=100Gb" to the Renvir file
# library(usethis)
# edit_r_environ()
# >>>> R_MAX_VSIZE=100Gb




options(timeout=1000) #override typical timeout to allow for larger audio files to load
# required packages
library(AMMonitor)
library(tidyverse)
library(RSQLite)
library(DBI)
library(birdnetR)
#for other functions
# library(sf) # spatial analysis
# library(aws.s3) # to host audio use Amazon's web services
# library(padr) # pad dates of a timeseries
# library(plotly) # to make ggplots interactive




setwd('~') # The folder that you want the database's structure located in
ammCreateDirectories("xx", paste0(getwd())) # creates directory WITHIN current wd
setwd('~/xx') # set directory to the newly created directory. Set this as wd for all analysis in the future

dbCreate(
new_db_name = "yy.sqlite",
new_db_filepath = paste0(getwd(), "/database"),
db_source = "default")

db.path <- '~/xx/database/yy.sqlite'
conx <- RSQLite::dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
RSQLite::dbExecute(conn = conx, statement = "PRAGMA foreign_keys = ON;")

# launch app to  create templates,add sites, add/view media, 
# manually annotate data, and verify model detections
AMMonitor::launchApp()

#### THIS IS HOW AUDIO DETECTIONS ARE RUN. *NOT* THROUGH THE APP
## After adding sites/visits/media using launchApp(), use this to subset files for analysis
# in this format: subset_files(conx, 'siteID', "firstdate", "lastdate")
# or create other vector/df of filenames for analysis (files must be included in database)

subset_files <- function(conx, site, start_date, stop_date) {
  mediafiles <- RSQLite::dbReadTable(conn = conx, name = 'media')
  mediafiles$Site <- str_extract(mediafiles$filename, "[^_]+")
  mediafiles <- mediafiles[mediafiles$Site == site, ]
  mediafiles$date <- paste0(mediafiles$start_date, " ", mediafiles$start_time) %>%
    as.POSIXlt()
  DATESTART <- as.Date(start_date)
  DATESTOP <- as.Date(stop_date)
  mediasubset <- mediafiles %>% filter(between(date, DATESTART, DATESTOP)) }

mediasubset <- subset_files(conx, 'siteID', "yyyy-mm-dd", "yyyy-mm-dd")

# run detections
scores <- scoresDetect(
  con = conx,
  recordingNames = mediasubset$filename,
  templateNames = 'templateID', # make templates in launchApp(), but can be shared as .rds once made
  # scoreThresholds = 12, # not currently functioning, only uses threshold from template
  recordingRootPath = 'filedirectory',
  ammlPath = paste0(getwd(), "/ammls"),
  dbInsert = T,
  showProgress = T
)


source(system.file("birdnet/Register_BirdNET_Model.R", package = "AMMonitor"))   # adds the BirdNET_v2.4 row to models
source(system.file("birdnet/Register_BirdNET_Species.R", package = "AMMonitor")) # adds your species list to taxa
birdSpeciesList("~/R/AMMonitor_VPMon/birdnet_species_list.csv")

birdsDetect(con,
    recordingNames = "all",
    minConfidence = 0.1,
    #speciesList = NULL,
    speciesListPath = "~/R/AMMonitor_VPMon/birdnet_species_list.csv",
    modelVersion = "v2.4",
    language = "en_us",
    dbInsert = FALSE,
    showProgress = FALSE) 

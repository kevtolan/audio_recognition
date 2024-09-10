options(timeout=100)
remotes::install_gitlab(
  repo = "vtcfwru/ammonitor@AMMonitor2.0",
  auth_token = Sys.getenv("GITLAB_PAT"),
  host = "code.usgs.gov",
  build_vignettes = FALSE,
  dependencies = TRUE,
  upgrade = "never")

library(AMMonitor)
library(aws.s3)
library(tidyverse)
library(RSQLite)
library(DBI)
# library(usethis)
# edit_r_environ()

setwd('~/AMMv2')

my_filepath <- ammCreateDirectories(
  amm_dirname = "dbExample",
  filepath = getwd())

dbCreate(
  new_db_name = "dbExample.sqlite",
  new_db_filepath = paste0(my_filepath, "/database"),
  db_source = "default")

db.path <- '~/AMMv2/database/dbExample.sqlite'
conx <- RSQLite::dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
RSQLite::dbExecute(conn = conx, statement = "PRAGMA foreign_keys = ON;")

AMMonitor::launchApp()

mediafiles <- RSQLite::dbReadTable(conn = conx,
                            name = 'media')
mediafiles$Site  <- str_extract(mediafiles$filename, "[^_]+")
mediafiles <- mediafiles[mediafiles$Site == "site1",]

mediafiles$date <- paste0(mediafiles$start_date," ",mediafiles$start_time) %>%
                              as.POSIXlt()

DATESTART <- as.Date("2019-01-01")
DATESTOP <- as.Date("2024-01-01")


mediasubset <- mediafiles %>% filter(between(date, DATESTART, DATESTOP))

Sys.time()
start <- Sys.time()
scores <- scoresDetect(
  con = conx,
  recordingNames = mediafiles$filename,
  templateNames = "template_SDF791_20220418_150000_bin_AM1",
  scoreThresholds = NA,
  recordingRootPath = 'https://vpmon-audio.s3.amazonaws.com/',
  ammlPath = paste0(getwd(), "/ammls"),
  dbInsert = T,
  showProgress = T
)
stop <- Sys.time()
stop - start



### download AWS filelist
bucketlist <- get_bucket_df(
                    bucket = 'vpmon-audio',
                    max = Inf)

bucketlist <- bucketlist[- grep("Misc_Audio/", bucketlist$Key),]

# bucketlist$Site <- substr(bucketlist$Key,0,10)

bucketlist$Site  <- str_extract(bucketlist$Key, "[^_]+")
bucketlist$filename  <- bucketlist$Key
bucketlist$filepath <- paste0('https://vpmon-audio.s3.amazonaws.com/',bucketlist$filename)
table(bucketlist$Site)


bucketlist$Date <- str_sub(bucketlist$filename, start = -19, end = -5)

buckettmp <- parse_date_time(bucketlist$Date, "Y-m-d_H-M-S", tz = "America/New_York")

bucketlist$Date <- buckettmp
bucketlist$start_date <- format(buckettmp,"%Y-%m-%d")
bucketlist$start_time <- format(buckettmp, "%H:%M:%S")
bucketlist$year <- format(buckettmp, "%Y")
table(bucketlist$Site,bucketlist$year)

bucketlist$Size <- as.numeric(bucketlist$Size)
sum(bucketlist$Size)/1099511627776

#add files to db


bucketadd <- bucketlist[bucketlist$Site == "MLS619",]

bucketadd <- bucketadd[,c("filename","filepath",'start_date','start_time')]

bucketadd$pk_mediaid <- NA
bucketadd$fk_visitid <- 8
bucketadd$sb_exclude <- NA
bucketadd$fk_sciencebaseid <- NA
bucketadd$filesize <- NA
bucketadd$timestamp <- NA
bucketadd$media_type <- "audio"

dbAppendTable(conx, "media", bucketadd)








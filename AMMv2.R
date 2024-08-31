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







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

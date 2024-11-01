# remotes::install_gitlab(
#   repo = "vtcfwru/ammonitor@AMMonitor2.1",
#   auth_token = Sys.getenv("GITLAB_PAT"),
#   host = "code.usgs.gov",
#   build_vignettes = FALSE,
#   dependencies = TRUE,
#   upgrade =  'never')


options(timeout=1000)
library(AMMonitor)
library(aws.s3)
library(tidyverse)
library(RSQLite)
library(elevatr)
library(padr)
library(DBI)
# library(exactextractr)
library(sf)
# library(parallel)
library(plotly)
#source("/Users/kevintolan/R/myfunctions.R")
# library(usethis)
# edit_r_environ()
  
setwd('~/R/AMMonitor_VPMon/VPMon_AMM')
# AMMonitor::launchApp()
db.path <- '~/R/AMMonitor_VPMon/VPMon_AMM/database/VPMon_AMM.sqlite'
conx <- RSQLite::dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
RSQLite::dbExecute(conn = conx, statement = "PRAGMA foreign_keys = ON;")



mediafiles <- RSQLite::dbReadTable(conn = conx,
                                   name = 'media')
mediafiles$Site  <- str_extract(mediafiles$filename, "[^_]+")
mediafiles <- mediafiles[mediafiles$Site == "NEW174",]
# mediafiles <- mediafiles[mediafiles$Site %in% c("SDF1112",'MLS721','NEW94','NEW63'),]

# mediafiles$Site  <- str_extract(mediafiles$filename, "[^_]+")
# table(mediafiles$Site)
##Subset 

mediafiles$date <- paste0(mediafiles$start_date," ",mediafiles$start_time) %>%
  as.POSIXlt()
DATESTART <- as.Date("2023-03-15")
DATESTOP <- as.Date("2023-05-15")
mediasubset <- mediafiles %>% filter(between(date, DATESTART, DATESTOP))

#'template_SDF791_20210408_150000bin_thresh40'
#template_SDF791_20220418_150000_bin_AM1

Sys.time()
start <- Sys.time()
scores <- scoresDetect(
  con = conx,
  recordingNames = mediasubset$filename,
  templateNames = 'template_SDF791_20210408_150000bin_thresh40_cu12',
  scoreThresholds = 12,
  recordingRootPath = 'https://vpmon-audio.s3.amazonaws.com/',
  ammlPath = paste0(getwd(), "/ammls"),
  dbInsert = T,
  showProgress = T
)
stop <- Sys.time()
Sys.time()
elapse <- stop - start
elapse
nrow(mediasubset)/as.numeric(elapse)


site <- 'MON0516'

mediafiles <- RSQLite::dbReadTable(conn = conx,
                                   name = 'media')

detections <- RSQLite::dbGetQuery(conn = conx,
                                  statement = "SELECT * FROM media INNER JOIN modeloutputs ON media.pk_mediaid = modeloutputs.fk_mediaid
                                WHERE fk_taxonid = 'Wood Frog' ")

detections$fk_modeloutputid <- detections$pk_modeloutputid

detections2<- detections[,c('pk_mediaid','filename','fk_modelid','start_time','start_date','value_num','fk_modeloutputid')]
# names(detections)[names(detections) == 'pk_modeloutputid'] <- 'fk_modeloutputid'


baddetx <- RSQLite::dbGetQuery(conn = conx,
                               statement = "SELECT * FROM modelverifications 
                                WHERE is_valid = 0; ")

baddetx <- as.data.frame(baddetx)
gooddetx <- anti_join(detections2,baddetx, by = join_by(fk_modeloutputid))

rm(baddetx)
rm(detections)
rm(detections2)

gooddetx <- gooddetx[gooddetx$fk_modelid == 4 | gooddetx$fk_modelid == 5,]


gooddetx <- gooddetx[gooddetx$value_num >= 14,]
gooddetx$Site  <- str_extract(gooddetx$filename, "[^_]+") %>% as.factor()
gooddetx$start_date <- as.Date(gooddetx$start_date)


detx <- gooddetx[,c('start_date','start_time','Site')]

rm(gooddetx)


detx <- detx %>% 
  group_by(Site, start_date) %>% 
  summarize(NumDetx = n())

detx$Year <-  format(detx$start_date,"%Y")
detx$Day <-  format(detx$start_date,"%m/%d")

detx$DateID <-  paste0(detx$Site,'_',detx$start_date)


# manually overwrite detections
detx$NumDetx[detx$DateID == 'MLS737_2019-04-09'] <- NA
detx$NumDetx[detx$DateID == 'MLS737_2019-04-13'] <- NA
detx$NumDetx[detx$DateID == 'MLS737_2019-04-20'] <- NA
detx$NumDetx[detx$DateID == 'MLS737_2019-04-22'] <- NA
detx$NumDetx[detx$DateID == 'MLS737_2019-04-23'] <- NA
detx$NumDetx[detx$DateID == 'MLS737_2019-04-24'] <- NA
detx$NumDetx[detx$DateID == 'MLS737_2019-04-26'] <- NA
detx$NumDetx[detx$DateID == 'MLS737_2019-05-01'] <- NA
detx$NumDetx[detx$DateID == 'MLS737_2019-05-02'] <- NA
detx$NumDetx[detx$DateID == 'MLS737_2019-05-03'] <- NA
detx$NumDetx[detx$DateID == 'MLS737_2019-05-07'] <- NA
detx$NumDetx[detx$DateID == 'MLS737_2021-04-08'] <- NA
detx$NumDetx[detx$DateID == 'MLS737_2021-04-23'] <- NA
detx$NumDetx[detx$DateID == 'MLS737_2021-05-13'] <- NA
detx$NumDetx[detx$DateID == 'MLS737_2021-05-18'] <- NA
detx$NumDetx[detx$DateID == 'MLS737_2022-04-07'] <- NA
detx$NumDetx[detx$DateID == 'MLS737_2023_03_25'] <- NA

# "MLS737_2023-04-12"
# "MLS737_2023-04-09"
detx$NumDetx[detx$DateID == 'MLS619_2019-03-20'] <- NA
detx$NumDetx[detx$DateID == 'MLS619_2019-03-31'] <- NA
detx$NumDetx[detx$DateID == 'MLS619_2019-04-03'] <- NA
detx$NumDetx[detx$DateID == 'MLS619_2019-04-22'] <- NA
detx$NumDetx[detx$DateID == 'MLS619_2019-04-23'] <- NA
detx$NumDetx[detx$DateID == 'MLS619_2019-04-24'] <- NA
detx$NumDetx[detx$DateID == 'MLS619_2019-04-25'] <- NA
detx$NumDetx[detx$DateID == 'MLS619_2019-04-26'] <- NA
detx$NumDetx[detx$DateID == 'MLS619_2023-04-06'] <- 12000
detx$NumDetx[detx$DateID == 'MLS619_2023-04-12'] <- 600
detx$NumDetx[detx$DateID == 'MLS619_2023-04-13'] <- 12000
detx$NumDetx[detx$DateID == 'MLS619_2024-03-09'] <- NA
detx$NumDetx[detx$DateID == 'MLS619_2024-03-20'] <- NA
detx$NumDetx[detx$DateID == 'MLS619_2024-03-29'] <- 600
detx$NumDetx[detx$DateID == 'MLS619_2024-04-17'] <- NA

detx$NumDetx[detx$DateID == 'CALT019_2019-05-19'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2019-05-20'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2019-05-21'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2019-05-22'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2019-05-23'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2019-05-24'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2019-05-25'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2019-05-28'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2020-03-30'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2020-03-31'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2020-04-01'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2020-04-02'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2020-04-03'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2020-04-06'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2020-04-09'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2020-04-11'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2020-04-12'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2020-04-15'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2020-04-17'] <- NA
detx$NumDetx[detx$DateID == 'CALT019_2020-05-10'] <- NA

detx$NumDetx[detx$DateID == 'WEA019_2020_04-10'] <- NA
detx$NumDetx[detx$DateID == 'WEA019_2020_04-14'] <- NA
detx$NumDetx[detx$DateID == 'WEA019_2020_04-20'] <- NA
detx$NumDetx[detx$DateID == 'WEA019_2020_04-26'] <- NA
detx$NumDetx[detx$DateID == 'WEA019_2020_04-27'] <- NA
detx$NumDetx[detx$DateID == 'WEA019_2020_04-28'] <- NA
detx$NumDetx[detx$DateID == 'WEA019_2021-04-04'] <- NA #airplane
detx$NumDetx[detx$DateID == 'WEA019_2023-05-06'] <- NA
detx$NumDetx[detx$DateID == 'WEA019_2023-05-16'] <- NA
detx$NumDetx[detx$DateID == 'WEA019_2024-03-27'] <- NA
detx$NumDetx[detx$DateID == 'WEA019_2024-03-20'] <- NA

detx$NumDetx[detx$DateID == 'NEW88_2024-03-20'] <- NA
detx$NumDetx[detx$DateID == 'NEW88_2024-05-17'] <- NA

detx$NumDetx[detx$DateID == 'MOET019_2019-04-11'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2019-04-13'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2019-04-14'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2019-04-15'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2019-04-16'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2019-04-17'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2019-04-18'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2019-04-19'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2019-04-24'] <- 1
detx$NumDetx[detx$DateID == 'MOET019_2019-04-27'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2019-04-28'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2019-04-30'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2019-05-02'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2019-05-03'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2019-05-07'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2019-05-09'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2019-05-10'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2020-03-29'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2020-03-30'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2020-03-31'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2020-04-01'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2020-04-02'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2020-04-03'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2020-04-04'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2020-04-05'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2020-04-06'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2020-04-07'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2020-04-08'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2020-04-09'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2020-04-10'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2020-04-11'] <- NA 
detx$NumDetx[detx$DateID == 'MOET019_2020-04-12'] <- 2
detx$NumDetx[detx$DateID == 'MOET019_2020-04-28'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2020-04-30'] <- 2
detx$NumDetx[detx$DateID == 'MOET019_2020-05-02'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2020-05-03'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2022-04-11'] <- NA
detx$NumDetx[detx$DateID == 'MOET019_2023-05-07'] <- NA

detx$NumDetx[detx$DateID == 'SDF1734_2019-04-03'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-04-04'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-04-15'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-04-16'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-04-18'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-04-29'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-05-01'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2024-03-10'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2024-03-21'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2024-04-29'] <- NA

detx$NumDetx[detx$DateID == 'SDF736_2020-03-20'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2020-04-22'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2020-04-23'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2020-04-28'] <- NA

detx$NumDetx[detx$DateID == 'SDF736_2023-03-25'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-03-26'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-03-27'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-03-28'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-03-29'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-03-30'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-03-31'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-04-01'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-04-02'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-04-03'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-04-04'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-04-05'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-04-06'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-04-07'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-04-08'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-04-09'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-04-10'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-04-11'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-04-12'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-04-13'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-04-14'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-04-15'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-05-02'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-05-03'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-05-04'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-05-05'] <- NA
detx$NumDetx[detx$DateID == 'SDF736_2023-05-06'] <- NA

detx$NumDetx[detx$DateID == 'SDF1264_2019-04-17'] <- NA
detx$NumDetx[detx$DateID == 'SDF1264_2019-04-18'] <- NA

detx$NumDetx[detx$DateID == 'NEW1387_2024-03-21'] <- NA
detx$NumDetx[detx$DateID == 'NEW1387_2024-05-18'] <- NA

detx$NumDetx[detx$DateID == 'NEW51_2020-03-20'] <- 6000
detx$NumDetx[detx$DateID == 'NEW51_2020-03-29'] <- 6000
detx$NumDetx[detx$DateID == 'NEW51_2020-03-29'] <- 6000
detx$NumDetx[detx$DateID == 'NEW51_2024-04-11'] <- 6000
detx$NumDetx[detx$DateID == 'NEW51_2024-04-12'] <- 12000
detx$NumDetx[detx$DateID == 'NEW51_2024-04-16'] <- NA
detx$NumDetx[detx$DateID == 'NEW51_2024-04-22'] <- NA
detx$NumDetx[detx$DateID == 'NEW51_2024-04-29'] <- NA
detx$NumDetx[detx$DateID == 'NEW51_2024-04-30'] <- NA

detx$NumDetx[detx$DateID == 'NEW12_2019-04-04'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2019-04-07'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2019-04-12'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2019-04-13'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2019-04-14'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2019-04-15'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2019-04-15'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2019-04-18'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2019-04-19'] <- 50
detx$NumDetx[detx$DateID == 'NEW12_2020-03-20'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-03-29'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-03-30'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-04-02'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-04-03'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-04-06'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-04-07'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-04-09'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-04-10'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-04-11'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-04-12'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-04-17'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-04-21'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-04-22'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-04-26'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-04-27'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-04-29'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-05-01'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-05-02'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-05-03'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-05-04'] <- NA
detx$NumDetx[detx$DateID == 'NEW12_2020-05-05'] <- NA

detx$NumDetx[detx$DateID == 'MLS411_2019-03-29'] <- NA
detx$NumDetx[detx$DateID == 'MLS411_2019-03-30'] <- NA
detx$NumDetx[detx$DateID == 'MLS411_2019-04-02'] <- NA
detx$NumDetx[detx$DateID == 'MLS411_2019-04-03'] <- NA
detx$NumDetx[detx$DateID == 'MLS411_2021-03-26'] <- NA
detx$NumDetx[detx$DateID == 'MLS411_2020-03-29'] <- NA
detx$NumDetx[detx$DateID == 'MLS411_2023-04-25'] <- 100

detx$NumDetx[detx$DateID == 'SDF791_2019-04-18'] <- 10
detx$NumDetx[detx$DateID == 'SDF791_2019-05-06'] <- 100
detx$NumDetx[detx$DateID == 'SDF791_2023-05-08'] <- 250

detx$NumDetx[detx$DateID == 'STA019_2019-05-04'] <- NA
detx$NumDetx[detx$DateID == 'STA019_2019-05-10'] <- NA


detx$NumDetx[detx$DateID == 'CON019_2019-04-03'] <- NA
detx$NumDetx[detx$DateID == 'CON019_2020-04-03'] <- NA #rain
detx$NumDetx[detx$DateID == 'CON019_2021-03-29'] <- NA 
detx$NumDetx[detx$DateID == 'CON019_2021-03-31'] <- 3000 
detx$NumDetx[detx$DateID == 'CON019_2021-04-06'] <- 3000 
detx$NumDetx[detx$DateID == 'CON019_2022-04-05'] <- NA 
detx$NumDetx[detx$DateID == 'CON019_2022-04-27'] <- NA
detx$NumDetx[detx$DateID == 'CON019_2023-04-11'] <- NA
detx$NumDetx[detx$DateID == 'CON019_2023-04-13'] <- NA

detx$NumDetx[detx$DateID == 'NEW1306_2023-04-14'] <- 6000
detx$NumDetx[detx$DateID == 'NEW1306_2024-04-12'] <- 12000
detx$NumDetx[detx$DateID == 'NEW1306_2024-04-13'] <- 2000

detx$NumDetx[detx$DateID == 'IRR019_2019-03-31'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2019-04-03'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2019-04-09'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2019-04-26'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2019-04-27'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2019-05-10'] <- NA 
detx$NumDetx[detx$DateID == 'IRR019_2020-03-17'] <- NA ## 2020: likely near nest/roost
detx$NumDetx[detx$DateID == 'IRR019_2020-03-20'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2020-03-21'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2020-03-24'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2020-03-25'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2020-03-28'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2020-03-29'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2020-04-02'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2020-04-04'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2020-04-09'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2020-04-16'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2020-04-19'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2020-04-21'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2020-04-22'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2020-04-26'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2020-04-27'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2020-04-29'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2020-04-30'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2021-04-21'] <- NA

detx$NumDetx[detx$DateID == 'IRR019_2023-04-02'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2023-04-22'] <- NA
detx$NumDetx[detx$DateID == 'IRR019_2023-04-25'] <- NA

detx$NumDetx[detx$DateID == 'SDF1112_2019-04-16'] <- NA
detx$NumDetx[detx$DateID == 'SDF1112_2019-04-18'] <- NA
detx$NumDetx[detx$DateID == 'SDF1112_2019-04-28'] <- 3000
detx$NumDetx[detx$DateID == 'SDF1112_2019-05-03'] <- NA
detx$NumDetx[detx$DateID == 'SDF1112_2019-05-09'] <- NA
detx$NumDetx[detx$DateID == 'SDF1112_2019-05-10'] <- NA

detx$NumDetx[detx$DateID == 'NEW63_2019-04-04'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-04-05'] <- NA

detx$NumDetx[detx$DateID == 'NEW94_2019-04-14'] <- NA
detx$NumDetx[detx$DateID == 'NEW94_2019-05-11'] <- NA

detx$NumDetx[detx$DateID == 'KWN581_2021-04-07'] <- 500
detx$NumDetx[detx$DateID == 'KWN581_2021-04-20'] <- NA
detx$NumDetx[detx$DateID == 'KWN581_2022-03-24'] <- NA

detx$NumDetx[detx$DateID == 'KWN827_2020-03-17'] <- NA
detx$NumDetx[detx$DateID == 'KWN827_2020-03-19'] <- NA
detx$NumDetx[detx$DateID == 'KWN827_2020-03-20'] <- NA
detx$NumDetx[detx$DateID == 'KWN827_2020-03-21'] <- NA
detx$NumDetx[detx$DateID == 'KWN827_2020-03-23'] <- NA
detx$NumDetx[detx$DateID == 'KWN827_2020-03-26'] <- NA
detx$NumDetx[detx$DateID == 'KWN827_2020-03-27'] <- NA
detx$NumDetx[detx$DateID == 'KWN827_2020-03-29'] <- NA
detx$NumDetx[detx$DateID == 'KWN827_2020-03-30'] <- NA
detx$NumDetx[detx$DateID == 'KWN827_2020-03-31'] <- NA

detx$NumDetx[detx$DateID == 'MIR019_2019-03-28'] <- NA
detx$NumDetx[detx$DateID == 'MIR019_2019-03-29'] <- NA
detx$NumDetx[detx$DateID == 'MIR019_2019-03-31'] <- 1
detx$NumDetx[detx$DateID == 'MIR019_2019-04-05'] <- 1

detx$NumDetx[detx$DateID == 'MLS165_2021-03-30'] <- NA
detx$NumDetx[detx$DateID == 'MLS165_2021-04-02'] <- NA
detx$NumDetx[detx$DateID == 'MLS165_2021-04-19'] <- NA
detx$NumDetx[detx$DateID == 'MLS165_2021-04-20'] <- NA

detx$NumDetx[detx$DateID == 'MLS318_2020-03-19'] <- NA



detx <- detx %>% drop_na(NumDetx)
detx$`Relative Call Intensity` <- detx$NumDetx

table(detx$Site, detx$Year)

# 
# sitedetx <- detx[detx$Site %in% c("WEA019", 'CALT019', 'NEW88',
#                                   'MLS737','MLS619','MOET019',
#                                   'SDF1734','SDF1264'),]

sitedetx <- detx[detx$Site == site,]
# sitedetx <- detx[detx$Site == paste0(siteID),]
# view(sitedetx)
# 
# ggplot() + 
#   geom_line(data = sitedetx, aes(x = Day, y = NumDetx, group = Site, color = Year)) 


every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}


p <- ggplot() +
  geom_jitter(data = sitedetx, aes(x = Day, y = Year, color = Year, size = `Relative Call Intensity`),  height = 0.03, fill = 'black', alpha = 0.7) +
 scale_x_discrete(breaks = every_nth(n = 3))
p
# geom_jitter(data = sitedetx, aes(x = Day, y = Site, color = Year, size = NumDetx),  height = 0.11, alpha = 0.7)
ggplotly(p)


sitedetxpad <- pad(sitedetx, group = "Year")


sitedetxpad$DateID <- paste0(site,"_",sitedetxpad$start_date)
sitedetxpad$Year <-  format(sitedetxpad$start_date,"%Y")
sitedetxpad$Day <-  format(sitedetxpad$start_date,"%m/%d")
sitedetxpad$Site <- site

sitedetxpad[is.na(sitedetxpad)] <- 0

# 
# sitedetxpad[is.na(sitedetxpad)] <- 0
p <- ggplot() + 
  geom_line(data = sitedetxpad, aes(x = Day, y = `Relative Call Intensity`, group = Year, color = Year), size=2) +
  # geom_smooth(data = sitedetx, aes(x = Day, y = `Relative Call Intensity`, group = Year, color = Year), method = 'loess',  level = 0.05, size=2) +
  geom_point(data = sitedetxpad, aes(x = Day, y = `Relative Call Intensity`, group = Year, color = Year)) +
  scale_x_discrete(breaks = every_nth(n = 3)) +
  scale_color_manual(values = c("#7F58AF","#64C5EB","#E84D8A","#FEB326","blue")) +
  theme(axis.title=element_text(size=20)) +
  # theme(legend.position = c(.15, .65)) + 
  theme(legend.title = element_text(face="bold", size=20)) +
  theme(legend.text = element_text(size=15)) +
  # theme(legend.background = element_rect(size=1.5, colour ="black")) +
  # theme(legend.key.height= unit(3, 'cm'), legend.key.width= unit(4, 'cm')) +
  # theme(text = element_text(size=16)) +
  geom_hline(data = data.frame(type="A", y=0), mapping=aes(yintercept=y), size = 1) +
  # geom_vline(xintercept = c(2,16,30,44),  linetype="dashed", size = 1) + 
  ylim(0, NA) +
  facet_wrap( ~Year, scales="free_y", ncol = 1) +
  labs(title = paste(sitedetx$Site,"Wood Frog Detections"))
p
# geom_jitter(data = sitedetx, aes(x = Day, y = Site, color = Year, size = NumDetx),  height = 0.11, alpha = 0.7)
# ggplotly(p)



# AMMonitor::launchApp()



# delete

# files_to_delete <- c("SDF1734_20190511_210000.wav")
# 
# Sys.time()
# start <- Sys.time()
# files_string <- paste0("'", paste(files_to_delete, collapse = "', '"), "'")
# query <- paste("DELETE FROM media WHERE filename IN (", files_string, ");")
# dbExecute(conx, query)
# stop <- Sys.time()
# Sys.time()
# stop - start
#NOT DELETED YET


# Add file list to media
# update <- read.csv('/Users/kevintolan/R/AMMonitor_VPMon/visits.csv')
# dbAppendTable(conx, "visits", update)

templates <- readRDS('/Users/kevintolan/R/AMMonitor_VPMon/VPMon_AMM/ammls/templateLibrary.RDS')


##### graph

locallist <- DBI::dbReadTable(conx, name = 'locations')
locallist <- locallist %>% drop_na(lat)

locals.sp <- st_as_sf(locallist, coords=c('long','lat'), crs=4326)
# locals.sp <- get_elev_point(locals.sp)
  
bioph <- st_read('~/R/AMMonitor_VPMon/VPMon_AMM/spatials/Biophysical_Regions.shp')
climzone <- st_read('~/R/AMMonitor_VPMon/VPMon_AMM/spatials/Climate_Zones.shp')
bioph <- st_transform(bioph,4326)
climzone <- st_transform(climzone,4326)

sf_use_s2(FALSE)
locals.bioph <- st_intersection(locals.sp,bioph)
locals.climzone <- st_intersection(locals.sp,climzone)

# sitedetxfull <- detx[detx$Site %in% c("WEA019", 'CALT019', 'NEW88','MLS737','MLS619','MOET019','SDF1734'),]
sitedetxfull <- detx

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

locals.bioph.joined <- full_join(sitedetxfull, locals.bioph, by=c("Site"="pk_locationid"))
locals.bioph.joined <- locals.bioph.joined %>% drop_na(NumDetx)

locals.climzone.joined <- full_join(sitedetxfull, locals.climzone, by=c("Site"="pk_locationid"))
locals.climzone.joined <- locals.climzone.joined %>% drop_na(NumDetx)

pbioph <- ggplot() + 
  # geom_jitter(data = sitedetx, aes(x = Day, y = Year, color = Year, size = NumDetx),  height = 0.03, fill = 'black', alpha = 0.7)
  geom_jitter(data = locals.bioph.joined, aes(x = Day, y = Site, fill = Year, size = NumDetx),  height = 0.15, alpha = 0.6) +
  facet_wrap( ~NAME, scales="free_y", ncol = 1) +
  scale_x_discrete(breaks = every_nth(n = 10)) 
ggplotly(pbioph)

pzone <- ggplot() + 
  # geom_jitter(data = sitedetx, aes(x = Day, y = Year, color = Year, size = NumDetx),  height = 0.03, fill = 'black', alpha = 0.7)
  geom_jitter(data = locals.climzone.joined, aes(x = Day, y = Site, fill = Year, size = NumDetx),  height = 0.05, alpha = 0.5) +
  facet_wrap( ~ZONE, scales="free_y", ncol = 1) +
  scale_x_discrete(breaks = every_nth(n = 10)) 
ggplotly(pzone)

pbioph <- ggplot() + 
  # geom_jitter(data = sitedetx, aes(x = Day, y = Year, color = Year, size = NumDetx),  height = 0.03, fill = 'black', alpha = 0.7)
  geom_jitter(data = locals.bioph.joined, aes(x = Day, y = Site, fill = Year, size = NumDetx),  height = 0.11, alpha = 0.7) +
  facet_wrap( ~NAME, scales="free_y", ncol = 1) +
  scale_x_discrete(breaks = every_nth(n = 10)) 
ggplotly(pbioph)


# pelev <- ggplot() + 
#   # geom_jitter(data = sitedetx, aes(x = Day, y = Year, color = Year, size = NumDetx),  height = 0.03, fill = 'black', alpha = 0.7)
#   geom_jitter(data = locals.climzone.joined, aes(x = Day, y = elevation, color = Year, size = NumDetx),  height = 0.11, alpha = 0.7) +
#   facet_wrap( ~Site, scales="free", ncol = 1)
# ggplotly(pelev)





### files to delete
Sys.time()
start <- Sys.time()
RSQLite::dbExecute(conn = conx,
                   statement = "DELETE FROM media
                                WHERE filename = 'KWN581_20230306_1400000.wav' ")
stop <- Sys.time()
Sys.time()
stop - start
#NOT DELETED YET



delete_these_files <- c('STA019_20220314_220000.wav',
                     'STA019_20220314_220000.wav',
                     'STA019_20220315_150000.wav',
                     'STA019_20220315_190000.wav',
                     'STA019_20220315_210000.wav',
                     'STA019_20220315_220000.wav',
                     'STA019_20220316_150000.wav',
                     'STA019_20220316_190000.wav',
                     'STA019_20220316_210000.wav',
                     'STA019_20220316_220000.wav',
                     'STA019_20220317_150000.wav',
                     'STA019_20220317_190000.wav',
                     'STA019_20220317_210000.wav',
                     'STA019_20220317_220000.wav',
                     'STA019_20220318_150000.wav',
                     'STA019_20220318_190000.wav',
                     'STA019_20220318_210000.wav',
                     'STA019_20220318_220000.wav',
                     'STA019_20220319_150000.wav',
                     'STA019_20220319_190000.wav',
                     'STA019_20220319_210000.wav',
                     'STA019_20220319_220000.wav',
                     'STA019_20220320_150000.wav',
                     'STA019_20220320_190000.wav',
                     'STA019_20220320_210000.wav',
                     'STA019_20220320_220000.wav',
                     'STA019_20220321_150000.wav',
                     'STA019_20220321_190000.wav',
                     'STA019_20220321_210000.wav',
                     'STA019_20220321_220000.wav',
                     'STA019_20220322_150000.wav',
                     'STA019_20220322_190000.wav',
                     'STA019_20220322_210000.wav',
                     'STA019_20220322_220000.wav',
                     'STA019_20220323_150000.wav',
                     'STA019_20220323_190000.wav',
                     'STA019_20220323_210000.wav',
                     'STA019_20220323_220000.wav')
for (filename in delete_these_files) {
  query <- paste0("DELETE FROM media WHERE filename = '", filename, "';")
    dbExecute(conx, query)
    cat(Sys.time(),"Deleted file:", filename, "\n")
}
cat("All files have been processed and removed from the database.\n")



# chart media
med <- RSQLite::dbReadTable(conn = conx,
                            name = 'Media')
medtmp <- as.POSIXlt(med$start_date)
med$Day <- format(as.Date(medtmp,'%Y-%m-%d %H:%M:%S'),"%m-%d")
med$Year <- format(as.Date(medtmp,'%Y-%m-%d %H:%M:%S'),"%Y")
med$Ordinal <- medtmp$yday
table(med$Year)
table(med$Day)
plot(med$Ordinal, med$Year)



## download AWS filelist
bucketlist <- get_bucket_df(
                    bucket = 'vpmon-audio',
                    max = Inf)

bucketlist <- bucketlist[- grep("Misc_Audio/", bucketlist$Key),]
bucketlist$Site  <- str_extract(bucketlist$Key, "[^_]+")
bucketlist$filename  <- bucketlist$Key
bucketlist$filepath <- paste0('https://vpmon-audio.s3.amazonaws.com/',bucketlist$filename)

bucketlist$Date <- str_sub(bucketlist$filename, start = -19, end = -5)

buckettmp <- parse_date_time(bucketlist$Date, "Y-m-d_H-M-S", tz = "America/New_York")

bucketlist$Date <- buckettmp
bucketlist$start_date <- format(buckettmp,"%Y-%m-%d")
bucketlist$start_time <- format(buckettmp, "%H:%M:%S")
bucketlist$year <- format(buckettmp, "%Y")
table(bucketlist$Site,bucketlist$year)

bucketlist$Size <- as.numeric(bucketlist$Size)
sum(bucketlist$Size)/1099511627776






visitlist <- DBI::dbReadTable(conx, name = 'visits')


visitlist2 <- visitlist[,c("pk_visitid","fk_locationid")]


visitlist <- visitlist$fk_locationid
# sitelist <- unique(bucketlist$Site)
# diff <- setdiff(sitelist,visitlist)
# diff






#### Commands
# delete
RSQLite::dbExecute(conn = conx,
                   statement = "DELETE FROM media
                                WHERE filename = 'MLS737_20210520_210000.wav' ")

RSQLite::dbExecute(conn = conx,
                   statement = "DELETE FROM media
                                WHERE fk_visitid = 79  ")
RSQLite::dbExecute(conn = conx,
                   statement = "DELETE FROM modeloutputs
                                WHERE fk_mediaid = '16462' ")
#add files to db


bucketadd <- bucketlist[bucketlist$Site == "CON019",]

bucketadd <- bucketadd[,c("filename","filepath",'start_date','start_time')]

bucketadd$pk_mediaid <- NA
bucketadd$fk_visitid <- 61
bucketadd$sb_exclude <- NA
bucketadd$fk_sciencebaseid <- NA
bucketadd$filesize <- NA
bucketadd$timestamp <- NA
bucketadd$media_type <- "audio"

medialist <- DBI::dbReadTable(conx, name = 'media')
bucketlist$Site  <- str_extract(bucketlist$Key, "[^_]+")

unaddedfiles <- anti_join(bucketadd,medialist,by="filename")


dbAppendTable(conx, "media", bucketadd)






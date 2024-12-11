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
library(ggrain)
library(tidyverse)
library(RSQLite)
library(elevatr)
library(padr)
library(DBI)
# library(exactextractr)
library(sf)
# library(parallel)
library(plotly)
source("/Users/kevintolan/R/myfunctions.R")
# library(usethis)
# edit_r_environ()

setwd('~/R/AMMonitor_VPMon/VPMon_AMM')
# AMMonitor::launchApp()
db.path <- '~/R/AMMonitor_VPMon/VPMon_AMM/database/VPMon_AMM.sqlite'
conx <- RSQLite::dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
RSQLite::dbExecute(conn = conx, statement = "PRAGMA foreign_keys = ON;")

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}


site <- 'NEW319'

mediafiles <- RSQLite::dbReadTable(conn = conx,
                                   name = 'media')
  
detections <- RSQLite::dbGetQuery(conn = conx,
                                  statement = "SELECT * FROM media INNER JOIN modeloutputs ON media.pk_mediaid = modeloutputs.fk_mediaid
                                WHERE fk_taxonid = 'Wood Frog' ")

detections$fk_modeloutputid <- detections$pk_modeloutputid

detections2 <- detections[,c('pk_mediaid','filename','fk_modelid','start_time','start_date','value_num','fk_modeloutputid')]
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


############# 
#manually overwrite detections
detx$NumDetx[detx$DateID == 'NEW319_2020-03-29'] <- NA 
detx$NumDetx[detx$DateID == 'NEW319_2020-04-01'] <- NA 
detx$NumDetx[detx$DateID == 'NEW319_2020-04-02'] <- NA 
detx$NumDetx[detx$DateID == 'NEW319_2020-04-03'] <- NA 
detx$NumDetx[detx$DateID == 'NEW319_2020-04-05'] <- NA 
detx$NumDetx[detx$DateID == 'NEW319_2020-04-06'] <- 1 
detx$NumDetx[detx$DateID == 'NEW319_2020-04-09'] <- NA 
detx$NumDetx[detx$DateID == 'NEW319_2020-04-10'] <- NA 
detx$NumDetx[detx$DateID == 'NEW319_2020-04-11'] <- NA 
detx$NumDetx[detx$DateID == 'NEW319_2020-04-12'] <- NA 
detx$NumDetx[detx$DateID == 'NEW319_2020-04-16'] <- NA 
detx$NumDetx[detx$DateID == 'NEW319_2020-04-17'] <- NA 
detx$NumDetx[detx$DateID == 'NEW319_2020-04-18'] <- 1 
detx$NumDetx[detx$DateID == 'NEW319_2020-04-21'] <- NA 
detx$NumDetx[detx$DateID == 'NEW319_2020-04-22'] <- NA 
detx$NumDetx[detx$DateID == 'NEW319_2020-04-23'] <- NA 
detx$NumDetx[detx$DateID == 'NEW319_2020-04-27'] <- NA 



detx$NumDetx[detx$DateID == 'KWN238_2024-04-24'] <- NA 

detx$NumDetx[detx$DateID == 'KWN473_2020-03-16'] <- NA 
detx$NumDetx[detx$DateID == 'KWN473_2020-03-17'] <- NA 
detx$NumDetx[detx$DateID == 'KWN473_2020-03-20'] <- NA 
detx$NumDetx[detx$DateID == 'KWN473_2020-03-21'] <- NA 
detx$NumDetx[detx$DateID == 'KWN473_2020-03-23'] <- NA 
detx$NumDetx[detx$DateID == 'KWN473_2020-03-27'] <- NA 
detx$NumDetx[detx$DateID == 'KWN473_2020-04-02'] <- NA 
detx$NumDetx[detx$DateID == 'KWN473_2020-04-05'] <- NA 
detx$NumDetx[detx$DateID == 'KWN473_2020-04-06'] <- NA 
detx$NumDetx[detx$DateID == 'KWN473_2020-04-10'] <- NA 
detx$NumDetx[detx$DateID == 'KWN473_2020-04-11'] <- NA 
detx$NumDetx[detx$DateID == 'KWN473_2020-04-12'] <- 100 
detx$NumDetx[detx$DateID == 'KWN473_2020-04-15'] <- NA 
detx$NumDetx[detx$DateID == 'KWN473_2020-04-16'] <- NA 
detx$NumDetx[detx$DateID == 'KWN473_2020-04-17'] <- NA 


detx$NumDetx[detx$DateID == 'NEW30_2021-03-28'] <- 100 
### double check detx$NumDetx[detx$DateID == 'NEW30_2023-03-18'] <- 100
detx$NumDetx[detx$DateID == 'NEW30_2023-03-29'] <- NA 

detx$NumDetx[detx$DateID == 'NEW429_2022-03-26'] <- NA #ducks
detx$NumDetx[detx$DateID == 'NEW429_2023-04-03'] <- 3000 
detx$NumDetx[detx$DateID == 'NEW429_2023-04-04'] <- 3000 
detx$NumDetx[detx$DateID == 'NEW429_2023-04-06'] <- 500 
detx$NumDetx[detx$DateID == 'NEW429_2024-03-29'] <- 2000 
detx$NumDetx[detx$DateID == 'NEW429_2024-03-31'] <- 500 
detx$NumDetx[detx$DateID == 'NEW429_2024-04-01'] <- 1000 


detx$NumDetx[detx$DateID == 'NEW447_2020-04-17'] <- 250
detx$NumDetx[detx$DateID == 'NEW447_2020-04-18'] <- 250
detx$NumDetx[detx$DateID == 'NEW447_2020-04-23'] <- 1000
detx$NumDetx[detx$DateID == 'NEW447_2020-04-27'] <- 1000
detx$NumDetx[detx$DateID == 'NEW447_2020-04-28'] <- 3000

detx$NumDetx[detx$DateID == 'NEW383_2020-03-20'] <- NA
detx$NumDetx[detx$DateID == 'NEW383_2020-03-29'] <- NA
detx$NumDetx[detx$DateID == 'NEW383_2020-04-13'] <- NA 
detx$NumDetx[detx$DateID == 'NEW383_2020-04-17'] <- NA 
detx$NumDetx[detx$DateID == 'NEW383_2020-04-21'] <- NA 
detx$NumDetx[detx$DateID == 'NEW383_2021-04-02'] <- NA 
detx$NumDetx[detx$DateID == 'NEW383_2021-04-07'] <- 500 
detx$NumDetx[detx$DateID == 'NEW383_2021-04-28'] <- 50 
detx$NumDetx[detx$DateID == 'NEW383_2023-04-02'] <- NA 
detx$NumDetx[detx$DateID == 'NEW383_2023-04-03'] <- NA 
detx$NumDetx[detx$DateID == 'NEW383_2023-04-07'] <- NA 
detx$NumDetx[detx$DateID == 'NEW383_2023-04-08'] <- NA 


detx$NumDetx[detx$DateID == 'MLS1143_2019-03-28'] <- NA
detx$NumDetx[detx$DateID == 'MLS1143_2019-04-01'] <- NA
detx$NumDetx[detx$DateID == 'MLS1143_2019-04-05'] <- NA
detx$NumDetx[detx$DateID == 'MLS1143_2019-04-14'] <- 100
detx$NumDetx[detx$DateID == 'MLS1143_2019-04-25'] <- NA
detx$NumDetx[detx$DateID == 'MLS1143_2019-04-26'] <- NA
detx$NumDetx[detx$DateID == 'MLS1143_2020-03-26'] <- NA
detx$NumDetx[detx$DateID == 'MLS1143_2020-03-29'] <- NA
detx$NumDetx[detx$DateID == 'MLS1143_2020-03-31'] <- NA
detx$NumDetx[detx$DateID == 'MLS1143_2020-04-01'] <- 50
detx$NumDetx[detx$DateID == 'MLS1143_2020-04-11'] <- NA



detx$NumDetx[detx$DateID == 'SDF1508_2023-04-11'] <- 1000
detx$NumDetx[detx$DateID == 'SDF1508_2023-04-12'] <- 3000
detx$NumDetx[detx$DateID == 'SDF1508_2023-04-13'] <- 7500
detx$NumDetx[detx$DateID == 'SDF1508_2023-04-14'] <- 2000

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
detx$NumDetx[detx$DateID == 'MLS619_2022-04-12'] <- 3000
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

detx$NumDetx[detx$DateID == 'NEW88_2021-03-31'] <- 1000 # check other times on date
detx$NumDetx[detx$DateID == 'NEW88_2022-04-26'] <- 6000
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
detx$NumDetx[detx$DateID == 'MOET019_2022-04-26'] <- 1000
detx$NumDetx[detx$DateID == 'MOET019_2023-05-07'] <- NA

detx$NumDetx[detx$DateID == 'SDF1734_2019-04-03'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-04-04'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-04-15'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-04-16'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-04-18'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-04-24'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-04-25'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-04-26'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-04-27'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-04-29'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-05-01'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-05-03'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-05-04'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-05-05'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-05-08'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-05-12'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-05-13'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-05-14'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-05-15'] <- NA
detx$NumDetx[detx$DateID == 'SDF1734_2019-05-16'] <- NA



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
detx$NumDetx[detx$DateID == 'NEW1387_2024-04-24'] <- NA
detx$NumDetx[detx$DateID == 'NEW1387_2024-05-01'] <- NA
detx$NumDetx[detx$DateID == 'NEW1387_2024-05-17'] <- NA
detx$NumDetx[detx$DateID == 'NEW1387_2024-05-18'] <- NA



detx$NumDetx[detx$DateID == 'NEW51_2020-03-20'] <- NA
detx$NumDetx[detx$DateID == 'NEW51_2020-03-29'] <- NA
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

#no MLS411 data from '19, though?
# detx$NumDetx[detx$DateID == 'MLS411_2019-03-29'] <- NA
# detx$NumDetx[detx$DateID == 'MLS411_2019-03-30'] <- NA
# detx$NumDetx[detx$DateID == 'MLS411_2019-04-02'] <- NA
# detx$NumDetx[detx$DateID == 'MLS411_2019-04-03'] <- NA
detx$NumDetx[detx$DateID == 'MLS411_2021-03-26'] <- NA
detx$NumDetx[detx$DateID == 'MLS411_2020-03-29'] <- NA
detx$NumDetx[detx$DateID == 'MLS411_2023-04-25'] <- 100
detx$NumDetx[detx$DateID == 'MLS411_2024-03-29'] <- NA
detx$NumDetx[detx$DateID == 'MLS411_2024-04-28'] <- 500
detx$NumDetx[detx$DateID == 'MLS411_2024-04-16'] <- 500

detx$NumDetx[detx$DateID == 'SDF791_2019-04-18'] <- 10
detx$NumDetx[detx$DateID == 'SDF791_2019-04-20'] <- 10
detx$NumDetx[detx$DateID == 'SDF791_2019-04-29'] <- NA
detx$NumDetx[detx$DateID == 'SDF791_2019-05-01'] <- NA
detx$NumDetx[detx$DateID == 'SDF791_2019-05-02'] <- 5
detx$NumDetx[detx$DateID == 'SDF791_2019-05-06'] <- 100
detx$NumDetx[detx$DateID == 'SDF791_2021-04-14'] <- 1500
detx$NumDetx[detx$DateID == 'SDF791_2021-04-18'] <- 50
detx$NumDetx[detx$DateID == 'SDF791_2022-04-13'] <- 1000
detx$NumDetx[detx$DateID == 'SDF791_2023-05-08'] <- 250



detx$NumDetx[detx$DateID == 'STA019_2019-04-20'] <- 1
detx$NumDetx[detx$DateID == 'STA019_2019-04-21'] <- NA
detx$NumDetx[detx$DateID == 'STA019_2019-04-22'] <- NA
detx$NumDetx[detx$DateID == 'STA019_2019-04-23'] <- NA
detx$NumDetx[detx$DateID == 'STA019_2019-04-25'] <- NA
detx$NumDetx[detx$DateID == 'STA019_2019-05-04'] <- NA
detx$NumDetx[detx$DateID == 'STA019_2019-05-10'] <- NA
## check 5/25?



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
detx$NumDetx[detx$DateID == 'NEW1306_2023-04-17'] <- 6000

detx$NumDetx[detx$DateID == 'NEW1306_2024-04-12'] <- 6000
detx$NumDetx[detx$DateID == 'NEW1306_2024-04-13'] <- 2000
detx$NumDetx[detx$DateID == 'NEW1306_2024-04-13'] <- 500

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
detx$NumDetx[detx$DateID == 'SDF1112_2022-04-22'] <- 50
detx$NumDetx[detx$DateID == 'SDF1112_2023-04-15'] <- 750
detx$NumDetx[detx$DateID == 'SDF1112_2023-04-16'] <- 500
detx$NumDetx[detx$DateID == 'SDF1112_2024-04-12'] <- 500
detx$NumDetx[detx$DateID == 'SDF1112_2024-04-13'] <- 500
detx$NumDetx[detx$DateID == 'SDF1112_2024-04-15'] <- 500
detx$NumDetx[detx$DateID == 'SDF1112_2024-04-15'] <- 100
detx$NumDetx[detx$DateID == 'SDF1112_2024-04-24'] <- NA



detx$NumDetx[detx$DateID == 'NEW63_2019-04-04'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-04-05'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-04-09'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-04-10'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-04-12'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-04-13'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-04-26'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-04-27'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-04-28'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-04-30'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-01'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-02'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-03'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-04'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-05'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-06'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-07'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-08'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-09'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-10'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-11'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-12'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-13'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-14'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-15'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-16'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-17'] <- NA
detx$NumDetx[detx$DateID == 'NEW63_2019-05-18'] <- NA

detx$NumDetx[detx$DateID == 'NEW63_2024-04-07'] <- NA

detx$NumDetx[detx$DateID == 'NEW94_2019-04-14'] <- NA
detx$NumDetx[detx$DateID == 'NEW94_2019-04-15'] <- NA
detx$NumDetx[detx$DateID == 'NEW94_2019-04-16'] <- NA
detx$NumDetx[detx$DateID == 'NEW94_2019-04-17'] <- NA
detx$NumDetx[detx$DateID == 'NEW94_2019-04-18'] <- NA
detx$NumDetx[detx$DateID == 'NEW94_2019-04-19'] <- NA
detx$NumDetx[detx$DateID == 'NEW94_2019-04-20'] <- NA
detx$NumDetx[detx$DateID == 'NEW94_2019-04-21'] <- NA
detx$NumDetx[detx$DateID == 'NEW94_2019-04-22'] <- 2000
detx$NumDetx[detx$DateID == 'NEW94_2019-04-23'] <- 5000
detx$NumDetx[detx$DateID == 'NEW94_2019-05-06'] <- NA
detx$NumDetx[detx$DateID == 'NEW94_2019-05-07'] <- NA
detx$NumDetx[detx$DateID == 'NEW94_2019-05-08'] <- NA
detx$NumDetx[detx$DateID == 'NEW94_2019-05-09'] <- NA
detx$NumDetx[detx$DateID == 'NEW94_2019-05-10'] <- NA
detx$NumDetx[detx$DateID == 'NEW94_2019-05-11'] <- NA

detx$NumDetx[detx$DateID == 'KWN581_2021-04-07'] <- 500
detx$NumDetx[detx$DateID == 'KWN581_2021-04-20'] <- NA
detx$NumDetx[detx$DateID == 'KWN581_2022-03-24'] <- NA
detx$NumDetx[detx$DateID == 'KWN581_2023-04-07'] <- 2500
detx$NumDetx[detx$DateID == 'KWN581_2023-04-07'] <- 2000
detx$NumDetx[detx$DateID == 'KWN581_2023-04-08'] <- 1000
detx$NumDetx[detx$DateID == 'KWN581_2023-04-11'] <- 1000
detx$NumDetx[detx$DateID == 'KWN581_2023-04-12'] <- 3000
detx$NumDetx[detx$DateID == 'KWN581_2023-04-13'] <- 3000

detx$NumDetx[detx$DateID == 'KWN827_2019-04-12'] <- NA
detx$NumDetx[detx$DateID == 'KWN827_2019-04-13'] <- NA
detx$NumDetx[detx$DateID == 'KWN827_2019-04-15'] <- NA
detx$NumDetx[detx$DateID == 'KWN827_2019-04-16'] <- NA
detx$NumDetx[detx$DateID == 'KWN827_2019-04-17'] <- NA
detx$NumDetx[detx$DateID == 'KWN827_2019-04-18'] <- NA
detx$NumDetx[detx$DateID == 'KWN827_2019-05-04'] <- NA

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

detx$NumDetx[detx$DateID == 'KWN827_2021-03-31'] <- NA

detx$NumDetx[detx$DateID == 'MIR019_2019-03-28'] <- NA
detx$NumDetx[detx$DateID == 'MIR019_2019-03-29'] <- NA
detx$NumDetx[detx$DateID == 'MIR019_2019-03-31'] <- 1
detx$NumDetx[detx$DateID == 'MIR019_2019-04-05'] <- 1
detx$NumDetx[detx$DateID == 'MIR019_2019-04-09'] <- NA
detx$NumDetx[detx$DateID == 'MIR019_2019-04-09'] <- NA
detx$NumDetx[detx$DateID == 'MIR019_2019-04-15'] <- NA
detx$NumDetx[detx$DateID == 'MIR019_2019-04-17'] <- NA
detx$NumDetx[detx$DateID == 'MIR019_2019-04-21'] <- NA
detx$NumDetx[detx$DateID == 'MIR019_2019-04-23'] <- NA
detx$NumDetx[detx$DateID == 'MIR019_2019-04-24'] <- NA

detx$NumDetx[detx$DateID == 'MLS165_2021-03-30'] <- NA
detx$NumDetx[detx$DateID == 'MLS165_2021-04-02'] <- NA
detx$NumDetx[detx$DateID == 'MLS165_2021-04-19'] <- NA
detx$NumDetx[detx$DateID == 'MLS165_2021-04-20'] <- NA

detx$NumDetx[detx$DateID == 'MLS318_2020-03-19'] <- NA

detx$NumDetx[detx$DateID == 'NEW174_2022-04-13'] <- 6000
detx$NumDetx[detx$DateID == 'NEW174_2022-04-26'] <- 6000

detx$NumDetx[detx$DateID == 'NEW450_2021-03-31'] <- 2000
detx$NumDetx[detx$DateID == 'NEW450_2021-04-10'] <- 1000
detx$NumDetx[detx$DateID == 'NEW450_2021-04-11'] <- 3000
detx$NumDetx[detx$DateID == 'NEW450_2021-04-14'] <- 3000
detx$NumDetx[detx$DateID == 'NEW450_2022-04-01'] <- 3000
detx$NumDetx[detx$DateID == 'NEW450_2022-04-06'] <- 250
detx$NumDetx[detx$DateID == 'NEW450_2022-04-13'] <- 6000
detx$NumDetx[detx$DateID == 'NEW450_2022-04-26'] <- 2500
detx$NumDetx[detx$DateID == 'NEW450_2023-04-14'] <- 12000
detx$NumDetx[detx$DateID == 'NEW450_2024-03-29'] <- NA
detx$NumDetx[detx$DateID == 'NEW450_2024-04-12'] <- NA

detx$NumDetx[detx$DateID == 'RUB019_2019-04-10'] <- NA
detx$NumDetx[detx$DateID == 'RUB019_2020-03-27'] <- NA
detx$NumDetx[detx$DateID == 'RUB019_2020-03-29'] <- NA
detx$NumDetx[detx$DateID == 'RUB019_2020-04-21'] <- NA
detx$NumDetx[detx$DateID == 'RUB019_2020-04-29'] <- NA
detx$NumDetx[detx$DateID == 'RUB019_2020-04-30'] <- NA
detx$NumDetx[detx$DateID == 'RUB019_2021-04-22'] <- NA
detx$NumDetx[detx$DateID == 'RUB019_2022-03-31'] <- NA
detx$NumDetx[detx$DateID == 'RUB019_2022-04-01'] <- NA

detx$NumDetx[detx$DateID == 'BOD119_2023-04-13'] <- 3000
detx$NumDetx[detx$DateID == 'BOD119_2023-04-14'] <- 3000
detx$NumDetx[detx$DateID == 'BOD119_2023-04-15'] <- 1000
detx$NumDetx[detx$DateID == 'BOD119_2023-04-16'] <- 2000
detx$NumDetx[detx$DateID == 'BOD119_2023-04-17'] <- 100
detx$NumDetx[detx$DateID == 'BOD119_2024-04-11'] <- 1000
detx$NumDetx[detx$DateID == 'BOD119_2024-04-30'] <- 100

detx$NumDetx[detx$DateID == 'MON0516_2022-03-24'] <- NA
detx$NumDetx[detx$DateID == 'MON0516_2022-03-31'] <- NA
detx$NumDetx[detx$DateID == 'MON0516_2022-04-21'] <- NA
detx$NumDetx[detx$DateID == 'MON0516_2022-04-25'] <- NA
detx$NumDetx[detx$DateID == 'MON0516_2023-04-03'] <- NA
detx$NumDetx[detx$DateID == 'MON0516_2023-04-05'] <- NA
detx$NumDetx[detx$DateID == 'MON0516_2024-03-09'] <- NA

detx$NumDetx[detx$DateID == 'SDF941_2022-04-26'] <- NA
detx$NumDetx[detx$DateID == 'SDF941_2023-04-05'] <- NA
detx$NumDetx[detx$DateID == 'SDF941_2023-04-06'] <- NA
detx$NumDetx[detx$DateID == 'SDF941_2023-04-07'] <- NA
detx$NumDetx[detx$DateID == 'SDF941_2023-04-08'] <- NA
detx$NumDetx[detx$DateID == 'SDF941_2023-04-19'] <- NA
detx$NumDetx[detx$DateID == 'SDF941_2023-04-20'] <- NA
detx$NumDetx[detx$DateID == 'SDF941_2023-04-21'] <- NA
detx$NumDetx[detx$DateID == 'SDF941_2023-04-22'] <- NA
detx$NumDetx[detx$DateID == 'SDF941_2023-04-23'] <- NA
detx$NumDetx[detx$DateID == 'SDF941_2023-04-24'] <- NA

detx$NumDetx[detx$DateID == 'MLS567_2020-03-15'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2020-03-16'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2020-03-17'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2020-03-18'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2020-03-19'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2020-03-20'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2020-03-21'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2020-03-22'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2020-03-23'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2020-03-24'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2020-03-25'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2020-03-26'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2020-04-02'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2020-04-10'] <- NA

detx$NumDetx[detx$DateID == 'MLS567_2020-04-21'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2020-04-22'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2020-04-24'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2022-03-20'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2022-04-19'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2022-04-26'] <- NA
detx$NumDetx[detx$DateID == 'MLS567_2023-04-15'] <- 100

detx$NumDetx[detx$DateID == 'NEW1002_2022-04-25'] <- 500

detx$NumDetx[detx$DateID == 'NEW1003_2022-04-13'] <- 100
detx$NumDetx[detx$DateID == 'NEW1003_2022-04-19'] <- NA
detx$NumDetx[detx$DateID == 'NEW1003_2023-04-07'] <- NA

detx$NumDetx[detx$DateID == 'NEW1038_2021-03-29'] <- NA
detx$NumDetx[detx$DateID == 'NEW1038_2023-04-30'] <- NA

detx$NumDetx[detx$DateID == 'NEW1098_2023-04-16'] <- 5000
detx$NumDetx[detx$DateID == 'NEW1098_2023-04-17'] <- 5000
detx$NumDetx[detx$DateID == 'NEW1098_2023-04-21'] <- 5000
detx$NumDetx[detx$DateID == 'NEW1098_2023-04-22'] <- 5000
detx$NumDetx[detx$DateID == 'NEW1098_2023-04-23'] <- 2500
detx$NumDetx[detx$DateID == 'NEW1098_2023-04-24'] <- 5000
detx$NumDetx[detx$DateID == 'NEW1098_2023-04-25'] <- 1000

detx$NumDetx[detx$DateID == 'SDF900_2021-04-10'] <- 5000
detx$NumDetx[detx$DateID == 'SDF900_2021-04-11'] <- 5000
detx$NumDetx[detx$DateID == 'SDF900_2021-04-06'] <- 5000

detx$NumDetx[detx$DateID == 'SDF900_2022-04-22'] <- NA
detx$NumDetx[detx$DateID == 'SDF900_2022-04-27'] <- NA
detx$NumDetx[detx$DateID == 'SDF900_2022-04-28'] <- NA
detx$NumDetx[detx$DateID == 'SDF900_2023-04-12'] <- 5000
detx$NumDetx[detx$DateID == 'SDF900_2023-04-13'] <- 10000
detx$NumDetx[detx$DateID == 'SDF900_2023-04-14'] <- 5000

detx$NumDetx[detx$DateID == 'SDF1746_2021-04-07'] <- 5000
detx$NumDetx[detx$DateID == 'SDF1746_2021-04-10'] <- 5000
detx$NumDetx[detx$DateID == 'SDF1746_2021-04-11'] <- 1000
detx$NumDetx[detx$DateID == 'SDF1746_2022-03-21'] <- NA
detx$NumDetx[detx$DateID == 'SDF1746_2023-04-12'] <- 5000
detx$NumDetx[detx$DateID == 'SDF1746_2023-04-13'] <- 10000
detx$NumDetx[detx$DateID == 'SDF1746_2023-04-15'] <- 7500
detx$NumDetx[detx$DateID == 'SDF1746_2023-04-16'] <- 2500

detx$NumDetx[detx$DateID == 'NEW448_2022-04-24'] <- 3000
detx$NumDetx[detx$DateID == 'NEW448_2023-04-13'] <- 3000
detx$NumDetx[detx$DateID == 'NEW448_2023-04-14'] <- 1000
detx$NumDetx[detx$DateID == 'NEW448_2023-04-21'] <- 1000


############  


  
detx <- detx %>% drop_na(NumDetx)
detx$`Relative Call Intensity` <- detx$NumDetx

table(detx$Site, detx$Year)

# 
# sitedetx <- detx[detx$Site %in% c("WEA019", 'CALT019', 'NEW88',
#                                   'MLS737','MLS619','MOET019',
#                                   'SDF1734','SDF1264'),]
# site <- 'SDF1746'
sitedetx <- detx[detx$Site == site,]
sitedetx$RecordingDate <- sitedetx$start_date
sitedetx <- subset(sitedetx, select=-c(start_date))


first_last_dates <- sitedetx %>%
  filter(!is.na(RecordingDate)) %>%
  mutate(Year = format(RecordingDate, "%Y")) %>%
  group_by(Year) %>%
  summarise(
    start_date = min(RecordingDate) - 1, 
    end_date = max(RecordingDate) + 1,   
    .groups = "drop"
  )

sitedetx <- sitedetx %>%
  mutate(Year = format(RecordingDate, "%Y")) %>%
  left_join(first_last_dates, by = "Year")

padded_data <- sitedetx %>%
  group_by(Year) %>%
  group_modify(~ {
    start_date <- unique(.x$start_date)
    end_date <- unique(.x$end_date)
    if (!is.na(start_date) & !is.na(end_date)) {
      pad(.x, start_val = start_date, end_val = end_date, by = "RecordingDate")
    } else {
      .x
    }
  }) %>%
  ungroup()


padded_data$DateID <- paste0(site,"_",padded_data$RecordingDate)
padded_data$Year <-  format(padded_data$RecordingDate,"%Y")
padded_data$Day <-  format(padded_data$RecordingDate,"%m/%d")
padded_data$Site <- site

padded_data_withNAs <- padded_data

padded_data <- padded_data %>%
  mutate(`Relative Call Intensity` = ifelse(is.na(NumDetx), 0, NumDetx))


p <- ggplot() +
  geom_jitter(data = padded_data_withNAs, aes(x = Day, y = Year, color = Year, size = `Relative Call Intensity`),  height = 0.03, fill = 'black', alpha = 0.7) +
  scale_x_discrete(breaks = every_nth(n = 3)) +
  theme(legend.position="none")
p
# geom_jitter(data = sitedetx, aes(x = Day, y = Site, color = Year, size = NumDetx),  height = 0.11, alpha = 0.7)
ggplotly(p)


p <- ggplot() + 
  geom_line(data = padded_data, aes(x = Day, y = `Relative Call Intensity`, group = Year, color = Year), size=2) +
  # geom_smooth(data = padded_data, aes(x = Day, y = `Relative Call Intensity`, group = Year, color = Year), method = "loess", size=2) +
  geom_point(data = padded_data, aes(x = Day, y = `Relative Call Intensity`, group = Year, color = Year)) +
  geom_area(data = padded_data, aes(x = Day, y = `Relative Call Intensity`, group = Year, fill = Year), alpha = .5) +
  scale_x_discrete(breaks = every_nth(n = 3)) +
  scale_color_manual(values = c("#7F58AF","#64C5EB","#E84D8A","#FEB326","#43aa8b","#023047")) +
  scale_fill_manual(values = c("#7F58AF","#64C5EB","#E84D8A","#FEB326","#43aa8b","#023047")) +
  theme(axis.title=element_text(size=20),
        legend.position="none",
        strip.text = element_text(size=12)) +
  geom_hline(data = data.frame(type="A", y=0), mapping=aes(yintercept=y), size = 1) +
  # geom_vline(xintercept = c(2,16,30,44),  linetype="dashed", size = 1) + 
  ylim(0, NA) +
  facet_wrap( ~ Year, scales="free_y", ncol = 1) +
  labs(title = paste(padded_data$Site,"Wood Frog Detections")) #+
  # coord_flip()
p
# geom_jitter(data = sitedetx, aes(x = Day, y = Site, color = Year, size = NumDetx),  height = 0.11, alpha = 0.7)
# ggplotly(p)

# ggplot(padded_data, aes(Year, `Relative Call Intensity`, fill = Year)) +
#   geom_rain() +
#   theme_classic() +
#   theme(axis.title.x = element_blank(), 
#         axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
#   coord_flip()






delete_these_files <- c('KWN473_20200313_150000.wav',
                        'KWN473_20200313_200000.wav',
                        'KWN473_20200313_220000.wav',
                        'KWN473_20200314_150000.wav',
                        'KWN473_20200314_200000.wav',
                        'KWN473_20200314_220000.wav',
                        'KWN473_20200315_150000.wav',
                        'KWN473_20200315_200000.wav',
                        'KWN473_20200315_220000.wav',
                        'NEW383_20210323_150000.wav',
                        'NEW383_20220319_150000.wav',
                        'NEW88_20230326_150000.wav')
delete_media(conx, delete_these_files)

  
  
  mediasubset <- subset_files(conx, 'KWN238', "2024-04-20", "2024-05-01")
  
  
  Sys.time()
  start <- Sys.time()
  scores <- scoresDetect(
    con = conx,
    recordingNames = mediasubset$filename,
    templateNames = 'template_SDF791_20210408_150000bin_thresh40_cu12',
    # scoreThresholds = 12,
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
                                WHERE filename = 'NEW448_20210321_150000.wav' ")
stop <- Sys.time()
Sys.time()
stop - start
#NOT DELETED YET






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
view(visitlist)
# sitelist <- unique(bucketlist$Site)
# diff <- setdiff(sitelist,visitlist)
# diff


#add files to db


bucketadd <- bucketlist[bucketlist$Site == "KWN934",]

bucketadd <- bucketadd[,c("filename","filepath",'start_date','start_time')]

bucketadd$pk_mediaid <- NA
bucketadd$fk_visitid <- 109
bucketadd$sb_exclude <- NA
bucketadd$fk_sciencebaseid <- NA
bucketadd$filesize <- NA
bucketadd$timestamp <- NA
bucketadd$media_type <- "audio"

# medialist <- DBI::dbReadTable(conx, name = 'media')
# bucketlist$Site  <- str_extract(bucketlist$Key, "[^_]+")

# unaddedfiles <- anti_join(bucketadd,medialist,by="filename")


dbAppendTable(conx, "media", bucketadd)










#### Commands
# delete
RSQLite::dbExecute(conn = conx,
                   statement = "DELETE FROM media
                                WHERE filename = 'KWN581_20230306_140000.wav' ")

RSQLite::dbExecute(conn = conx,
                   statement = "DELETE FROM media
                                WHERE fk_visitid = 13  ")

RSQLite::dbExecute(conn = conx,
                   statement = "DELETE FROM modeloutputs
                                WHERE fk_mediaid = '16462' ")



stations <- locationsGetStations(
  amm_fp = '~/R/AMMonitor_VPMon/VPMon_AMM',
  conx,
  noaa_token = "settings",
  startDate = NULL,
  endDate = NULL,
  # minlat, minlong, maxlat, maxlong
  bbox =  c(42.8, -73.5, 45.1, -71.4),  
  dbInsert = FALSE,
  disconnect = FALSE
)
# -73.43904261392613,26973989929895,9364526975269,1550900568005


DBI::dbAppendTable(
  conn = conx, 
  name = "locations", 
  value = stations
)

locations <- DBI::dbReadTable(conx, name = "locations")
indices <- stats::complete.cases(locations$lat, locations$long, locations$location_type)
locations_xy <- locations[indices, ]

g <- ggplot2::ggplot(locations_xy,  aes(x = long, y = lat))  +
  geom_point(
    data = locations_xy, 
    aes(x = long, y = lat, shape = location_type, color = location_type), 
    size = 3) +
  coord_fixed() +
  theme(
    legend.position = "top", 
    legend.direction = "horizontal") + 
  labs(
    title = "Map of Locations",
    x = "Longitude",
    y = "Latitude") 
#theme_minimal()

# show the plot
g




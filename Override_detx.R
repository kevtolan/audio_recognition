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


detx$NumDetx[detx$DateID == 'MLS411_2021-03-26'] <- NA
detx$NumDetx[detx$DateID == 'MLS411_2020-03-29'] <- NA

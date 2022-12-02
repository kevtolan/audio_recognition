library(tidyverse)
library(ggalt)
library(lubridate)
library(hrbrthemes)
library(plotly)
library(pivottabler)
library(reshape2)
library(padr)
library(raincloudplots)
library(dplyr)
library(gghalves)
library(RColorBrewer)
library(ggbeeswarm) 
library(cowplot)
library(dplyr)
library(readr)

df <- list.files(path=getwd(), pattern = ".csv") %>% 
  lapply(read_csv) %>% 
  bind_rows

df <- df[,-c(1,2,3,4)]
df$datetime2 <- as.POSIXct(df$datetime, format = '%m/%d/%y %H:%M', tz = "GMT")
df$Year <-  format(as.Date(df$datetime,'%m/%d/%y %H:%M'),"%Y")


df_2019 <- df[df$Year == '2019',]
df_2020 <- df[df$Year == '2020',]
df_2021 <- df[df$Year == '2021',]
df_2022 <- df[df$Year == '2022',]

Site <- df_2019$Site
Bioph <- df_2019$Bioph


df_2019 <- df_2019 %>%
        pad(group = c('Site', 'Bioph'))

df_2019$CountFinal[is.na(df_2019$CountFinal)] <- 0
df_2019$Year[is.na(df_2019$Year)] <- 2019

Site <- df_2020$Site
Bioph <- df_2020$Bioph

df_2020 <- df_2020 %>%
  pad(group = c('Site', 'Bioph'))

df_2020$CountFinal[is.na(df_2020$CountFinal)] <- 0
df_2020$Year[is.na(df_2020$Year)] <- 2020

Site <- df_2021$Site
Bioph <- df_2021$Bioph

df_2021 <- df_2021 %>%
  pad(group = c('Site', 'Bioph'))

df_2021$CountFinal[is.na(df_2021$CountFinal)] <- 0
df_2021$Year[is.na(df_2021$Year)] <- 2021

Site <- df_2022$Site
Bioph <- df_2022$Bioph


df_2022 <- df_2022 %>%
  pad(group = c('Site', 'Bioph'))

df_2022$CountFinal[is.na(df_2022$CountFinal)] <- 0
df_2022$Year[is.na(df_2022$Year)] <- 2022

df2 <-  bind_rows(df_2019,df_2020,df_2021,df_2022)

df2$month <- format(df2$datetime2,"%m")
df2$day <- format(df2$datetime2,"%d")
df2$Date <- paste0(df2$month,'/',df2$day)
#df2$Date <- as.Date(df2$Date)
df2$Relative_Intensity <- df2$CountFinal
df2$DateFull <- df2$datetime2
df2$Time <- substr(df2$DateFull,12,16)   
df2$DateTime <- paste0(df2$Date," ",df2$Time)
df2$DateNum <- strftime(df2$DateFull, format = "%j")


df2 <- df2[,-c(1,2,5)]

hours <- c("15:00","19:00","20:00","21:00","22:00")

df3 <- df2[df2$Time %in% hours, ]



df3_2019 <- df3[df3$Year == '2019',]
df3_2020 <- df3[df3$Year == '2020',]
df3_2021 <- df3[df3$Year == '2021',]
df3_2022 <- df3[df3$Year == '2022',]

df4 <-   bind_rows(df3_2019,df3_2021)


################### Multiple line plot
 
plot <- ggplot(df3, aes(x = Date, y = Relative_Intensity, group = Bioph)) + 
   #geom_point(aes(color = Bioph)) +
   geom_smooth(aes(color = Bioph), method = 'loess') 
  # scale_color_manual(values = c("#7F58AF","#64C5EB","#E84D8A","#FEB326")) +
 #ylim(-1000, NA)
 #scale_x_date(labels = df3$dateOG)
plot
ggplotly(plot)


########### Box plot


plot <- ggplot(df3,aes(x=DateNum,y=Bioph, fill = Bioph, colour = Bioph))+
  geom_half_violin()

plot

ggplotly(plot)












pt <- PivotTable$new()
pt$addData(df)
pt$addColumnDataGroups("Date", addTotal=FALSE)
#pt$addColumnDataGroups("Count")    #    << **** CODE CHANGE **** <<
pt$addRowDataGroups("Year", addTotal=FALSE,)
# pt$defineCalculation(calculationName="Total calls",
#                      summariseExpression="sum(Count, na.rm=TRUE)/n()")
pt$defineCalculation(calculationName="Total calls",
                     summariseExpression="sum(Count")
pt$renderPivot()
pt$evaluatePivot()

df_piv <- pt$asDataFrame()
df_piv
df_piv$Year <- c("2019","2020","2021","2022")
df_melt <- melt(df_piv)
df_melt[is.na(df_melt)] <- 0

plot <- ggplot(df_melt, aes(x=variable, y=value, group=Year)) +
        geom_area(aes(color=Year)) +
        scale_color_manual(values = c("#7F58AF","#64C5EB","#E84D8A","#FEB326")) +
     #   geom_vline(aes(color=Year)) +
        scale_y_continuous(limits = c(0, NA)) +
        theme_minimal()
plot

# 

# 
# 
# plot <- ggplot(df, aes(x= Date, y=Count, group=Year)) + 
#   geom_line(aes(color=Site)) +
#   scale_y_continuous(limits = c(0, NA)) + 
#   theme_minimal() 
#   #scale_x_date(labels = date_format("%b"))
# plot 






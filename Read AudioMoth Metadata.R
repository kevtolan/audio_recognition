
# Path to the wave file #

# this lists all the *.wav files in a folder. 
# if you have many files this will list them all with the full 
# directory path 

wavfiles <- list.files("C:/Users/mhall/OneDrive/Desktop/",
                pattern = ".wav",
                full.names = TRUE)

# this simple call will go through all the files, 
# extract the data you want, and put it into a data frame #

allmetadata <- do.call(rbind,lapply(wavfiles, getWavMetaData))

# BELOW IS A FUNCTION TO EXTRACT ALL THE METADATA #
# THIS CAN EITHER BE EXECUTED IN R - highlight the whole function and hit RUN 
# OR copied into another R script and use source("script with function") 


# Params 
# @wavfiles = path to *.wav files of interest to extract data 
# @filenames = optional vector of file names for the *.wav files - should be save length 
#              as wavfiles and in same order

getWavMetaData <- function(wavfiles, filenames = NULL){

# parse out the file name - this assumes the full path is given #
if(is.null(filenames)){
 if(sum(grep(pattern = "\\/", wavfiles))!=0L){
fileID <- gsub(".*/","",wavfiles)}else{
fileID <- wavfiles
}
}else{
fileID <- filenames
}

# how many characters at in the file #

size <- readBin(wavfiles, integer(), size = 4, endian = "little")

# read in the data #

binDat <- suppressWarnings(readBin(wavfiles, "character", n = size))

# determine which element of the list has what you want #

metadat <- binDat[which(grepl("Recorded",binDat))]

# grab the different metadata components #

date_time <- regexpr(pattern = "Recorded at \\d*:\\d*:\\d*\\d*\\/\\d*\\/\\d*",
                     text = metadat[1])

# Get date location in string #
timeLoc <- regexpr(pattern = "[0-9]{2}:[0-9]{2}:[0-9]{2} [0-9]{2}\\/[0-9]{2}\\/[0-9]{4}", 
                     text = metadat[1])

# Get voltage data location in string #
voltLoc <- regexpr("[0-9].[0-9]V", metadat[1])

# Get temp. data location in string #
tempLoc <- regexpr("[0-9]{1,2}.[0-9]C", metadat[1]) 

# Get Audiomoth ID location # 
audiomothLoc <- regexpr(pattern = "AudioMoth (.*) at ", metadat[1]) 

# get the date in character form as is the file #
timechar <- substr(metadat[1],start = timeLoc, stop = timeLoc+attr(timeLoc,"match.length")-1)

# make the date into a useful time format #
recordtime <- as.POSIXct(timechar, format = "%H:%M:%S %d/%m/%Y", tz = "UTC")

# Get state of battery in volts *note -2 to get rid of space and V*
batteryState <- substr(metadat[1], start = voltLoc, stop = voltLoc+attr(voltLoc,"match.length")-2)

# Get state of battery in C *note -2 to get rid of space and C*
tempRecord <- substr(metadat[1], start = tempLoc, stop = tempLoc+attr(tempLoc,"match.length")-2)

# audioMoth id # 
amID <- substr(metadat[1], 
               start = audiomothLoc+10, 
               stop = (audiomothLoc)+attr(audiomothLoc,"match.length")-5)


return(data.frame(AudioMothID = amID,
                  File = fileID,
                  Recorded = recordtime,
                  Battery_V = as.numeric(batteryState),
                  Temperature_C = as.numeric(tempRecord)))
}





# TEMPLATE AUDIO FILES: https://drive.google.com/drive/folders/1R6VjI1TQPksERSB0YAKoBwg7S9EhSQXM

library(tuneR)
library(monitoR)
library(plyr) 

# Wood Frog
WOFRITemplate1 <- makeBinTemplate("WOFRITemplate1.WAV", 
                                  t.lim = c(241.59,241.69),
                                  frq.lim = c(0,6),
                                  score.cutoff = 13,
                                  amp.cutoff = -29,  
                                  name="WOFRITemplate1")                                 
WOFRITemplate2 <- makeBinTemplate("WOFRITemplate2.wav",
                                        t.lim = c(2.275,2.325),
                                        frq.lim = c(0,4), 
                                        amp.cutoff = -27,
                                        buffer = .5,
                                        score.cutoff = 16,
                                        name="WOFRITemplate2")
WOFRITemplate3 <- makeBinTemplate("WOFRITemplate3.wav",
                                       t.lim = c(0.06,.16),
                                       frq.lim = c(0,6),
                                       amp.cutoff =-33,
                                       dens = .75,
                                       score.cutoff = 12,
                                       name="WOFRITemplate3")
# Spring Peeper
SPPEITemplate  <- makeBinTemplate("SPPEITemplate.wav",
                                  t.lim = c(.55,.9),
                                  frq.lim = c(2,6),
                                  amp.cutoff = -37,
                                  score.cutoff = 12,
                                  buffer = 1,
                                  name = "SPPEITemplate")                                  
# Eastern Whip-poor-will
EWPWTemplate <- makeBinTemplate("EWPWTemplate.wav",
                                 amp.cutoff = -20, 
                                 score.cutoff = 0,
                                 frq.lim = c(1,5),
                                 name = "EWPWTemplate")                                                                
# Barred Owl                                  
BADOTemplate <- makeCorTemplate("BADOTemplate.wav",
                                t.lim = c(2.65,3.35),
                                frq.lim = c(0.25,2.5),
                                score.cutoff = 0.4,
                                name="BADOTemplate")

bins <- combineBinTemplates(EWPWTemplate1,EWPWTemplate2)
cors <- combineCorTemplates(EWPWTemplate1,EWPWTemplate2)

survey <- 'BITH_ XC469600.wav'

#Bin
BinMatch <- binMatch(survey, bins,
                     show.prog = TRUE,cor.method = "pearson",time.source = "fileinfo",
                     rec.tz = "US/Eastern")
BinDetects <- findPeaks(BinMatch, frame = 1.01)
plot(BinDetects)

EWPWTemplate_Detects1 <- nrow(BinDetects@detections[["EWPWTemplate1"]])
EWPWTemplate_Detects1

#Cor
CorMatch <- corMatch(survey, cortemp1,
                     show.prog = TRUE,cor.method = "pearson",time.source = "fileinfo",
                     rec.tz = "US/Eastern")
CorDetects <- findPeaks(CorMatch, frame = 1.01)
plot(CorDetects)

EWPWTemplate_Detects1 <- nrow(CorDetects@detections[["EWPWTemplate1"]])
EWPWTemplate_Detects1




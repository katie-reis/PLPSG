###This code pertains to the Perceptual Learning Polysomnography (PLPSG) project.
###This is a file that will read in (1) Perceptual learning behavioral data, (2) Sleep statistic data (including spindle data), (3) demographic data.
###This data was collected by Katie Reis while in the APEX Lab at UChicago. 

#import necessary packages 
library(dplyr) #needed for data manipulation
library(data.table) #needed for reading in files
library(tidyverse) #needed for tidying data

#read in some relevant csv files beforehand 
setwd("~/Documents/PLPSG_sleep_analysis/")
demographics = read.csv("Demographics.csv")
#spindles = fread("PLPSG_Spindle_Counts.csv", select = c("subject","Spindles_Avg_11-16Hz"))

#import behavioral data that was scored in python using scripts "perceptual-learning-scoring.ipynb" and "Perceptual_Learning_Scoring_AfterCheck.ipynb"
setwd("~/Documents/PLPSG_sleep_analysis/PLPSG_behavioral_results/Scored_Final/")
behavioral = read.csv("data.csv")

#now switch working directories to recursively read in information from the hypnograms (sleep statistics)
setwd("~/Documents/PLPSG_sleep_analysis/PLPSG_sleepscoring_hypnogram/")
filelist = list.files(pattern = "*.txt")

#make some changes to the behavioral data
PLPSG_sleep_stats_longer = behavioral
PLPSG_sleep_stats_longer$subject = gsub("subj","",as.character(PLPSG_sleep_stats_longer$subject))
PLPSG_sleep_stats_longer = PLPSG_sleep_stats_longer %>% arrange(subject, block)
PLPSG_sleep_stats_longer = subset(PLPSG_sleep_stats_longer, select = -X)
PLPSG_sleep_stats_longer$block = as.factor(PLPSG_sleep_stats_longer$block)

PLPSG_sleep_stats_wider = PLPSG_sleep_stats_longer %>% 
  select(c(subject,block,score)) %>%
  pivot_wider(names_from = block,
              values_from = score) %>% 
  mutate(learning = (block3-block1),
         loss = -1*(block4-block3),
         recovery = (block5-block4),
         maintenance = (block6-block5),
         protective = (block6-block3),
         longtermrecovery = (block6 - block4),
         retention = (block6-block1),
         #fasting = factor(rep(c("yes", "no"),each=16)),
         condition = "",
         lightsoff = "",
         lightson = "",
         TRT = "",
         TST = "",
         SL = "",
         SE = "",
         WASO = "",
         W = "",
         N1 = "",
         N2 = "",
         N3 = "",
         R = "",
         perW = "",
         perN1 = "",
         perN2 = "",
         perN3 = "",
         perR = "",
         latN1 = "",
         latN2 = "",
         latN3 = "",
         latR = "") %>%
  relocate(c(condition), .after = subject)

for (i in 1:length(filelist)){
  #read in the file
  setwd("~/Documents/PLPSG_sleep_analysis/PLPSG_sleepscoring_hypnogram/")
  file_hypno = read.delim(filelist[i], header = FALSE, col.names = "stage")
  
  #identify when lights off (lights out clock time)
  recording_time_info = read.csv("PLPSG_lightson_lightsoff.csv", header = TRUE)
  
  #extract the subject ID
  PLPSG_sleep_stats_wider$subject[i] = substr(filelist[i], start = 5, stop = 7)
  
  #extract what group/condition they were in (sleep or wake)
  PLPSG_sleep_stats_wider$condition[i] = recording_time_info$condition[i]
  
  lightsoff = recording_time_info$lightsoff[i]
  PLPSG_sleep_stats_wider$lightsoff[i] = lightsoff
  
  #identify when lights on (lights on clock time)
  lightson = recording_time_info$lightson[i]
  PLPSG_sleep_stats_wider$lightson[i] = lightson
  
  #select only the rows (epochs) that are between lights off and lights on (inclusive)
  file_hypno = file_hypno[lightsoff:lightson,]
  
  ###now perform the following calculations: 
  #calculate number of epochs between lights off and on (total recording time = TRT, in minutes)
  TRT = length(file_hypno) / 60
  PLPSG_sleep_stats_wider$TRT[i] = TRT
  
  #time spent in W (in minutes)
  W = sum(file_hypno == 0) / 60  
  PLPSG_sleep_stats_wider$W[i] = W
  
  #time spent in N1 (in minutes)
  N1 = sum(file_hypno == 1) / 60
  PLPSG_sleep_stats_wider$N1[i] = N1
  
  #time spent in N2 (in minutes)
  N2 = sum(file_hypno == 2) / 60
  PLPSG_sleep_stats_wider$N2[i] = N2
  
  #time spent in N3 (in minutes)
  N3 = sum(file_hypno == 3) / 60
  PLPSG_sleep_stats_wider$N3[i] = N3
  
  #time spent in R (in minutes)
  R = sum(file_hypno == 4) / 60
  PLPSG_sleep_stats_wider$R[i] = R
  
  #total sleep time (TST; time spent in N1, N2, N3, or R, in minutes)
  TST = N1 + N2 + N3 + R 
  PLPSG_sleep_stats_wider$TST[i] = TST
  
  #sleep latency (SL; lights out to first epoch of sleep, in minutes)
  SL = (which(file_hypno!=0)[1]) / 60
  PLPSG_sleep_stats_wider$SL[i] = SL
  
  #percent sleep efficiency (TST/TRT * 100)
  PLPSG_sleep_stats_wider$SE[i] = (TST/TRT) * 100
  
  #wake after sleep onset (WASO; TRT - SL - TST, in minutes)
  PLPSG_sleep_stats_wider$WASO[i] = TRT - SL - TST
  
  ###now calculate percent of TST in each stage 
  #% spent in W (W/TST * 100)
  PLPSG_sleep_stats_wider$perW[i] = (W/TST) * 100
  
  #% spent in N1 (N1/TST * 100)
  PLPSG_sleep_stats_wider$perN1[i] = (N1/TST) * 100
  
  #% spent in N2 (N2/TST * 100)
  PLPSG_sleep_stats_wider$perN2[i] = (N2/TST) * 100
  
  #% spent in N3 (N3/TST * 100)
  PLPSG_sleep_stats_wider$perN3[i] = (N3/TST) * 100
  
  #% spent in R (R/TST * 100)
  PLPSG_sleep_stats_wider$perR[i] = (R/TST) * 100
  
  #latency in minutes to first epoch of N1
  PLPSG_sleep_stats_wider$latN1[i] = (which(file_hypno == 1)[1]) / 60
  
  #latency in minutes to first epoch of N2
  PLPSG_sleep_stats_wider$latN2[i] = (which(file_hypno == 2)[1]) / 60
  
  #latency in minutes to first epoch of N3
  PLPSG_sleep_stats_wider$latN3[i] = (which(file_hypno == 3)[1]) / 60
  
  #latency in minutes to first epoch of R
  PLPSG_sleep_stats_wider$latR[i] = (which(file_hypno == 4)[1]) / 60
}

PLPSG_sleep_stats_wider$condition = factor(PLPSG_sleep_stats_wider$condition)

#add demographic info
demographics = rename(demographics, subject = Subject)
demographics$subject = as.character(demographics$subject)
PLPSG_sleep_stats_wider = left_join(PLPSG_sleep_stats_wider, demographics, by = "subject")

#add spindle information
#PLPSG_sleep_stats_wider <- merge(PLPSG_sleep_stats_wider, spindles, by="subject")

#rename spindle column so that there isn't a dash
#PLPSG_sleep_stats_wider = PLPSG_sleep_stats_wider %>% rename(spindles = 'Spindles_Avg_11-16Hz')

#calculate median of pre-test score 
median_sleep = median(PLPSG_sleep_stats_wider$block1[PLPSG_sleep_stats_wider$condition == "sleep"])
median_wake = median(PLPSG_sleep_stats_wider$block1[PLPSG_sleep_stats_wider$condition == "wake"])

#create column that dictates whether folks are above the median (inclusive) for pre-test score
PLPSG_sleep_stats_wider = PLPSG_sleep_stats_wider %>% mutate(above_median = 
                                                               ifelse((block1 >= median_sleep & condition == "sleep") | 
                                                                        (block1 >= median_wake & condition == "wake"), 
                                                                      "yes", "no"))

#create a dataset that creates a one column called block
PLPSG_sleep_stats_longer = PLPSG_sleep_stats_wider %>%
  pivot_longer(cols = c("block1", "block3","block4","block5", "block6"),
               names_to = "block",
               values_to = "score")

PLPSG_sleep_stats_longer = PLPSG_sleep_stats_longer %>% mutate(test= ifelse(block == "block1", "PreTest",
                                                                            ifelse(block == "block3", "AfterTrainingPostTest",
                                                                                   ifelse(block == "block4", "BeforeSleepPostTest", 
                                                                                          ifelse(block == "block5", "AfterSleepPostTest","NightPostTest")))))

#export datasets
write.csv(PLPSG_sleep_stats_wider,"~/Documents/PLPSG_sleep_analysis/PLPSG_sleepscoring_hypnogram/PLPSG_sleep_stats_wider.csv", row.names = FALSE)
write.csv(PLPSG_sleep_stats_longer,"~/Documents/PLPSG_sleep_analysis/PLPSG_sleepscoring_hypnogram/PLPSG_sleep_stats_longer.csv", row.names = FALSE)

#clear environment
rm(list=ls()[-match(c("PLPSG_sleep_stats_wider", "PLPSG_sleep_stats_longer"), ls())])
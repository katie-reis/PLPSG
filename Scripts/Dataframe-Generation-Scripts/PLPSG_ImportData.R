###This code pertains to the Perceptual Learning Polysomnography (PLPSG) project.
###This is a file that will read in (1) Perceptual learning behavioral data, (2) Sleep statistic data (including spindle data), (3) demographic data.
###This data was collected by Katie Reis while in the APEX Lab at UChicago. 

#import necessary packages 
library(dplyr) #needed for data manipulation
library(data.table) #needed for reading in files
library(tidyverse) #needed for tidying data

#read in some questionnaire dataframes
setwd("~/repos/PL_Sleep_PSG/DataAnalysis/Data")

#demographics
demographics = read.csv("Demographics.csv")
demographics$Subject = as.character(demographics$Subject)

#language experience questionnaire 
languageexperience = read.csv("LanguageExperienceQuestionnaire.csv")
languageexperience = languageexperience %>% select(-c(Age, Gender)) %>% mutate_at("Subject", as.character) %>%
  rename("LanguageExperienceNotes" = "Notes") %>% rename_with(.fn = ~gsub("_years$", "_place_lived_years", .) %>%
  gsub("_lived$", "_place_lived", .), .cols = everything())
names(languageexperience)<-sapply(str_remove_all(colnames(languageexperience),"X"),"[") 

#usual sleep lengths
usualsleeplengths = read.csv("UsualSleepLengths.csv")
usualsleeplengths = usualsleeplengths %>% 
  mutate(Subject = as.character(Subject), UsualSleepLengthHours = as.numeric(UsualSleepLengthHours))

#sleep questionnaire 
sleepquestionnaire = read.csv("SleepQuestionnaire.csv")
sleepquestionnaire = sleepquestionnaire %>% mutate(Subject = as.character(Subject)) %>% rename("UsualBedTimeRange" = "UsualBedTime", "UsualWakeUpTimeRange" = "UsualWakeUpTime")

#sleep logs
sleeplogs = read.csv("PLPSG_SleepLog_ALL.csv")
sleeplogs = sleeplogs %>% mutate(Subject = as.character(Subject))

#stanford sleepiness scale responses
stanfordsleepiness = read.csv("StanfordSleepiness.csv")
stanfordsleepiness = stanfordsleepiness %>% mutate(Subject = as.character(Subject)) %>%
  rename_with(.cols = -1, .fn = ~ paste0("SS_", .))

#nback data 
nback = read.csv("N-back_Scores_SleepWake.csv")
names(nback)<-sapply(str_remove_all(colnames(nback),"X"),"[")
nback = nback %>% mutate(Subject = as.character(Subject)) %>% rename('2dprime'= '2d.', '3dprime'='3d.', 'nback_score' = 'score')

#read in spindle information 
YASA_spindles_wider = read.csv("YASA_spindle_count_summary_extended.csv")
YASA_spindles_wider = YASA_spindles_wider %>% mutate(Subject = as.character(Subject))

#import behavioral data that was scored in python using scripts "perceptual-learning-scoring.ipynb" and "Perceptual_Learning_Scoring_AfterCheck.ipynb" 
#the file data.csv that was exported to ~/Documents/PLPSG_sleep_analysis/PLPSG_behavioral_results/Scored_Final/ by these scripts is the SAME FILE as the file being loaded
behavioral = read.csv("PLPSG_PerceptualLearning_Longer.csv")
behavioral$subject = gsub("subj","",as.character(behavioral$subject))
behavioral = behavioral %>% arrange(subject, block) %>% select(-X) %>% 
  rename("Subject"="subject") %>% mutate(block = as.factor(block))

#now switch working directories to recursively read in information from the hypnograms (sleep statistics)
setwd("~/repos/PL_Sleep_PSG/Sleep_Scoring/sleepscoredfiles/files_organized_for_sleep_summarizing")
filelist = list.files(pattern = "*.txt")

PLPSG_sleep_stats_wider = behavioral %>% 
  select(c(Subject,block,score)) %>%
  pivot_wider(names_from = block,
              values_from = score) %>% 
  mutate(learning = (block3-block1),
         loss = -1*(block4-block3),
         recovery = (block5-block4),
         maintenance = (block6-block5),
         protective = (block6-block3),
         longtermrecovery = (block6 - block4),
         retention = (block6-block1),
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
         latR = "",
         N2andN3 = "",
         perN2andN3 = "") %>%
  relocate(c(condition), .after = Subject)

for (i in 1:length(filelist)){
  #read in the file
  setwd("~/repos/PL_Sleep_PSG/Sleep_Scoring/sleepscoredfiles/files_organized_for_sleep_summarizing")
  file_hypno = read.delim(filelist[i], header = FALSE, col.names = "stage")
  
  #identify when lights off (lights out clock time)
  recording_time_info = read.csv("PLPSG_lightson_lightsoff.csv", header = TRUE)
  
  #extract the subject ID
  PLPSG_sleep_stats_wider$Subject[i] = substr(filelist[i], start = 5, stop = 7)
  
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
  
  #total time in minutes that subjects spent in N2 and N3
  PLPSG_sleep_stats_wider$N2andN3[i] = N2 + N3
  
  #percentage of time that subjects spent in N2 and N3
  PLPSG_sleep_stats_wider$perN2andN3[i] = ((N2 + N3)/TST) *100
}

PLPSG_sleep_stats_wider$condition = factor(PLPSG_sleep_stats_wider$condition)

#calculate median of pre-test score 
median_sleep = median(PLPSG_sleep_stats_wider$block1[PLPSG_sleep_stats_wider$condition == "sleep"])
median_wake = median(PLPSG_sleep_stats_wider$block1[PLPSG_sleep_stats_wider$condition == "wake"])

#create column that dictates whether folks are above the median (inclusive) for pre-test score
PLPSG_sleep_stats_wider = PLPSG_sleep_stats_wider %>% mutate(above_median = 
                                                               ifelse((block1 >= median_sleep & condition == "sleep") | 
                                                                        (block1 >= median_wake & condition == "wake"), 
                                                                      "yes", "no"))


# filter out subjects who didn't learn (subjects 134, 141, 142)
PLPSG_sleep_stats_wider = subset(PLPSG_sleep_stats_wider, Subject != '134' & Subject != '141' & Subject != '142')

#merge all dataframes
PLPSG_sleep_stats_wider <- PLPSG_sleep_stats_wider %>%
  left_join(YASA_spindles_wider, by="Subject") %>%
  left_join(demographics, by="Subject") %>%
  left_join(languageexperience, by="Subject") %>%
  left_join(sleepquestionnaire, by="Subject") %>%
  left_join(usualsleeplengths, by="Subject") %>%
  left_join(sleeplogs, by="Subject") %>%
  left_join(stanfordsleepiness, by="Subject") %>%
  left_join(nback, by="Subject")
  

#create a dataset that creates a one column called block
PLPSG_sleep_stats_longer = PLPSG_sleep_stats_wider %>%
  pivot_longer(cols = c("block1", "block3","block4","block5", "block6"),
               names_to = "block",
               values_to = "score")

PLPSG_sleep_stats_longer = PLPSG_sleep_stats_longer %>% 
  mutate(test= ifelse(block == "block1", "PreTest",
                      ifelse(block == "block3", "AfterTrainingPostTest",
                             ifelse(block == "block4", "BeforeSleepPostTest", 
                                    ifelse(block == "block5", "AfterSleepPostTest","NightPostTest"))))) %>%
  relocate(c(block, score), .after = condition)

#export datasets
setwd("~/repos/PL_Sleep_PSG/DataAnalysis/Data/CombinedDataFrames")
write.csv(PLPSG_sleep_stats_wider,"PLPSG_PL_surveys_and_PSG_wider.csv", row.names = FALSE)
write.csv(PLPSG_sleep_stats_longer,"PLPSG_PL_surveys_and_PSG_longer.csv", row.names = FALSE)

#clear environment
#rm(list=ls()[-match(c("PLPSG_sleep_stats_wider", "PLPSG_sleep_stats_longer"), ls())])

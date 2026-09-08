# data cleaning and preparation for EM harmonized dataset received from NASA JSC
# BHP Laboratory on Dec 19 2024
# Author: Mich Lin
# Date created: 19FEB2025 / Date last modified: 20JUN2025

##### housekeeping #####
# load libraries
library("readxl")
library(ggplot2)
library(tibble)
library(dplyr)
library("ggpubr")
library(reticulate)
library(tidyr)

# set color palette for ggplot (colorblind accessible)
cbPalette <- c("#999999","#E69F00","#56B4E9","#009E73",
               "#F0E442","#0072B2","#D55E00","#CC79A7")


##### data import ##### 
# set working directory; change to own directory if working from a different machine
setwd("/Users/michellelin/MIT Dropbox/Mich Lin/Research/behavioral_health/hera_nek/shaq_code/data")
SHAQ_Home <- read_excel("HFBP-EM_SHAQ_Home.xlsx",sheet = 3) 
SHAQ_Hotel <- read_excel("HFBP-EM_SHAQ_Hotel.xlsx",sheet = 3) 
SHAQ_HERA <- read_excel("HFBP-EM_SHAQ_Standard.xlsx",sheet = 3) 
POMS_C4_C5 <- read_excel("HFBP-EM_POMS_Item_Level.xlsx",sheet = 3)
POMS_C6 <-read_excel("HFBP-EM_HERA_C6_POMS.xlsx",sheet = 3)

# rename for consistency; from RC_SHAQ_Stress_How to SHAQ_Stress_How
SHAQ_Home <- rename(SHAQ_Home, "SHAQ_Stress_How" = "RC_SHAQ_Stress_How")
SHAQ_Hotel <- rename(SHAQ_Hotel, "SHAQ_Stress_How" = "RC_SHAQ_Stress_How")
SHAQ_HERA <- rename(SHAQ_HERA, "SHAQ_Stress_How" = "RC_SHAQ_Stress_How")

##### data preparation ##### 
# make categorical variables into factors
SHAQ_Home$Campaign <- as.factor(SHAQ_Home$Campaign)
SHAQ_Home$Mission <- as.factor(SHAQ_Home$Mission)
SHAQ_Home$ID <- as.factor(SHAQ_Home$ID)
SHAQ_Home$SHAQ_Hab_Area <- as.factor(SHAQ_Home$SHAQ_Hab_Area)
SHAQ_Home$Role <- as.factor(SHAQ_Home$Role)

# keeping relevant variables
SHAQ_Home <- SHAQ_Home %>% select(Campaign, Mission, ID, Role, MissionDay, SHAQ_Hab_Area,
                         SHAQ_IndivPerf_How, SHAQ_IndivPerf_Why_1, SHAQ_IndivPerf_Why_2, SHAQ_IndivPerf_Why_3, SHAQ_IndivPerf_Why_4,SHAQ_IndivPerf_Why_5,SHAQ_IndivPerf_Why_6,
                         SHAQ_TeamPerf_How,SHAQ_TeamPerf_Why_1, SHAQ_TeamPerf_Why_2, SHAQ_TeamPerf_Why_3,SHAQ_TeamPerf_Why_4,SHAQ_TeamPerf_Why_5,SHAQ_TeamPerf_Why_6,
                         SHAQ_Mood_How,SHAQ_Mood_Why_1,SHAQ_Mood_Why_2,SHAQ_Mood_Why_3,SHAQ_Mood_Why_4,SHAQ_Mood_Why_5,SHAQ_Mood_Why_6,
                         SHAQ_Stress_How,SHAQ_Stress_Why_1,SHAQ_Stress_Why_2,SHAQ_Stress_Why_3,SHAQ_Stress_Why_4,SHAQ_Stress_Why_5,SHAQ_Stress_Why_6,
                         SHAQ_Sleep_How,SHAQ_Sleep_Why_1,SHAQ_Sleep_Why_2,SHAQ_Sleep_Why_3,SHAQ_Sleep_Why_4,SHAQ_Sleep_Why_5,SHAQ_Sleep_Why_6,
                         SHAQ_Social_How,SHAQ_Social_Why_1,SHAQ_Social_Why_2,SHAQ_Social_Why_3,SHAQ_Social_Why_4,SHAQ_Social_Why_5,SHAQ_Social_Why_6)
# create a row for C5M1 ID 9107 Hab area 2 (manual fix for missing participant)
SHAQ_Home <- rbind(SHAQ_Home, NA)
SHAQ_Home[nrow(SHAQ_Home),1] <- "5"; SHAQ_Home[nrow(SHAQ_Home),2] <- "1";
SHAQ_Home[nrow(SHAQ_Home),3] <- "9107"; SHAQ_Home[nrow(SHAQ_Home),4] <- "MS2"
SHAQ_Home[nrow(SHAQ_Home),5] <- -16; SHAQ_Home[nrow(SHAQ_Home),6] <- "2";

# make categorical variables into factors
SHAQ_Hotel$Campaign <- as.factor(SHAQ_Hotel$Campaign)
SHAQ_Hotel$Mission <- as.factor(SHAQ_Hotel$Mission)
SHAQ_Hotel$ID <- as.factor(SHAQ_Hotel$ID)
SHAQ_Hotel$SHAQ_Hab_Area <- as.factor(SHAQ_Hotel$SHAQ_Hab_Area)
SHAQ_Hotel$Role <- as.factor(SHAQ_Hotel$Role)

# keeping relevant variables
SHAQ_Hotel <- SHAQ_Hotel %>% select(Campaign, Mission, ID, Role, MissionDay, SHAQ_Hab_Area,
                                    SHAQ_IndivPerf_How, SHAQ_IndivPerf_Why_1, SHAQ_IndivPerf_Why_2, SHAQ_IndivPerf_Why_3, SHAQ_IndivPerf_Why_4,SHAQ_IndivPerf_Why_5,SHAQ_IndivPerf_Why_6,
                                    SHAQ_TeamPerf_How,SHAQ_TeamPerf_Why_1, SHAQ_TeamPerf_Why_2, SHAQ_TeamPerf_Why_3,SHAQ_TeamPerf_Why_4,SHAQ_TeamPerf_Why_5,SHAQ_TeamPerf_Why_6,
                                    SHAQ_Mood_How,SHAQ_Mood_Why_1,SHAQ_Mood_Why_2,SHAQ_Mood_Why_3,SHAQ_Mood_Why_4,SHAQ_Mood_Why_5,SHAQ_Mood_Why_6,
                                    SHAQ_Stress_How,SHAQ_Stress_Why_1,SHAQ_Stress_Why_2,SHAQ_Stress_Why_3,SHAQ_Stress_Why_4,SHAQ_Stress_Why_5,SHAQ_Stress_Why_6,
                                    SHAQ_Sleep_How,SHAQ_Sleep_Why_1,SHAQ_Sleep_Why_2,SHAQ_Sleep_Why_3,SHAQ_Sleep_Why_4,SHAQ_Sleep_Why_5,SHAQ_Sleep_Why_6,
                                    SHAQ_Social_How,SHAQ_Social_Why_1,SHAQ_Social_Why_2,SHAQ_Social_Why_3,SHAQ_Social_Why_4,SHAQ_Social_Why_5,SHAQ_Social_Why_6)
# keeping the pre-mission hotel values
SHAQ_Hotel <- subset(SHAQ_Hotel,MissionDay == -1)

# make categorical variables into factors
SHAQ_HERA$Campaign <- as.factor(SHAQ_HERA$Campaign)
SHAQ_HERA$Mission <- as.factor(SHAQ_HERA$Mission)
SHAQ_HERA$ID <- as.factor(SHAQ_HERA$ID)
SHAQ_HERA$SHAQ_Hab_Area <- as.factor(SHAQ_HERA$SHAQ_Hab_Area)
SHAQ_HERA$Role <- as.factor(SHAQ_HERA$Role)

# keeping relevant variables
SHAQ_HERA <- SHAQ_HERA %>% select(Campaign, Mission, ID, Role, MissionDay, SHAQ_Hab_Area,
                                  SHAQ_IndivPerf_How, SHAQ_IndivPerf_Why_1, SHAQ_IndivPerf_Why_2, SHAQ_IndivPerf_Why_3, SHAQ_IndivPerf_Why_4,SHAQ_IndivPerf_Why_5,SHAQ_IndivPerf_Why_6,
                                  SHAQ_TeamPerf_How,SHAQ_TeamPerf_Why_1, SHAQ_TeamPerf_Why_2, SHAQ_TeamPerf_Why_3,SHAQ_TeamPerf_Why_4,SHAQ_TeamPerf_Why_5,SHAQ_TeamPerf_Why_6,
                                  SHAQ_Mood_How,SHAQ_Mood_Why_1,SHAQ_Mood_Why_2,SHAQ_Mood_Why_3,SHAQ_Mood_Why_4,SHAQ_Mood_Why_5,SHAQ_Mood_Why_6,
                                  SHAQ_Stress_How,SHAQ_Stress_Why_1,SHAQ_Stress_Why_2,SHAQ_Stress_Why_3,SHAQ_Stress_Why_4,SHAQ_Stress_Why_5,SHAQ_Stress_Why_6,
                                  SHAQ_Sleep_How,SHAQ_Sleep_Why_1,SHAQ_Sleep_Why_2,SHAQ_Sleep_Why_3,SHAQ_Sleep_Why_4,SHAQ_Sleep_Why_5,SHAQ_Sleep_Why_6,
                                  SHAQ_Social_How,SHAQ_Social_Why_1,SHAQ_Social_Why_2,SHAQ_Social_Why_3,SHAQ_Social_Why_4,SHAQ_Social_Why_5,SHAQ_Social_Why_6)

# make categorical variables into factors
POMS_C4_C5$Campaign <- as.factor(POMS_C4_C5$Campaign)
POMS_C4_C5$Mission <- as.factor(POMS_C4_C5$Mission)
POMS_C4_C5$ID <- as.factor(POMS_C4_C5$ID)
POMS_C4_C5$Role <- as.factor(POMS_C4_C5$Role)

POMS_C6$Campaign <- as.factor(POMS_C6$Campaign)
POMS_C6$Mission <- as.factor(POMS_C6$Mission)
POMS_C6$ID <- as.factor(POMS_C6$ID)
POMS_C6$Role <- as.factor(POMS_C6$Role)

# keeping relevant variables
POMS_C4_C5 <- POMS_C4_C5 %>% select(Campaign,Mission, ID, Role, MissionDay, POMS_TensionAnxiety_Sum,
                        POMS_DepressionDejection_Sum,POMS_AngerHostility_Sum,POMS_FatigueInertia_Sum,
                        POMS_VigorActivity_Sum,POMS_ConfusionBewilderment_Sum,POMS_TotalMoodDist_Sum)
POMS_C6 <- POMS_C6  %>% select(Campaign,Mission, ID, Role, MissionDay, POMS_TensionAnxiety_Sum,
                               POMS_DepressionDejection_Sum,POMS_AngerHostility_Sum,POMS_FatigueInertia_Sum,
                               POMS_VigorActivity_Sum,POMS_ConfusionBewilderment_Sum,POMS_TotalMoodDist_Sum)

# bind C5, C6 into one variable
POMS <- rbind(POMS_C4_C5,POMS_C6)

# removing backups/withdrawn CM
POMS <- POMS %>% subset(Role != "BU1" & Role != "BU2" & # remove backups
                          Role != "WithdrawnMS2" & Role != "WithdrawnFE" & Role != "WithdrawnCDR") # remove withdrawn CM

# saved as rawdata.Rdata

##### interpolate POMS #####
# averaging function to take in initial and final MD, averaging across values for these days
POMS_avg <- function(data, MD_target, MD_i, MD_f){
  subset(data, MissionDay >= MD_i & MissionDay <= MD_f) %>%
    group_by(Campaign, Mission, ID, Role) %>%
    summarize(MissionDay = MD_target, 
              # na.rm = TRUE
              Anxiety_mean = mean(POMS_TensionAnxiety_Sum),
              Depression_mean = mean(POMS_DepressionDejection_Sum),
              Anger_mean = mean(POMS_AngerHostility_Sum),
              Fatigue_mean = mean(POMS_FatigueInertia_Sum),
              Vigor_mean = mean(POMS_VigorActivity_Sum),
              Confusion_mean = mean(POMS_ConfusionBewilderment_Sum),
              MoodTotal_mean = mean(POMS_TotalMoodDist_Sum))
}

# interpolate POMS values to be the same MD that SHAQ were administered
POMS_MDt1 <- POMS_avg(POMS, -16, -16, -1)
POMS_MD4 <- POMS_avg(POMS, 4, 1, 6)
POMS_MD9 <- POMS_avg(POMS, 9, 7, 12)
POMS_MD16 <- POMS_avg(POMS, 16, 13, 19)
POMS_MD23 <- POMS_avg(POMS, 23, 20, 26)
POMS_MD30 <- POMS_avg(POMS, 30, 27, 33)
POMS_MD37 <- POMS_avg(POMS, 37, 34, 40)
POMS_MD44 <- POMS_avg(POMS, 44, 41, 45)
POMS_interp <- rbind(POMS_MDt1, POMS_MD4, POMS_MD9, POMS_MD16, POMS_MD23, 
                     POMS_MD30, POMS_MD37, POMS_MD44) # bind into one df

# save as workdata.RData

##### create different configurations of the data #####
rm(list = ls())

# import rawdata.RData
load("~/MIT Dropbox/Mich Lin/Research/behavioral_health/hera_nek/shaq_code/data/rawdata.RData")

# areas
# 1 sleep
# 2 hygiene
# 3 work
# 4 kitchen

# 'why's
# 1 - privacy; 2 - social density; 3 - efficiency
# 4 - control; 5 - comfort; 6 - convenience

# add habitat labels to home/hotel/HERA
SHAQ_Home <- SHAQ_Home %>% mutate(Habitat = "Home")
SHAQ_Hotel <- SHAQ_Hotel %>% mutate(Habitat = "Hotel")
SHAQ_HERA<- SHAQ_HERA %>% mutate(Habitat = "HERA")

# pivot each dataframe longer
# then wider to arrange by a column of BHP outcome & why's as independent columns
SHAQ_Home <- SHAQ_Home %>% pivot_longer(cols = SHAQ_IndivPerf_How:SHAQ_Social_Why_6, 
                                        names_to = c("BHP_Outcome","Rating"), 
                                        names_pattern = "SHAQ_([^_]+)_(.*)", 
                                        values_to = "Value")  %>% 
  pivot_wider(names_from = Rating, values_from = Value)

SHAQ_Hotel <- SHAQ_Hotel %>% pivot_longer(cols = SHAQ_IndivPerf_How:SHAQ_Social_Why_6, 
                                          names_to = c("BHP_Outcome","Rating"), 
                                          names_pattern = "SHAQ_([^_]+)_(.*)", 
                                          values_to = "Value")  %>% 
  pivot_wider(names_from = Rating, values_from = Value) 

# hard code here
SHAQ_HERA <- SHAQ_HERA %>% 
  # problem with a CM who took the survey twice; keeping the first instance of the survey
  # could figure out how to average the two entries but it might take a while
  # use distinct to resolve non-uniqueness issue
  distinct(Campaign, Mission, ID, Role, MissionDay, SHAQ_Hab_Area, .keep_all = TRUE) %>% 
  pivot_longer(cols = SHAQ_IndivPerf_How:SHAQ_Social_Why_6, 
               names_to = c("BHP_Outcome","Rating"), 
               names_pattern = "SHAQ_([^_]+)_(.*)", 
               values_to = "Value") %>% 
  pivot_wider(names_from = Rating, values_from = Value) 

# save as wide.RData

##### missing data analysis ##### 
# count percentage of missing data overall
for (i in 1:4) {
  BHP <- "Social" # change out BHP parameter here (IndivPerf, TeamPerf, Stress, Mood, Sleep, Social)
   c <- SHAQ_HERA %>% # change out data source here (SHAQ_Home, SHAQ_Hotel, SHAQ_HERA)
    subset(SHAQ_Hab_Area == i & BHP_Outcome == BHP) %>% 
    summarise(count=sum(is.na(.)))/1561   # divide by 224 for Home/Hotel, by 1561 for HERA

   
   print(c)
   
   # count unique crewmembers remain if missing data is removed
   n <- SHAQ_HERA %>% 
     subset(SHAQ_Hab_Area == i & BHP_Outcome == BHP) %>% 
     na.omit() %>%
     summarise(n_distinct(ID))
   print(n)
}
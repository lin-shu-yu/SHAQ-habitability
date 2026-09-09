# data cleaning and preparation for EM harmonized dataset received from NASA JSC
# BHP Laboratory on Dec 19 2024
# Author: Mich Lin
# Date created: 19FEB2025 / Date last modified: 09SEP2026

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
# set working directory to where the data files are using setwd()
# if you do not have the raw excel sheet files, skip this code file

SHAQ_Home <- read_excel("HFBP-EM_SHAQ_Home.xlsx",sheet = 3) 
SHAQ_Hotel <- read_excel("HFBP-EM_SHAQ_Hotel.xlsx",sheet = 3) 
SHAQ_HERA <- read_excel("HFBP-EM_SHAQ_Standard.xlsx",sheet = 3) 
POMS_C4_C5 <- read_excel("HFBP-EM_POMS_Item_Level.xlsx",sheet = 3)
POMS_C6 <-read_excel("HFBP-EM_HERA_C6_POMS.xlsx",sheet = 3)

# rename for consistency; from RC_SHAQ_Stress_How to SHAQ_Stress_How
SHAQ_Home <- rename(SHAQ_Home, "SHAQ_Stress_How" = "RC_SHAQ_Stress_How")
SHAQ_Hotel <- rename(SHAQ_Hotel, "SHAQ_Stress_How" = "RC_SHAQ_Stress_How")
SHAQ_HERA <- rename(SHAQ_HERA, "SHAQ_Stress_How" = "RC_SHAQ_Stress_How")

# count missing cells
POMS_C4_C5 %>% 
  # subset(MissionPhase_Nom == "Pre-Mission") %>% 
  # subset(MissionPhase_Nom == "In-Mission") %>% 
  # subset(MissionPhase_Nom == "Post-Mission") %>% 
  select(!c(Analog, Study, Campaign, Mission, ID_Crew, Crew_Count, ID, Role, Crew_Expedition,
            Recorded_Date,Response_ID,Survey_Duration,MissionDay_Nom,
            MissionDay_Abs, MissionWeek, MissionPhase_Abs, MissionPhase_Nom,
            MissionDay,MissionDay_Pct,MissionDay_CrewPct,
            POMS_AngerHostility_Sum, POMS_TensionAnxiety_Sum, POMS_DepressionDejection_Sum,
            POMS_FatigueInertia_Sum, POMS_VigorActivity_Sum, POMS_ConfusionBewilderment_Sum,
            POMS_TotalMoodDist_Sum)) %>%
  summarise(count=sum(is.na(.)))

# get overall df dimension
POMS_C4_C5 %>% 
  # subset(MissionPhase_Nom == "Pre-Mission") %>% 
  # subset(MissionPhase_Nom == "In-Mission") %>% 
  # subset(MissionPhase_Nom == "Post-Mission") %>% 
  select(!c(Analog, Study, Campaign, Mission, ID_Crew, Crew_Count, ID, Role,Crew_Expedition,
            Recorded_Date,Response_ID,Survey_Duration,MissionDay_Nom,
            MissionDay_Abs, MissionWeek, MissionPhase_Abs, MissionPhase_Nom,
            MissionDay,MissionDay_Pct,MissionDay_CrewPct,
            POMS_AngerHostility_Sum, POMS_TensionAnxiety_Sum, POMS_DepressionDejection_Sum,
            POMS_FatigueInertia_Sum, POMS_VigorActivity_Sum, POMS_ConfusionBewilderment_Sum,
            POMS_TotalMoodDist_Sum)) %>%
  {prod(dim(.))}

# count missing cells
POMS_C6 %>% 
  # subset(MissionPhase_Nom == "Pre-Mission") %>% 
  # subset(MissionPhase_Nom == "In-Mission") %>% 
  # subset(MissionPhase_Nom == "Post-Mission") %>% 
  select(!c(Analog, Study, Campaign, Mission, ID_Crew, Crew_Count, ID, Role,
            Recorded_Date,Response_ID,Survey_Duration,MissionDay_Nom,
            MissionPhase_Nom, MissionDay,MissionDay_Pct, MissionDay_Crew, MissionDay_CrewPct,
            POMS_AngerHostility_Sum, POMS_TensionAnxiety_Sum, POMS_DepressionDejection_Sum,
            POMS_FatigueInertia_Sum, POMS_VigorActivity_Sum, POMS_ConfusionBewilderment_Sum,
            POMS_TotalMoodDist_Sum)) %>%
  summarise(count=sum(is.na(.)))

# get overall df dimension
POMS_C6 %>% 
  # subset(MissionPhase_Nom == "Pre-Mission") %>% 
  # subset(MissionPhase_Nom == "In-Mission") %>% 
  # subset(MissionPhase_Nom == "Post-Mission") %>% 
  select(!c(Analog, Study, Campaign, Mission, ID_Crew, Crew_Count, ID, Role,
            Recorded_Date,Response_ID,Survey_Duration,MissionDay_Nom,
            MissionPhase_Nom, MissionDay,MissionDay_Pct, MissionDay_Crew, MissionDay_CrewPct,
            POMS_AngerHostility_Sum, POMS_TensionAnxiety_Sum, POMS_DepressionDejection_Sum,
            POMS_FatigueInertia_Sum, POMS_VigorActivity_Sum, POMS_ConfusionBewilderment_Sum,
            POMS_TotalMoodDist_Sum)) %>%
  {prod(dim(.))}

# pre-mission (0.000220535)
# in mission (0.002510898)
# post mission (0.0002782673)
# POMS missing data as a percentage of raw responses: 0.001758112 or 0.18%

# SHAQ missing data
BHP <- c("SHAQ_IndivPerf", "SHAQ_TeamPerf", "SHAQ_Stress", "SHAQ_Mood", 
         "SHAQ_Sleep", "SHAQ_Social")

# create empty df's for storing values
shaq_home_missing <- data.frame(matrix(nrow = 4, ncol = 6))
colnames(shaq_home_missing) <- c("IndivPerf", "TeamPerf", "Stress", "Mood", 
                                 "Sleep", "Social")
rownames(shaq_home_missing) <- c("Sleep","Hygiene","Work","Galley")

# SHAQ Home
for (i in 1:4) {
  for (j in 1:6) {
    # count missing data
    c <- SHAQ_Home %>% subset(SHAQ_Hab_Area == i) %>%
      select(!c(Analog, Study, Campaign, Mission, ID_Crew, Crew_Count, ID, Role, Crew_Expedition,
                Recorded_Date,Response_ID,Survey_Duration,MissionDay_Nom,
                MissionPhase_Nom, MissionDay,MissionDay_Pct, MissionDay_Crew, MissionDay_CrewPct,
                SHAQ_Mission_Phase, SHAQ_Type, SHAQ_Type_Other, SHAQ_Dur_Mnths, SHAQ_Dur_Yrs,
                SHAQ_OwnRent, SHAQ_OwnRent_Other, SHAQ_Occ, SHAQ_Pets_Cats, SHAQ_Pets_Dogs,
                SHAQ_Pets_Other1, SHAQ_Pets_Other1_Amt, SHAQ_Pets_Other2, SHAQ_Pets_Other2_Amt,
                SHAQ_Pets_Other3, SHAQ_Pets_Other3_Amt, SHAQ_Bed_Width_Ft, SHAQ_Bed_Width_In,
                SHAQ_Bed_Length_Ft, SHAQ_Bed_Length_In, SHAQ_Bath_Length_Ft, SHAQ_Bath_Length_In,
                SHAQ_Bath_Width_Ft, SHAQ_Bath_Width_In, SHAQ_Work_Length_Ft, SHAQ_Work_Length_In, 
                SHAQ_Work_Width_Ft, SHAQ_Work_Width_In, SHAQ_Kit_Length_Ft, SHAQ_Kit_Length_In,
                SHAQ_Kit_Width_Ft, SHAQ_Kit_Width_In, SHAQ_Rec_Length_Ft, SHAQ_Rec_Length_In, 
                SHAQ_Rec_Width_Ft, SHAQ_Rec_Width_In, SHAQ_Measure_Method, SHAQ_Measure_Method_Cmt,
                SHAQ_Size, SHAQ_Size_Cmt, ends_with("Sqft"), Home_Duration_Yrs, Pet_Quantity,
                SHAQ_Hab_Area, ends_with("_Cmt"))) %>%
      select(contains(BHP[j])) %>% 
      summarise(count=sum(is.na(.)))
    
    # df dimension
    d <- SHAQ_Home %>% subset(SHAQ_Hab_Area == i) %>%
      select(!c(Analog, Study, Campaign, Mission, ID_Crew, Crew_Count, ID, Role, Crew_Expedition,
                Recorded_Date,Response_ID,Survey_Duration,MissionDay_Nom,
                MissionPhase_Nom, MissionDay,MissionDay_Pct, MissionDay_Crew, MissionDay_CrewPct,
                SHAQ_Mission_Phase, SHAQ_Type, SHAQ_Type_Other, SHAQ_Dur_Mnths, SHAQ_Dur_Yrs,
                SHAQ_OwnRent, SHAQ_OwnRent_Other, SHAQ_Occ, SHAQ_Pets_Cats, SHAQ_Pets_Dogs,
                SHAQ_Pets_Other1, SHAQ_Pets_Other1_Amt, SHAQ_Pets_Other2, SHAQ_Pets_Other2_Amt,
                SHAQ_Pets_Other3, SHAQ_Pets_Other3_Amt, SHAQ_Bed_Width_Ft, SHAQ_Bed_Width_In,
                SHAQ_Bed_Length_Ft, SHAQ_Bed_Length_In, SHAQ_Bath_Length_Ft, SHAQ_Bath_Length_In,
                SHAQ_Bath_Width_Ft, SHAQ_Bath_Width_In, SHAQ_Work_Length_Ft, SHAQ_Work_Length_In, 
                SHAQ_Work_Width_Ft, SHAQ_Work_Width_In, SHAQ_Kit_Length_Ft, SHAQ_Kit_Length_In,
                SHAQ_Kit_Width_Ft, SHAQ_Kit_Width_In, SHAQ_Rec_Length_Ft, SHAQ_Rec_Length_In, 
                SHAQ_Rec_Width_Ft, SHAQ_Rec_Width_In, SHAQ_Measure_Method, SHAQ_Measure_Method_Cmt,
                SHAQ_Size, SHAQ_Size_Cmt, ends_with("Sqft"), Home_Duration_Yrs, Pet_Quantity,
                SHAQ_Hab_Area, ends_with("_Cmt"))) %>%
      select(contains(BHP[j])) %>% 
      {prod(dim(.))}
    
    # calculate percentage
    shaq_home_missing[i,j] <- c/d
  }
}

# SHAQ Hotel
shaq_hotel_missing <- data.frame(matrix(nrow = 4, ncol = 6))
colnames(shaq_hotel_missing) <- c("IndivPerf", "TeamPerf", "Stress", "Mood", 
                                  "Sleep", "Social")
rownames(shaq_hotel_missing) <- c("Sleep","Hygiene","Work","Galley")

for (i in 1:4) {
  for (j in 1:6) {
    # count missing data
    c <- SHAQ_Hotel %>% subset(SHAQ_Hab_Area == i) %>%
      select(!c(Analog, Study, Campaign, Mission, ID_Crew, Crew_Count, ID, Role, Crew_Expedition,
                Recorded_Date,Response_ID,Survey_Duration,MissionDay_Nom,
                MissionPhase_Nom, MissionDay,MissionDay_Pct, MissionDay_Crew, MissionDay_CrewPct,
                SHAQ_Mission_Phase, SHAQHotel_Dim_YN, SHAQ_Type, SHAQ_Type_Other, SHAQ_Dur_Mnths, SHAQ_Dur_Yrs,
                SHAQ_OwnRent, SHAQ_OwnRent_Other, SHAQ_Occ, SHAQ_Pets_Cats, SHAQ_Pets_Dogs,
                SHAQ_Pets_Other1, SHAQ_Pets_Other1_Amt, SHAQ_Pets_Other2, SHAQ_Pets_Other2_Amt,
                SHAQ_Pets_Other3, SHAQ_Pets_Other3_Amt, SHAQ_Bed_Width_Ft, SHAQ_Bed_Width_In,
                SHAQ_Bed_Length_Ft, SHAQ_Bed_Length_In, SHAQ_Bath_Length_Ft, SHAQ_Bath_Length_In,
                SHAQ_Bath_Width_Ft, SHAQ_Bath_Width_In, SHAQ_Work_Length_Ft, SHAQ_Work_Length_In, 
                SHAQ_Work_Width_Ft, SHAQ_Work_Width_In, SHAQ_Kit_Length_Ft, SHAQ_Kit_Length_In,
                SHAQ_Kit_Width_Ft, SHAQ_Kit_Width_In, SHAQ_Rec_Length_Ft, SHAQ_Rec_Length_In, 
                SHAQ_Rec_Width_Ft, SHAQ_Rec_Width_In, SHAQ_Measure_Method, SHAQ_Measure_Method_Cmt,
                SHAQHotel_Size, ends_with("Sqft"), Home_Duration_Yrs, Pet_Quantity,
                SHAQ_Hab_Area, ends_with("_Cmt"))) %>%
      select(contains(BHP[j])) %>% 
      summarise(count=sum(is.na(.)))
    
    # df dimension
    d <- SHAQ_Hotel %>% subset(SHAQ_Hab_Area == i) %>%
      select(!c(Analog, Study, Campaign, Mission, ID_Crew, Crew_Count, ID, Role, Crew_Expedition,
                Recorded_Date,Response_ID,Survey_Duration,MissionDay_Nom,
                MissionPhase_Nom, MissionDay,MissionDay_Pct, MissionDay_Crew, MissionDay_CrewPct,
                SHAQ_Mission_Phase, SHAQHotel_Dim_YN, SHAQ_Type, SHAQ_Type_Other, SHAQ_Dur_Mnths, SHAQ_Dur_Yrs,
                SHAQ_OwnRent, SHAQ_OwnRent_Other, SHAQ_Occ, SHAQ_Pets_Cats, SHAQ_Pets_Dogs,
                SHAQ_Pets_Other1, SHAQ_Pets_Other1_Amt, SHAQ_Pets_Other2, SHAQ_Pets_Other2_Amt,
                SHAQ_Pets_Other3, SHAQ_Pets_Other3_Amt, SHAQ_Bed_Width_Ft, SHAQ_Bed_Width_In,
                SHAQ_Bed_Length_Ft, SHAQ_Bed_Length_In, SHAQ_Bath_Length_Ft, SHAQ_Bath_Length_In,
                SHAQ_Bath_Width_Ft, SHAQ_Bath_Width_In, SHAQ_Work_Length_Ft, SHAQ_Work_Length_In, 
                SHAQ_Work_Width_Ft, SHAQ_Work_Width_In, SHAQ_Kit_Length_Ft, SHAQ_Kit_Length_In,
                SHAQ_Kit_Width_Ft, SHAQ_Kit_Width_In, SHAQ_Rec_Length_Ft, SHAQ_Rec_Length_In, 
                SHAQ_Rec_Width_Ft, SHAQ_Rec_Width_In, SHAQ_Measure_Method, SHAQ_Measure_Method_Cmt,
                SHAQHotel_Size, ends_with("Sqft"), Home_Duration_Yrs, Pet_Quantity,
                SHAQ_Hab_Area, ends_with("_Cmt"))) %>%
      select(contains(BHP[j])) %>% 
      {prod(dim(.))}
    
    # calculate percentage
    shaq_hotel_missing[i,j] <- c/d
  }
}

# SHAQ HERA
shaq_hera_missing <- data.frame(matrix(nrow = 4, ncol = 6))
colnames(shaq_hera_missing) <- c("IndivPerf", "TeamPerf", "Stress", "Mood", 
                                 "Sleep", "Social")
rownames(shaq_hera_missing) <- c("Sleep","Hygiene","Work","Galley")

for (i in 1:4) {
  for (j in 1:6) {
    # count missing data
    c <- SHAQ_HERA %>% subset(SHAQ_Hab_Area == i) %>%
      select(!c(Analog, Study, Campaign, Mission, ID_Crew, Crew_Count, ID, Role, Crew_Expedition,
                Recorded_Date,Response_ID,Survey_Duration,MissionDay_Nom,
                MissionPhase_Nom, MissionDay,MissionDay_Pct, MissionDay_Crew, MissionDay_CrewPct,
                SHAQ_Hab_Area, ends_with("_Cmt"))) %>%
      select(contains(BHP[j])) %>% 
      summarise(count=sum(is.na(.)))
    
    # df dimension
    d <- SHAQ_HERA %>% subset(SHAQ_Hab_Area == i) %>%
      select(!c(Analog, Study, Campaign, Mission, ID_Crew, Crew_Count, ID, Role, Crew_Expedition,
                Recorded_Date,Response_ID,Survey_Duration,MissionDay_Nom,
                MissionPhase_Nom, MissionDay,MissionDay_Pct, MissionDay_Crew, MissionDay_CrewPct,
                SHAQ_Hab_Area, ends_with("_Cmt"))) %>%
      select(contains(BHP[j])) %>% {prod(dim(.))}
    
    # calculate percentage
    shaq_hera_missing[i,j] <- c/d
  }
}

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

save.image("rawdata.RData")

##### create different configurations of the data #####
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

SHAQ_HERA <- SHAQ_HERA %>% 
  distinct(Campaign, Mission, ID, Role, MissionDay, SHAQ_Hab_Area, .keep_all = TRUE) %>% 
  pivot_longer(cols = SHAQ_IndivPerf_How:SHAQ_Social_Why_6, 
               names_to = c("BHP_Outcome","Rating"), 
               names_pattern = "SHAQ_([^_]+)_(.*)", 
               values_to = "Value") %>% 
  pivot_wider(names_from = Rating, values_from = Value) 

save.image("widedata.RData")

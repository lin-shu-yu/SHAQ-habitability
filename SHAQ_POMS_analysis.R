# RQ1: How is habitat-impacted mood related to overall mood? 
# Pearson's correlation analysis of SHAQ/POMS data by habitat area and by habitat
# Author: Mich Lin shuyulin [at] mit [dot] edu
# Date created: 24FEB2025 / Date last modified: 05FEB2026

##### housekeeping #####
# load libraries
library("readxl")
library(ggplot2)
library(tibble)
library(dplyr)
library("ggpubr")
library(reticulate)
library(nlme)
library(tidyr)
library(magrittr)
library(ggridges)

# set color palette for ggplot (colorblind accessible)
cbPalette <- c("#999999","#E69F00","#56B4E9","#009E73",
               "#F0E442","#0072B2","#D55E00","#CC79A7")

#### load data #####
# import workdata.RData
load("~/MIT Dropbox/Mich Lin/Research/behavioral_health/hera_nek/shaq_code/data/workdata.RData")

# add habitat labels
SHAQ_Home <- SHAQ_Home %>% mutate(Habitat = "Home")
SHAQ_Hotel <- SHAQ_Hotel %>% mutate(Habitat = "Hotel")
SHAQ_HERA<- SHAQ_HERA %>% mutate(Habitat = "HERA")

# add habitat labels by mission days (no home values for POMS)
POMS_interp <- POMS_interp %>% mutate(Habitat = case_when(
  MissionDay < 0 ~ "Hotel",
  MissionDay > 0 ~ "HERA"
))

# get POMS on SHAQ administration days (MD 4,9,16,23,30,37,44) and save this as POMS_HERA_select
POMS_HERA_select <- POMS %>% subset(MissionDay == -16 | 
                  MissionDay == 4 |
                  MissionDay == 9 |
                  MissionDay == 16 |
                  MissionDay == 23 |
                  MissionDay == 30 |
                  MissionDay == 37 |
                  MissionDay == 44) %>% select (-Role)

##### visualization #####
# summarize into mood mean/sd, grouping by mission day
SHAQ_summary <- rbind(SHAQ_Home, SHAQ_Hotel, SHAQ_HERA) %>% group_by(MissionDay) %>%
  dplyr::summarize(
    data = "SHAQ",
    mood_mean = mean(SHAQ_Mood_How, na.rm = TRUE),
    mood_sd = sd(SHAQ_Mood_How, na.rm = TRUE),
    stress_mean = mean(SHAQ_Stress_How, na.rm = TRUE),
    stress_sd = sd(SHAQ_Stress_How, na.rm = TRUE)
  )

POMS_summary <- POMS_interp %>% group_by(MissionDay) %>%
  dplyr::summarize(
    data = "POMS",
    mood_mean = mean(MoodTotal_mean, na.rm = TRUE),
    mood_sd = sd(MoodTotal_mean, na.rm = TRUE)
  )

#plots
# SHAQ mood over each mission day, mean + SD error bars
ggplot(SHAQ_summary, aes(x=MissionDay, y = mood_mean))+
  geom_errorbar(aes(ymin=mood_mean-mood_sd, ymax=mood_mean+mood_sd), width=2) +
  xlab("Mission Day") + ylab("Mood") + ggtitle("SHAQ Mood") + ylim(-100,100) + xlim(-17,45) +
  # home bar
  geom_rect(aes(xmin = -17, xmax = -15, ymin = -Inf, ymax = Inf, fill = cbPalette[4]), alpha = 0.05) +
  # hotel bar
  geom_rect(aes(xmin = -15, xmax = 0, ymin = -Inf, ymax = Inf, fill = "#999999"), alpha = 0.05) + 
  # HERA bar
  geom_rect(aes(xmin = 0, xmax = 45, ymin = -Inf, ymax = Inf, fill = cbPalette[2]), alpha = 0.05) +
  geom_point()+
  scale_fill_manual('Habitat',
                    labels = c("Home","Hotel","HERA"),
                    values = c(cbPalette[1],cbPalette[6],cbPalette[7]),  # change colors here
                    guide = guide_legend(override.aes = list(alpha = 1))) 

# POMS TMD over each mission day, mean + SD error bars
ggplot(POMS_summary, aes(x=MissionDay, y = mood_mean))+
  geom_errorbar(aes(ymin=mood_mean-mood_sd, ymax=mood_mean+mood_sd), width=2) +
  geom_point()+
  xlab("Mission Day") + ylab("Total Mood Disturbance") + ggtitle("POMS Total Mood Disturbance")+
  ylim(0,148) + xlim(-17,45)+
  # home bar
  geom_rect(aes(xmin = -17, xmax = -15, ymin = -Inf, ymax = Inf, fill = cbPalette[4]), alpha = 0.05) + 
  # hotel bar
  geom_rect(aes(xmin = -15, xmax = 0, ymin = -Inf, ymax = Inf, fill = cbPalette[3]), alpha = 0.05) + 
  # HERA bar
  geom_rect(aes(xmin = 0, xmax = 45, ymin = -Inf, ymax = Inf, fill = cbPalette[2]), alpha = 0.05) +
  scale_fill_manual('Habitat',
                    labels = c("Home","Hotel","HERA"),
                    values = c(cbPalette[2],cbPalette[3],cbPalette[4]),  
                    guide = guide_legend(override.aes = list(alpha = 1))) 

# plotting mood changes across habitat environments
# SHAQ box + whiskers plot (IQR)
ggplot(rbind(SHAQ_Home, SHAQ_Hotel, SHAQ_HERA), aes(x=Habitat, y = (SHAQ_Mood_How)))+
  stat_boxplot(geom ='errorbar', width = 0.5) +
  geom_boxplot()+
  scale_x_discrete(limits = c('Home','Hotel','HERA'))+
  ylab("SHAQ Mood")+xlab("Environment")+ylim(c(-100,100))+
  ggtitle("SHAQ Mood across Home/Hotel/HERA")
# SHAQ violin plot + jittered scatter data points
ggplot(rbind(SHAQ_Home, SHAQ_Hotel, SHAQ_HERA), aes(x=Habitat, y = SHAQ_Mood_How))+
  geom_violin()+ 
  geom_jitter(shape=20, position=position_jitter(0.2))+ # can comment to turn off data pts
  scale_x_discrete(limits = c('Home','Hotel','HERA'))+
  ylab("SHAQ Mood")+xlab("Environment")+
  ggtitle("SHAQ Mood - spread of data")

# POMS box + whiskers plot (IQR)
ggplot(POMS_interp, aes(x=Habitat, y = MoodTotal_mean))+
  stat_boxplot(geom ='errorbar', width = 0.5) +
  geom_boxplot()+
  scale_x_discrete(limits = c('Home','Hotel','HERA'))+
  ylab("POMS Mood Disturbance")+xlab("Environment")+ylim(c(0,148))+
  ggtitle("POMS Mood Disturbance across Home/Hotel/HERA")
# POMS violin plot + jittered scatter data points
ggplot(POMS_interp, aes(x=Habitat, y = MoodTotal_mean))+
  geom_violin()+ 
  geom_jitter(shape=20, position=position_jitter(0.2))+ # can comment to turn off data pts
  scale_x_discrete(limits = c('Home','Hotel','HERA'))+
  ylab("POMS Mood Disturbance")+xlab("Environment")+
  ggtitle("POMS Mood Disturbance - spread of data")

# bind SHAQ/POMS (all environments) into one df to plot across environments
SHAQ_all <- rbind(SHAQ_Home, SHAQ_Hotel, SHAQ_HERA) %>% 
  select(Campaign, Mission, ID, MissionDay, SHAQ_Mood_How, Habitat)
SHAQ_all$Habitat <- as.factor(SHAQ_all$Habitat)
POMS_all <- POMS %>% mutate(Habitat = case_when(MissionDay < 0 ~ "Hotel", 
                                                MissionDay > 0 ~ "HERA")) %>%
  select(Campaign, Mission, ID, MissionDay, POMS_TotalMoodDist_Sum, Habitat)
SHAQ_POMS_all <- full_join(SHAQ_all, POMS_all, by = c("Campaign","Mission","ID","MissionDay","Habitat"))

# SHAQ ridgeline
# swap out the first two lines if plotting with density as the height
#
# ggplot(SHAQ_POMS_all, aes(height = stat(density)))+ 
# geom_density_ridges(stat = "density", aes(y=Habitat, x=SHAQ_Mood_How), alpha = 0.7, color = "red", fill = "red") + 
ggplot(SHAQ_POMS_all)+  
geom_density_ridges(aes(y=Habitat, x=SHAQ_Mood_How), alpha = 0.7, color = "red", fill = "red") +
  coord_cartesian(clip = "off") + 
  scale_y_discrete(limits = c('HERA','Hotel','Home')) + 
  scale_x_reverse(limits = c(100,-100), breaks = c(100, 0, -100), labels = c("Positive Mood","No Effect","Negative Mood"))

#POMS ridgeline
ggplot(SHAQ_POMS_all, aes(height = stat(density)))+
  geom_density_ridges(aes(y=Habitat, x=POMS_TotalMoodDist_Sum), alpha = 0.7, color = "blue", fill = "blue") +
  coord_cartesian(clip = "off") + 
  scale_x_continuous(limits=c(0,148), breaks = c(0,  37,  74, 111, 148))

# both plots overlaid
# if allowing R to pick the global bandwidth parameter, substitute these next three lines instead
# -> looks more smooth, but density is slightly misrepresented
ggplot(SHAQ_POMS_all)+
  geom_density_ridges(aes(y=Habitat, x= 100 - SHAQ_Mood_How), alpha = 0.6, color = "red", fill = "red") +
  geom_density_ridges(aes(y=Habitat, x=POMS_TotalMoodDist_Sum), alpha = 0.6, color = "blue", fill = "blue") +

# local (group wise) bandwidth parameter 
# -> each group looks proportional to their N
# ggplot(SHAQ_POMS_all, aes(height = stat(density)))+
#   geom_density_ridges(stat = "density",aes(y=Habitat, x= 95 - SHAQ_Mood_How), alpha = 0.6, color = "red", fill = "red") +
#   geom_density_ridges(stat = "density",aes(y=Habitat, x=POMS_TotalMoodDist_Sum), alpha = 0.6, color = "blue", fill = "blue") +
  coord_cartesian(clip = "off") + 
  scale_y_discrete(limits = c('HERA','Hotel','Home')) + 
  scale_x_continuous(limits=c(-5,195), 
                     # breaks = c(),
                     # breaks = c(-5, 0, 95, 190, 195),
                     # labels = c("", "", "" ,"", "")
                     ) + 
  ylab("Habitat Environment")+xlab("")

# more on bandwidth parameter here: https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html


##### reconstruction dataframes for comparison #####
# reconstruct dataframes
# "How" only, no "Why's"
SHAQ_HERA_short <- SHAQ_HERA %>% 
    select(Campaign, Mission, ID, MissionDay, SHAQ_Hab_Area,
           SHAQ_IndivPerf_How, SHAQ_TeamPerf_How, SHAQ_Mood_How, SHAQ_Stress_How,
           SHAQ_Sleep_How, SHAQ_Social_How) %>% 
  rename(iperf = SHAQ_IndivPerf_How, teamperf = SHAQ_TeamPerf_How, mood = SHAQ_Mood_How, 
         stress = SHAQ_Stress_How, sleep = SHAQ_Sleep_How, social = SHAQ_Social_How)

# rename variables
POMS_interp <- POMS_interp %>% rename(anxiety = Anxiety_mean,
                               depression = Depression_mean,
                               anger = Anger_mean,
                               fatigue = Fatigue_mean,
                               vigor = Vigor_mean,
                               confusion = Confusion_mean,
                               mood_dist = MoodTotal_mean)

# checks for normality

# syntax: qqnorm(x, ylim, main, xlab, ylab, plot.it, datax, ...)
# SHAQ
qqnorm(SHAQ_HERA_short$mood, main = "SHAQ Mood")
qqline(SHAQ_HERA_short$mood)

qqnorm(SHAQ_HERA_short$stress, main = "SHAQ Stress")
qqline(SHAQ_HERA_short$stress)

qqnorm(SHAQ_HERA_short$iperf, main = "SHAQ Ind. Perf.")
qqline(SHAQ_HERA_short$iperf)

qqnorm(SHAQ_HERA_short$teamperf, main = "SHAQ Team Perf.")
qqline(SHAQ_HERA_short$teamperf)

qqnorm(SHAQ_HERA_short$social, main = "SHAQ Social")
qqline(SHAQ_HERA_short$social)

qqnorm(SHAQ_HERA_short$sleep, main = "SHAQ Sleep")
qqline(SHAQ_HERA_short$sleep)

# POMS
qqnorm(POMS_interp$anxiety, main = "POMS Tension-Anxiety")
qqline(POMS_interp$anxiety)

qqnorm(POMS_interp$depression, main = "POMS Depression-Dejection")
qqline(POMS_interp$depression)

qqnorm(POMS_interp$anger, main = "POMS Anger-Hostility")
qqline(POMS_interp$anger)

qqnorm(POMS_interp$fatigue, main = "POMS Fatigue-Inertia")
qqline(POMS_interp$fatigue)

qqnorm(POMS_interp$vigor, main = "POMS Vigor-Activity")
qqline(POMS_interp$vigor)

qqnorm(POMS_interp$confusion, main = "POMS Confusion-Bewilderment")
qqline(POMS_interp$confusion)

qqnorm(POMS_interp$mood_dist, main = "POMS Total Mood Disturbance")
qqline(POMS_interp$mood_dist)

##### repeated measures correlations by habitat area & overall #####
# 1 sleep
# 2 hygiene
# 3 work
# 4 kitchen

# construct combined dataframe for SHAQ and POMS to have shared hab area values
SHAQ_POMS_byarea <- left_join(SHAQ_HERA_short, POMS_HERA_select, by = c("Campaign","Mission","ID","MissionDay"))

# create empty correlation df to fill in
# for each habitat area; 4 areas by 2 values (correlation coeff + p value)
corr_SHAQ_POMS <- data.frame(matrix(ncol = 5, nrow = 2))
names(corr_SHAQ_POMS) <- c("Sleep","Hygiene","Work","Kitchen","Overall") # col names
rownames(corr_SHAQ_POMS) <- c("corr coeff", "p-value")


# using baselined data for SHAQ and POMS
# combining C5 + C6
for (i in 1:4){
  res <- rmcorr(data = subset(SHAQ_POMS_byarea, SHAQ_Hab_Area == i),
                measure1 = mood, 
                measure2 = POMS_TotalMoodDist_Sum,
                participant = factor(ID))
  
  corr_SHAQ_POMS[1,i] <- res$r
  corr_SHAQ_POMS[2,i] <- res$p
}


# pearson's correlations across habitat
# build SHAQ POMS dataframe with SHAQ areas collapsed to a habitat mean
SHAQ_totalhab <- SHAQ_HERA_short %>% 
  group_by(Campaign, Mission, ID, MissionDay) %>% 
  mutate(iperf_mean = mean(iperf, na.rm = TRUE), 
         teamperf_mean = mean(teamperf, na.rm = TRUE),
         mood_mean = mean((mood), na.rm = TRUE),
         stress_mean = mean(stress, na.rm = TRUE),
         sleep_mean = mean(sleep, na.rm = TRUE),
         social_mean = mean(social, na.rm = TRUE),
         .keep= "none") %>% 
  distinct(ID, .keep_all = TRUE)

# build dataframe keeping campaigns/missions/id/missionday present in SHAQ
SHAQ_POMS_totalhab <- left_join(SHAQ_totalhab, POMS_HERA_select, by = c("Campaign","Mission","ID","MissionDay"))

# pearsons's correlation for two mood vectors
res <- res <- rmcorr(data = SHAQ_POMS_totalhab,
                     measure1 = mood_mean, 
                     measure2 = POMS_TotalMoodDist_Sum,
                     participant = factor(ID))
corr_SHAQ_POMS[1,5] <- res$r
corr_SHAQ_POMS[2,5] <- res$p

p.adjust(p = corr_SHAQ_POMS[2,], method = "holm")

##### differences in mood across habitat environments #####
# lin model for SHAQ
SHAQ_lme <- lme(data = SHAQ_all %>% na.omit(), fixed = SHAQ_Mood_How ~ Habitat, random = ~ 1 | ID)
summary(SHAQ_lme)
emmeans(SHAQ_lme, pairwise ~ Habitat)

# lin model for POMS (only hotel/HERA)
POMS_lme <- lme(data = POMS_all %>% na.omit(), fixed = POMS_TotalMoodDist_Sum ~ Habitat, random = ~ 1 | ID)
summary(POMS_lme)
emmeans(POMS_lme, pairwise ~ Habitat)


# RQ1: How is habitat-impacted mood related to overall mood? 
# Pearson's correlation analysis of SHAQ/POMS data by habitat area and by habitat
# Author: Mich Lin shuyulin [at] mit [dot] edu
# Date created: 24FEB2025 / Date last modified: 09SEP2026

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
library(rmcorr)
library(emmeans)

# set color palette for ggplot (colorblind accessible)
cbPalette <- c("#999999","#E69F00","#56B4E9","#009E73",
               "#F0E442","#0072B2","#D55E00","#CC79A7")

#### load data #####
# import workdata.RData
load("rawdata.RData")

# add habitat labels
SHAQ_Home <- SHAQ_Home %>% mutate(Habitat = "Home")
SHAQ_Hotel <- SHAQ_Hotel %>% mutate(Habitat = "Hotel")
SHAQ_HERA<- SHAQ_HERA %>% mutate(Habitat = "HERA")

# bind all together
SHAQ <- rbind(SHAQ_Home, SHAQ_Hotel, SHAQ_HERA) %>% mutate(Habitat = 
                                                             factor(Habitat, levels = c("HERA", "Hotel", "Home")
                                                             ))

POMS <- POMS %>% subset(Campaign != 4) %>% mutate(Habitat = case_when(MissionDay < 0 ~ "Hotel", 
                                                                      MissionDay > 0 ~ "HERA"))

# get POMS on SHAQ administration days (MD 4,9,16,23,30,37,44) and save this as POMS_HERA_select
POMS_HERA_select <- POMS %>% subset(MissionDay == -15 | # using MD=-15 instead since C5/6 doesn't have -16 for hotel
                  MissionDay == 4 |
                  MissionDay == 9 |
                  MissionDay == 16 |
                  MissionDay == 23 |
                  MissionDay == 30 |
                  MissionDay == 37 |
                  MissionDay == 44) %>% 
  # select (-Role) %>%
  mutate(Habitat = case_when(
    MissionDay < 0 ~ "Hotel",
    MissionDay > 0 ~ "HERA"
  ))

##### visualization #####
# summarize into mood mean/sd, grouping by mission day
SHAQ_summary <- rbind(SHAQ_Home, SHAQ_Hotel, SHAQ_HERA) %>% group_by(MissionDay) %>%
  dplyr::summarize(
    mood_mean = mean(SHAQ_Mood_How, na.rm = TRUE),
    mood_sd = sd(SHAQ_Mood_How, na.rm = TRUE),
    stress_mean = mean(SHAQ_Stress_How, na.rm = TRUE),
    stress_sd = sd(SHAQ_Stress_How, na.rm = TRUE)
  )

POMS_summary <- POMS %>% group_by(MissionDay) %>%
  dplyr::summarize(
    mood_mean = mean(POMS_TotalMoodDist_Sum, na.rm = TRUE),
    mood_sd = sd(POMS_TotalMoodDist_Sum, na.rm = TRUE)
  )

#plots
# SHAQ mood over each mission day, mean + SD error bars
ggplot(SHAQ_summary, aes(x=MissionDay, y = mood_mean))+
  geom_errorbar(aes(ymin=mood_mean-mood_sd, ymax=mood_mean+mood_sd), width=2) +
  xlab("Mission Day") + ylab("Mood") + ggtitle("SHAQ Mood") + ylim(-100,100) + xlim(-17,45) +
  # home bar
  geom_rect(aes(xmin = -17, xmax = -15, ymin = -Inf, ymax = Inf, fill = "Home"), alpha = 0.05) +
  # hotel bar
  geom_rect(aes(xmin = -15, xmax = 0, ymin = -Inf, ymax = Inf, fill = "Hotel"), alpha = 0.05) + 
  # HERA bar
  geom_rect(aes(xmin = 0, xmax = 45, ymin = -Inf, ymax = Inf, fill = "HERA"), alpha = 0.05) +
  geom_point()+
  scale_fill_manual('Habitat',
                    values = c("Home" = "#999999", "Hotel" = "#0072B2", "HERA" = "#D55E00"),
                    limits = c("Home","Hotel","HERA"),
                    guide = guide_legend(override.aes = list(alpha = 1))) 

# POMS TMD over each mission day, mean + SD error bars
ggplot(POMS_summary, aes(x=MissionDay, y = mood_mean))+
  geom_errorbar(aes(ymin=mood_mean-mood_sd, ymax=mood_mean+mood_sd), width=2) +
  geom_point()+
  xlab("Mission Day") + ylab("Total Mood Disturbance") + ggtitle("POMS Total Mood Disturbance")+
  ylim(0,148) + xlim(-17,45) +
  # hotel bar
  geom_rect(aes(xmin = -15, xmax = 0, ymin = -Inf, ymax = Inf, fill = "Hotel"), alpha = 0.01) +
  # HERA bar
  geom_rect(aes(xmin = 0, xmax = 45, ymin = -Inf, ymax = Inf, fill = "HERA"), alpha = 0.01) +
  scale_fill_manual('Habitat',
                    values = c("Hotel" = "#0072B2", "HERA" = "#D55E00"),
                    limits = c("Hotel","HERA"),
                    guide = guide_legend(override.aes = list(alpha = 1))) 

# plotting mood changes across habitat environments
# SHAQ box + whiskers plot (IQR)
ggplot(SHAQ, aes(x=Habitat, y = (SHAQ_Mood_How)))+
  stat_boxplot(geom ='errorbar', width = 0.5) +
  geom_boxplot()+
  scale_x_discrete(limits = c('Home','Hotel','HERA'))+
  ylab("SHAQ Mood")+xlab("Environment")+ylim(c(-100,100))+
  ggtitle("SHAQ Mood across Home/Hotel/HERA")

# SHAQ violin plot + jittered scatter data points
ggplot(SHAQ, aes(x=Habitat, y = SHAQ_Mood_How))+
  geom_violin()+ 
  geom_jitter(shape=20, position=position_jitter(0.2))+ # can comment to turn off data pts
  scale_x_discrete(limits = c('Home','Hotel','HERA'))+
  ylab("SHAQ Mood")+xlab("Environment")+
  ggtitle("SHAQ Mood - spread of data")

# POMS box + whiskers plot (IQR)
ggplot(POMS, aes(x=Habitat, y = POMS_TotalMoodDist_Sum))+
  stat_boxplot(geom ='errorbar', width = 0.5) +
  geom_boxplot()+
  scale_x_discrete(limits = c('Home','Hotel','HERA'))+
  ylab("POMS Mood Disturbance")+xlab("Environment")+ylim(c(0,148))+
  ggtitle("POMS Mood Disturbance across Home/Hotel/HERA")

# POMS violin plot + jittered scatter data points
ggplot(POMS, aes(x=Habitat, y = POMS_TotalMoodDist_Sum))+
  geom_violin()+ 
  geom_jitter(shape=20, position=position_jitter(0.2))+ # can comment to turn off data pts
  scale_x_discrete(limits = c('Home','Hotel','HERA'))+
  ylab("POMS Mood Disturbance")+xlab("Environment")+
  ggtitle("POMS Mood Disturbance - spread of data")

SHAQ_POMS_all <- full_join(SHAQ, POMS, by = c("Campaign","Mission","ID","MissionDay","Habitat"))

# SHAQ ridgeline
ggplot(SHAQ)+  
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

##### reconstruct dataframes, check for normality #####
# reconstruct dataframes
# "How" only, no "Why's"
SHAQ_HERA_short <- SHAQ_HERA %>% 
    select(Campaign, Mission, ID, MissionDay, SHAQ_Hab_Area,
           SHAQ_IndivPerf_How, SHAQ_TeamPerf_How, SHAQ_Mood_How, SHAQ_Stress_How,
           SHAQ_Sleep_How, SHAQ_Social_How) %>% 
  rename(iperf = SHAQ_IndivPerf_How, teamperf = SHAQ_TeamPerf_How, mood = SHAQ_Mood_How, 
         stress = SHAQ_Stress_How, sleep = SHAQ_Sleep_How, social = SHAQ_Social_How)

# rename variables
POMS <- POMS %>% rename(anxiety = POMS_TensionAnxiety_Sum,
                        depression = POMS_DepressionDejection_Sum,
                        anger = POMS_AngerHostility_Sum,
                        fatigue = POMS_FatigueInertia_Sum,
                        vigor = POMS_VigorActivity_Sum,
                        confusion = POMS_ConfusionBewilderment_Sum,
                        mood_dist = POMS_TotalMoodDist_Sum)

# checks for normality - not actually necessary
# SHAQ
ggqqplot(SHAQ_HERA_short$mood, main = "SHAQ Mood")

ggqqplot(SHAQ_HERA_short$stress, main = "SHAQ Stress")

ggqqplot(SHAQ_HERA_short$iperf, main = "SHAQ Ind. Perf.")

ggqqplot(SHAQ_HERA_short$teamperf, main = "SHAQ Team Perf.")

ggqqplot(SHAQ_HERA_short$social, main = "SHAQ Social")

ggqqplot(SHAQ_HERA_short$sleep, main = "SHAQ Sleep")

# POMS - most fail, TMD passes
ggqqplot(POMS$anxiety, main = "POMS Tension-Anxiety")

ggqqplot(POMS$depression, main = "POMS Depression-Dejection")

ggqqplot(POMS$anger, main = "POMS Anger-Hostility")

ggqqplot(POMS$fatigue, main = "POMS Fatigue-Inertia")

ggqqplot(POMS$vigor, main = "POMS Vigor-Activity")

ggqqplot(POMS$confusion, main = "POMS Confusion-Bewilderment")

ggqqplot(POMS$mood_dist, main = "POMS Total Mood Disturbance")

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

# correlation across habitat
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

# holm-adjusted p values
p.adjust(p = corr_SHAQ_POMS[2,], method = "holm") # nothing is significant

##### differences in mood across habitat environments #####
# lin model for SHAQ
SHAQ_lme <- lme(data = SHAQ %>% na.omit(), fixed = SHAQ_Mood_How ~ Habitat, random = ~ 1 | ID)
summary(SHAQ_lme)
emmeans(SHAQ_lme, pairwise ~ Habitat)

# lin model for POMS (only hotel/HERA)
POMS_lme <- lme(data = POMS %>% na.omit(), fixed = mood_dist ~ Habitat, random = ~ 1 | ID)
summary(POMS_lme)
emmeans(POMS_lme, pairwise ~ Habitat)


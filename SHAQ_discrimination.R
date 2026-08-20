# RQ3: Are the most important aspects of habitability adequately designed within HERA?
# Cross habitat environment comparisons (visualizations, ANOVA) by habitat area and by aggregate habitat
# Author: Mich Lin shuyulin [at] mit [dot] edu
# Date created: 12MAR2025 / Date last modified: 07FEB2026

##### housekeeping #####
# packages needed: ggplot2, corrr, ggcorrplot, FactoMineR, factoextra
# load libraries
library("readxl")
library(ggplot2)
library(tibble)
library(psych)
library(dplyr)
library("ggpubr")
library(reticulate)
library(nlme)
library(tidyr)
library(magrittr)
library('corrr')
library('ggcorrplot')
library('FactoMineR')
library('factoextra')
library('ggridges')
library('Hmisc')

# set color palette for ggplot (colorblind accessible)
cbPalette <- c("#999999","#E69F00","#56B4E9","#009E73",
               "#F0E442","#0072B2","#D55E00","#CC79A7")

# import rawdata.RData
load("~/MIT Dropbox/Mich Lin/Research/behavioral_health/hera_nek/shaq_code/data/wide.RData")

SHAQ_HERA <- mutate(SHAQ_HERA, Habitat = "HERA")
SHAQ_Home <- mutate(SHAQ_Home, Habitat = "Home")
SHAQ_Hotel <- mutate(SHAQ_Hotel, Habitat = "Hotel")


# bind all together
SHAQ <- rbind(SHAQ_Home, SHAQ_Hotel, SHAQ_HERA) %>% mutate(Habitat = 
                                                             factor(Habitat, levels = c("HERA", "Hotel", "Home")
                                                             ))


##### total habitat aggregate #####
PAH_lab <- c("Privacy", "Social Density", "Efficiency","Control","Comfort", "Convenience")
hab_areas_lab <- c("Sleep","Hygiene","Work","Galley")

# violin plot (by area or total habitat) PAH across different environments
discrimination_plot <- function(data, hab_area, BHP_outcome, PAH){
  # data = dataframe; hab_area = {1,2,3,4}, BHP_outcome = {"Mood", "Stress",...} PAH = {"Privacy","Social Density", ...}
  if(hab_area == 5){
    ggplot(subset(data, BHP_Outcome == BHP_outcome), aes(x=Habitat, y=!!sym(PAH)))+
      geom_violin()+
      stat_summary(fun=mean, geom='point', shape=20, size = 5, color = "red")+
      stat_summary(fun.data = "mean_sdl", geom = "errorbar", color = "red",width = .15) +
      xlab("Habitat Environment")+ylab(PAH_lab[as.numeric(unlist(strsplit(PAH, "_"))[2])])+
      ggtitle(paste("Impact of",PAH_lab[as.numeric(unlist(strsplit(PAH, "_"))[2])],"on",BHP_outcome, 
                    "across Habitat Environments", sep=" "))+
      scale_x_discrete(limits = c('Home','Hotel','HERA'))}
  else{ggplot(subset(data, SHAQ_Hab_Area == hab_area & BHP_Outcome == BHP_outcome), aes(x=Habitat, y=!!sym(PAH)))+
      # stat_boxplot(geom ='errorbar', width = 0.3) +
      geom_violin()+
      # geom_boxplot()+
      stat_summary(fun=mean, geom='point', shape=20, size = 5, color = "red")+
      stat_summary(fun.data = "mean_sdl", geom = "errorbar", color = "red",width = .15) +
      xlab("Habitat Environment")+ylab(PAH_lab[as.numeric(unlist(strsplit(PAH, "_"))[2])])+
      ggtitle(paste("Impact of",PAH_lab[as.numeric(unlist(strsplit(PAH, "_"))[2])],"on",BHP_outcome, 
                    "across Habitat Environments", sep=" "))+
      scale_x_discrete(limits = c('Home','Hotel','HERA'))+
      scale_y_discrete(limits = c("1","2","3","4","5","6","7"))}
}

# ridgeline plot
discrimination_plot <- function(data, hab_area, BHP_outcome, PAH){
  # data = dataframe; hab_area = {1,2,3,4}, BHP_outcome = {"Mood", "Stress",...} PAH = {"Privacy","Social Density", ...}
  if(hab_area == 5){
    df <- subset(data, BHP_Outcome == BHP_outcome)}
  else{
    df <- subset(data, SHAQ_Hab_Area == hab_area & BHP_Outcome == BHP_outcome)
  }
  
  ggplot(df)+
    geom_density_ridges(aes(y=Habitat, x=!!sym(PAH)), alpha = 0.7) +
    # mean and standard deviation error bars
    stat_summary(aes(y=Habitat, x=!!sym(PAH)), fun=mean, geom='point', 
                 shape=20, size = 4, color = "red", position = position_nudge(y=0.3))+
    stat_summary(aes(y=Habitat, x=!!sym(PAH)), fun.data =  mean_sdl, 
                 # fun.args = list(conf.int = .5),
                 fun.args = list(mult = 1),
                 geom = "errorbar", color = "red",width = .15, position = position_nudge(y=0.3)) +
    # reverse the x axis so it goes from ideal to inadequate
    scale_x_continuous(breaks = c(1,4,7), labels = c("Inadequate","Adequate","Ideal")) +
    # scale_x_reverse(breaks = c(7, 4, 1), labels = c("Ideal","Adequate","Inadequate"))+
    
    ylab("Habitat Environment")+xlab(PAH_lab[as.numeric(unlist(strsplit(PAH, "_"))[2])])+
    coord_cartesian(clip = "off") + 
    ggtitle(paste("Impact of",PAH_lab[as.numeric(unlist(strsplit(PAH, "_"))[2])],"on",BHP_outcome, 
                  "in the", hab_areas_lab[hab_area], "Area", sep=" "))+
    theme_ridges()+ theme(plot.title = element_text(size = 11), 
                          axis.text = element_text(size= 10), 
                          axis.title = element_text(size = 11))
}
# call plots, hab_area = 5 == total habitat is plotted as aggregate
# "Privacy", "Social Density", "Efficiency","Control","Comfort", "Convenience"
# print(discrimination_plot(SHAQ, 4, "Social", "Why_4"))

why_vec <- c("Why_1","Why_2","Why_3","Why_4","Why_5","Why_6")
BHP_vec <- c("IndivPerf","TeamPerf","Mood","Stress","Sleep","Social")
for (i in why_vec) {
  print(discrimination_plot(SHAQ, 1, "Mood", i))
}

# summary statistics (psych-describe) for reporting // change PAH/BHP/area/habitat
describe(subset(SHAQ, SHAQ_Hab_Area == 1 & BHP_Outcome == "Mood" & Habitat == "HERA"))

# put together table in latex form
describe_disc_latex <- function(hab_area, hab_env){
  mean_df <- data.frame(matrix(ncol = 6, nrow = 6))
  sd_df <- data.frame(matrix(ncol = 6, nrow = 6))
  for (i in 1:6){
   # 10-15 are positions of why 1-6; transpose and add as a col vector
   mean_df[,i] <- psych::describe(subset(SHAQ, SHAQ_Hab_Area == hab_area & BHP_Outcome == BHP_vec[i] & Habitat == hab_env))$mean[10:15]
  sd_df[,i] <- psych::describe(subset(SHAQ, SHAQ_Hab_Area == hab_area & BHP_Outcome == BHP_vec[i] & Habitat == hab_env))$sd[10:15]
 }

 for (i in 1:6){
   # cat for printing with double slashes
   # paste0 for concat headers + other text
   # paste for adding & delimiters in collapse function
   # sprintf for running through numeric vectors
  
    cat(paste0("& ",hab_env, " & ", paste(sprintf("$%.1f \\pm %.1f$", mean_df[i,], sd_df[i,]), collapse = " &  &"), " &", " \\\\ \n"))
   }
}

describe_disc_latex(1, "Home")

# generate per habitat area for all environments
for (environments in c("Home","Hotel","HERA")){
  # describe_disc_latex <- function(hab_area, hab_env)
  describe_disc_latex(4, environments)
}

##### one way ANOVAs #####
# test case
# res.anova <- aov(formula = reformulate("Why_1") ~ Habitat, data = subset(SHAQ, SHAQ_Hab_Area == 1 & BHP_Outcome == "Sleep"))
# summary(res.anova)
# TukeyHSD(res.anova)

# ANOVA accounting for multiple t tests, specifying habitat area (not the whole habitat)
PAH_anova <- function(data, hab_area, BHP_outcome, PAH){
  fo <- reformulate("Habitat", PAH) # function to help pass in string as variable
  df <- subset(data, SHAQ_Hab_Area == hab_area & BHP_Outcome == BHP_outcome) # subset the df to be habitat area specific
  res.anova <- aov(fo, df)
  # res.anova <- aov(formula = as.name(PAH) ~ Habitat, data = subset(data, SHAQ_Hab_Area == hab_area & BHP_Outcome == BHP_outcome))
  anova_summary <- summary(res.anova)
  tukey_result <- TukeyHSD(res.anova)
  
  # Extract p-value from ANOVA summary
  anova_p_value <- anova_summary[[1]]$`Pr(>F)`[1]
  
  # Extract adjusted p-values from TukeyHSD
  tukey_p_values <- tukey_result$Habitat[, "p adj"]

  return(list(ANOVA_p_value = anova_p_value, Tukey_p_values = tukey_p_values))
}

# assessing ANOVA & tukey results 
# "Privacy", "Social Density", "Efficiency","Control","Comfort", "Convenience"
test <- PAH_anova(SHAQ, 1, "Sleep",'Why_1')

# run through all combinations to get raw p values
all_p_values <- list()

for (ha in c(1,2,3,4)) {
  for (bhp in c("IndivPerf","TeamPerf","Mood","Stress","Sleep","Social")) {
    for (pah in c("Why_1","Why_2","Why_3","Why_4","Why_5","Why_6")) {
      results <- PAH_anova(SHAQ, ha, bhp, pah)
      analysis_name <- paste(ha, bhp, pah, sep = "_")
      all_p_values[[analysis_name]] <- list(
        ANOVA_p = results$ANOVA_p_value,
        Tukey_p = results$Tukey_p_values
      )
    }
  }
}

all_raw_p_values <- unlist(lapply(all_p_values, function(x) c(x$ANOVA_p, x$Tukey_p)))
adjusted_p_values_holm <- p.adjust(all_raw_p_values, method = "holm") # holm adjusted {p.adjust(pvalues, method = "bonferroni")}



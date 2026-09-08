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


##### plotting #####
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

###### testing for significant differences between mean mood in habitat env #####
# unit test
# summary(lme(data = subset(SHAQ, SHAQ_Hab_Area == 3 & BHP_Outcome == "Sleep") %>% 
# na.omit(), fixed = Why_6 ~ Habitat, random = ~ 1 | ID))

emmeans_obj <- emmeans(lme(data = subset(SHAQ, SHAQ_Hab_Area == 1 & BHP_Outcome == "Stress") %>% 
                             na.omit(), fixed = Why_2 ~ Habitat, random = ~ 1 | ID), pairwise ~ Habitat) %>% summary()
emmeans_obj$emmeans$emmean %>% rev()
emmeans_obj$emmeans$SE %>% rev()

# generating latex code for paper
for (env in 1:3){ # iterate over env (home/hotel/HERA)
  for (i in 4:4){ # iterate over habitat areas
    mean_df <- data.frame(matrix(ncol = 6, nrow = 6))
    sd_df <- data.frame(matrix(ncol = 6, nrow = 6))
    for (j in 1:6){ # iterate over BHP outcomes
      for (k in 1:6){ # iterate over why's
        emmeans_obj <- emmeans(lme(data = subset(SHAQ, SHAQ_Hab_Area == i & BHP_Outcome == BHP_vec[j]) %>% 
                                     na.omit(), fixed = as.formula(paste(why_vec[k], "~", "Habitat")), random = ~ 1 | ID), pairwise ~ Habitat) %>% summary()
        
        mean_df[k,j] <- emmeans_obj$emmeans$emmean %>% rev() %>% .[env]
        sd_df[k,j] <- emmeans_obj$emmeans$SE %>% rev() %>% .[env]
        
      }
    }
  }
  if (env == 1){ # printing home
    for (i in 1:6){
      # cat for printing with double slashes
      # paste0 for concat headers + other text
      # paste for adding & delimiters in collapse function
      # sprintf for running through numeric vectors
      cat(paste0("& ","Home", " & ", paste(sprintf("$%.1f \\pm %.1f$", mean_df[i,], sd_df[i,]), collapse = " &  &"), " &", " \\\\ \n"))
    }
  } else if (env == 2) {# printing hotel
    for (i in 1:6){
      cat(paste0("& ","Hotel", " & ", paste(sprintf("$%.1f \\pm %.1f$", mean_df[i,], sd_df[i,]), collapse = " &  &"), " &", " \\\\ \n"))
    }
  }
  else {
    for (i in 1:6){# printing HERA
      cat(paste0("& ","HERA", " & ", paste(sprintf("$%.1f \\pm %.1f$", mean_df[i,], sd_df[i,]), collapse = " &  &"), " &", " \\\\ \n"))
    }
  }
}

# built function to run lme's and generate list of saved p-values
# do not apply FWER correction here since adjusting using holm at the end
PAH_lme <- function(hab_area_i, BHP_i, PAH_i){
  emmeans_obj <- emmeans(lme(data = subset(SHAQ, SHAQ_Hab_Area == hab_area_i & BHP_Outcome == BHP_i) %>% na.omit(), 
                             fixed = as.formula(paste(PAH_i, "~", "Habitat")), 
                             random = ~ 1 | ID), pairwise ~ Habitat, adjust = "tukey") %>% 
    summary()
  # extract p values from summary
  labels <- emmeans_obj$contrasts$contrast
  p_vals <- emmeans_obj$contrasts$p.value %>% t() %>% as.data.frame()
  names(p_vals) <- labels 
  # save as list object under p_values, where the column names are the contrasts (e.g., home-hotel)
  # and the values are the Tukey-adjusted p values
  return(list(p_values = p_vals))
}

# run through all combinations to get raw p values
all_p_values <- list()

# iterate through combinations to save p values to list
for (i in 1:4){ # iterate over habitat areas
  for (j in 1:6){ # iterate over BHP outcomes
    for (k in 1:6){ # iterate over why's
      model_res <- PAH_lme(i, BHP_vec[j], why_vec[k])
      
      analysis_name <- paste(i, BHP_vec[j], why_vec[k], sep = "_")
      all_p_values[[analysis_name]] <- list(
        p_tukey = model_res$p_values
      )
    }
  }
}

# holm adjusted p values over the whole list
adjusted_p_values_holm <- p.adjust(unlist(lapply(all_p_values, function(x) c(x$p_tukey))), 
                                   method = "holm") 
adjusted_p_values_holm < 0.05
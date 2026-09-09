# RQ2: What are the most important aspects of habitability?
# PCA, linear model, linear mixed-effects model, visualizations
# Author: Mich Lin shuyulin [at] mit [dot] edu
# Date created: 03MAR2025 / Date last modified: 09SEP2026

##### housekeeping #####
# load libraries
library("readxl")
library(ggplot2)
library(tibble)
library(dplyr)
library("ggpubr")
library(reticulate)
library(nlme)
library(lme4)
library(tidyr)
library(magrittr)
library('corrr')
library('ggcorrplot')
library('FactoMineR')
library('factoextra')
library(glmmTMB)
library(glmnet)
library(brms)
library(ggridges)
library(ggradar)
library(car)

# set color palette for ggplot (colorblind accessible)
cbPalette <- c("#999999","#E69F00","#56B4E9","#009E73",
               "#F0E442","#0072B2","#D55E00","#CC79A7")

##### load data #####
# import wide data format (from data_cleaning.R)
load("wide.RData")
PAH_lab <- c("Privacy", "Social","Efficiency","Control","Comfort","Convenience")

##### data precheck #####
# linearity
# independence
# homoscedasticity of residuals - AFTER
# normality of residuals - AFTER
##### linear mixed effects model #####
habitability_moderator_model <- function(df, hab_area, bhp){
  df.active <- df %>% subset(SHAQ_Hab_Area == hab_area & BHP_Outcome == bhp) %>% na.omit()
  # select(c(How, Why_1, Why_2, Why_3, Why_4, Why_5, Why_6))
  # print(df.active)
  
  # form linear model for the why's
  model <- lme(
    fixed = How ~ Why_1 + Why_2 + Why_3 +
      Why_4 + Why_5 + Why_6 + Campaign + (MissionDay),
    # random = lilmest(ID = pdDiag(~ 1 + MissionDay)),
    random = ~ 1 + MissionDay | ID,
    # fixed = How ~ Why_1 + Why_2 + Why_3 +
    #              Why_4 + Why_5 + Why_6,
    #            random = list(
    #              ID = pdDiag(~ 1 + MissionDay),
    #              Campaign = pdDiag(~ 1)),
              
               data = df.active,
    control = lmeControl(opt = "optim", msMaxIter = 100))
  
  print(summary(model))
  print(resid(model, type = "normalized"))
  
  # plot residuals
  print(plot(model))
  
  # variance inflation factor (multicollinearity check)
  # PASS; can uncomment for full check 
  # print(vif(model))
  
  # post-check; normality of residuals
  hist(residuals(model))
  # Plot the residuals
  qqnorm(residuals(model))
  qqline(residuals(model))
  
  # multicollinearity assumption check  
  # Remove the How column
  reduced_data <- df %>% 
    subset(SHAQ_Hab_Area == hab_area & BHP_Outcome == bhp) %>% 
    select(c(Why_1, Why_2, Why_3, Why_4, Why_5, Why_6)) %>% na.omit()
  colnames(reduced_data) <- PAH_lab
  corr_matrix = round(cor(reduced_data), 2)
  
  # Compute and show the  result
  ggcorrplot(corr_matrix, hc.order = FALSE, type = "lower",
             lab = TRUE)
}

# # manual check
# model_res <- habitability_moderator_model(SHAQ_HERA, 4, "Social")
# print(model_res)

# multicollinearity check of PAHs across the whole dataset
corr_df = SHAQ_HERA %>% 
  select(c(Why_1, Why_2, Why_3, Why_4, Why_5, Why_6)) %>%
  na.omit()
colnames(corr_df) <- PAH_lab
ggcorrplot(round(cor(corr_df),2), hc.order = FALSE, type = "lower",
           lab = TRUE)

##### RADAR PLOTS - LM COEFFS #####
# run lm model, use coeffs as magnitude for radar plots
# 1 sleep
# 2 hygiene
# 3 work
# 4 kitchen
# change variables here
model_res <- habitability_moderator_model(SHAQ_HERA,3, "TeamPerf")
print(model_res)

# SLEEP
sleep_radar <- data.frame(matrix(ncol = 3, nrow = 6))
names(sleep_radar) <- c("Mood","Stress","Sleep")
rownames(sleep_radar) <- c("privacy","social","efficiency","control","comfort","convenience")
sleep_radar$Mood[5] <- 6.55026
sleep_radar$Mood[6] <- 5.11103
sleep_radar$Stress[4] <- 5.54132
sleep_radar$Stress[5] <- 6.37227
sleep_radar$Sleep[4] <- 5.66974
sleep_radar$Sleep[5] <- 14.41213
sleep_radar <- sleep_radar %>% 
  replace(is.na(.),0) %>% 
  t() %>% as.data.frame()
sleep_radar <- cbind(group = as.factor(c("Mood","Stress","Sleep")),sleep_radar)
ggradar(sleep_radar,
        grid.min = -5, grid.mid = 0, grid.max = 15,
        label.gridline.min = FALSE, label.gridline.mid = FALSE, label.gridline.max = FALSE,
        values.radar = c("-5", "0", "15"),
        # Polygons
        group.line.width = 1, 
        group.point.size = 2,
        group.colours = c(cbPalette[2],cbPalette[3],cbPalette[4]),
        fill = TRUE,
        fill.alpha = 0.5,
        # Background and grid lines
        background.circle.colour = "white",
        gridline.mid.colour = "grey",
        grid.label.size = 8,
        axis.label.size = 8,
        # legend
        legend.position	= "bottom",
        legend.text.size = 18) + 
  theme(plot.margin = margin(0,2,0,2, 'cm'),
        coord_cartesian(clip = "off"))

# HYGIENE
hygiene_radar <- data.frame(matrix(ncol = 1, nrow = 6))
names(hygiene_radar) <- c("Mood")
rownames(hygiene_radar) <- c("privacy","social","efficiency","control","comfort","convenience")
hygiene_radar$Mood[5] <- 6.94602 
hygiene_radar$Mood[6] <- 5.04359
hygiene_radar <- hygiene_radar %>% 
  replace(is.na(.),0) %>% 
  t() %>% as.data.frame()
hygiene_radar <- cbind(group = as.factor(c("Mood")),hygiene_radar)
ggradar(hygiene_radar,
        grid.min = -5, grid.mid = 0, grid.max = 15,
        label.gridline.min = FALSE, label.gridline.mid = FALSE, label.gridline.max = FALSE,
        values.radar = c("-5", "0", "15"),
        # Polygons
        group.line.width = 1, 
        group.point.size = 2,
        group.colours = c(cbPalette[2]),
        fill = TRUE,
        fill.alpha = 0.5,
        # Background and grid lines
        background.circle.colour = "white",
        gridline.mid.colour = "grey",
        grid.label.size = 8,
        axis.label.size = 8,
        # legend
        legend.position	= "bottom",
        legend.text.size = 18) + 
  theme(plot.margin = margin(0,2,0,2, 'cm'),
        coord_cartesian(clip = "off"))

# WORK
work_radar <- data.frame(matrix(ncol = 3, nrow = 6))
names(work_radar) <- c("Stress","IndivPerf","TeamPerf")
rownames(work_radar) <- c("privacy","social","efficiency","control","comfort","convenience")
work_radar$Stress[1] <- 5.07819
work_radar$Stress[5] <- 7.35477
work_radar$IndivPerf[4] <- 7.05484
work_radar$IndivPerf[5] <- 6.17214
work_radar$IndivPerf[6] <- 5.42814
work_radar$TeamPerf[2] <- 9.727094
work_radar <- work_radar %>% 
  replace(is.na(.),0) %>% 
  t() %>% as.data.frame()
work_radar <- cbind(group = as.factor(c("Stress","IndivPerf","TeamPerf")),work_radar)
ggradar(work_radar,
        grid.min = -5, grid.mid = 0, grid.max = 15,
        label.gridline.min = FALSE, label.gridline.mid = FALSE, label.gridline.max = FALSE,
        values.radar = c("-5", "0", "15"),
        # Polygons
        group.line.width = 1, 
        group.point.size = 2,
        group.colours = c(cbPalette[2],cbPalette[3],cbPalette[4]),
        fill = TRUE,
        fill.alpha = 0.5,
        # Background and grid lines
        background.circle.colour = "white",
        gridline.mid.colour = "grey",
        grid.label.size = 8,
        axis.label.size = 8,
        # legend
        legend.position	= "bottom",
        legend.text.size = 18) + 
  theme(plot.margin = margin(0,2,0,2, 'cm'),
        coord_cartesian(clip = "off"))

# GALLEY
galley_radar <- data.frame(matrix(ncol = 2, nrow = 6))
names(galley_radar) <- c("Mood","Social")
rownames(galley_radar) <- c("privacy","social","efficiency","control","comfort","convenience")
galley_radar$Mood[1] <- -4.78975
galley_radar$Mood[4] <- 5.99575
galley_radar$Mood[5] <- 6.27002
galley_radar$Social[1] <- -3.671325
galley_radar$Social[2] <- 9.275042
galley_radar$Social[6] <- 4.289839
galley_radar <- galley_radar %>% 
  replace(is.na(.),0) %>% 
  t() %>% as.data.frame()
galley_radar <- cbind(group = as.factor(c("Mood","Social")),galley_radar)
ggradar(galley_radar,
        grid.min = -5, grid.mid = 0, grid.max = 15,
        label.gridline.min = FALSE, label.gridline.mid = FALSE, label.gridline.max = FALSE,
        values.radar = c("-5", "0", "15"),
        # Polygons
        group.line.width = 1, 
        group.point.size = 2,
        group.colours = c(cbPalette[2],cbPalette[3]),
        fill = TRUE,
        fill.alpha = 0.5,
        # Background and grid lines
        background.circle.colour = "white",
        gridline.mid.colour = "grey",
        grid.label.size = 8,
        axis.label.size = 8,
        # legend
        legend.position	= "bottom",
        legend.text.size = 18) + 
  theme(plot.margin = margin(0,2,0,2, 'cm'),
        coord_cartesian(clip = "off"))

##### data investigation #####
# histograms for misbehaving residuals
hist(SHAQ_HERA %>% subset(SHAQ_Hab_Area == 1 & BHP_Outcome == "Sleep") %>% .$How,
     main = "", xlab = "")

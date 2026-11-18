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
# load("widedata.RData")
PAH_lab <- c("Privacy", "Social","Efficiency","Control","Comfort","Convenience")

##### data precheck #####
# sets to run:
# 1: Mood, Stress, Sleep
# 2: Mood
# 3: IndivPerf, TeamPerf, Stress
# 4: Mood, Social
hist(SHAQ_HERA %>% subset(SHAQ_Hab_Area == 1 & BHP_Outcome == "Sleep") %>% .$How,
     main = "", xlab = "")

##### linear mixed effects model #####
# assumptions for linear models

# linearity - checked with residuals
# independence - 
# homoscedasticity of residuals - checked visually
# normality of residuals - checked visually
# (not an assumption, but can limit model interpretation): 
# independence of observations - checked with correlations, calculate VIF

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
  
  # summary table of coefficients and standard error
  print(summary(model)$tTable)
  
  # full summary table and residual vector
  # print(summary(model))
  # print(resid(model, type = "normalized"))
  
  # plot residuals - check for homoscedasticity
  print(plot(model))

  # variance inflation factor (multicollinearity check)
  # PASS; can uncomment for full check 
  # print(vif(model))
  
  # check; normality of residuals
  hist(residuals(model))
  # check; linearity of residuals
  qqnorm(residuals(model))
  qqline(residuals(model))
  print(shapiro.test(residuals(model)))
  
  # independence of residuals check
  print(plot(ACF(model)))
  
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
# 1 sleep: Mood, Stress, Sleep
# 2 hygiene: Mood
# 3 work: IndivPerf, TeamPerf, Stress
# 4 galley: Mood, Social
# change variables here
model_res <- habitability_moderator_model(SHAQ_HERA,4, "Social")
print(model_res)

# SLEEP
sleep_radar <- data.frame(matrix(ncol = 3, nrow = 6))
names(sleep_radar) <- c("Mood","Stress","Sleep")
rownames(sleep_radar) <- c("privacy","social","efficiency","control","comfort","convenience")
sleep_radar$Mood[5] <- 6.41839980
sleep_radar$Mood[6] <- 5.6045321
sleep_radar$Stress[4] <- 5.452147
sleep_radar$Stress[5] <- 6.4928281
sleep_radar$Sleep[4] <- 5.9356086
sleep_radar$Sleep[5] <- 15.0054496
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
names(hygiene_radar) <- c("Mood", "Social")
rownames(hygiene_radar) <- c("privacy","social","efficiency","control","comfort","convenience")
hygiene_radar$Mood[5] <- 6.230177 
hygiene_radar$Mood[6] <- 3.031229
hygiene_radar$Social[5] <- 6.230177 
hygiene_radar$Mood[6] <- 3.031229
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
work_radar$Stress[1] <- 4.871902584
work_radar$Stress[5] <- 7.257657467
work_radar$IndivPerf[4] <- 6.96146422
work_radar$IndivPerf[5] <- 6.17486649
work_radar$IndivPerf[6] <- 5.09736684
work_radar$TeamPerf[2] <- 9.74275371
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
galley_radar$Mood[1] <- -5.024373
galley_radar$Mood[4] <- 6.389230
galley_radar$Mood[5] <- 6.230177
galley_radar$Social[2] <- 9.120337097
galley_radar$Social[6] <- 4.305024532
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


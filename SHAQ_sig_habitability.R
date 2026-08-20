# RQ2: What are the most important aspects of habitability?
# PCA, linear model, linear mixed-effects model, visualizations
# Author: Mich Lin shuyulin [at] mit [dot] edu
# Date created: 03MAR2025 / Date last modified: 22APR2025

##### housekeeping #####
# packages needed: ggplot2, corrr, ggcorrplot, FactoMineR, factoextra
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


# set color palette for ggplot (colorblind accessible)
cbPalette <- c("#999999","#E69F00","#56B4E9","#009E73",
               "#F0E442","#0072B2","#D55E00","#CC79A7")

##### load data #####
# import wide data format (from data_cleaning.R)
load("~/MIT Dropbox/Mich Lin/Research/behavioral_health/hera_nek/shaq_code/data/wide.RData")

##### PCA by habitat area #####
# didn't end up using, not applicable to repeated measures & too few constructs to begin with

PCA_byarea <- function(df, hab_area, bhp){
  # takes df = data, hab_area = {1, 2, 3, 4}, bhp = {"IndivPerf", "TeamPerf", ...}
  
   df.active <- df %>% subset(SHAQ_Hab_Area == hab_area & BHP_Outcome == bhp) %>% 
    select(c(Why_1, Why_2, Why_3, Why_4, Why_5, Why_6)) 
  # print(df.active)
  # check percentage of missing data, should be less than 0.2
  # https://medium.com/@seb231/principal-component-analysis-with-missing-data-9e28f440ce93
   
  # print percentage of missing data
  print(sum(is.na(df.active))/(dim(df.active)[1] * dim(df.active)[2]))
  
  # omit missing data for PCA
  df.active <- df.active %>% na.omit() 
   
   # form PCA for the why's
  prcomp(df.active, center = TRUE, scale = TRUE)
}

# change data source, habitat area, and 'why'
pca_res <- PCA_byarea(SHAQ_Home, 4, "Social")
print(pca_res)
pca_res$sdev^2 / sum(pca_res$sdev^2) # proportion of variance
biplot(pca_res, scale = 0)

##### trying multiple models  #####
# this section of code adapted from gemini

# LASSO MODEL - didn't end up using, virtually no difference
# # Assuming 'your_data' is your data frame
# df.active <- subset(SHAQ_HERA, SHAQ_Hab_Area == 1 & BHP_Outcome == "Sleep") %>% na.omit()
# X <- model.matrix(How ~ Why_1 + Why_2 + Why_3 + Why_4 + Why_5 + Why_6, data = df.active)
# y <- df.active$How
#
# # Now try running glmnet
# lasso_model <- glmnet(X, y, alpha = 1)
# 
# # Cross-validation to find the optimal lambda
# cv_lasso <- cv.glmnet(X, y, alpha = 1)
# best_lambda <- cv_lasso$lambda.min
# 
# # Model with the best lambda
# best_lasso_model <- glmnet(X, y, alpha = 1, lambda = best_lambda)
# coef(best_lasso_model)
# 
# print(best_lasso_model)
# 
# # ZERO-INFLATED MODEL
# formula_count <- How ~ Why_1 + Why_2 + Why_3 + Why_4 + Why_5 + Why_6 +
#   (1 | ID) + (1 | Campaign)
# 
# formula_zero <- ~ 1
# 
# model_zi <- glmmTMB(formula = formula_count,
#                     ziformula = formula_zero,
#                     data = subset(SHAQ_HERA, SHAQ_Hab_Area == 1 & BHP_Outcome == "Sleep"),
#                     family = gaussian(link = "identity"))
# 
# summary(model_zi)
# 
# # BAYESIAN MODEL
# 
# # Assuming your outcome is 'BehavioralHealthOutcome' and predictors are 'Why_1' to 'Why_6', 'MissionDay'
# # Random effects: Intercept and slope of MissionDay vary by ID within MissionID, and intercept varies by CampaignID
# 
# formula_bayes <- brmsformula(
#   How ~ Why_1 + Why_2 + Why_3 + Why_4 + Why_5 + Why_6 +
#     (1 + MissionDay | ID) + (1 | Campaign),
#   family = gaussian() # Assuming VAS scale can be treated as approximately normal
# )
# 
# # Weakly informative priors
# priors_weak <- c(
#   prior(normal(0, 1), class = b),       # Fixed effects coefficients (why's, MissionDay)
#   prior(exponential(1), class = sd),    # Standard deviations of random effects and residual
#   prior(lkj_corr_cholesky(1), class = cor) # Correlations between random effects (if any)
# )
# 
# # Fit the Bayesian model
# bayes_model <- brm(
#   formula = formula_bayes,
#   data = df.active,
#   prior = priors_weak,
#   chains = 4,        # Number of MCMC chains
#   iter = 4000,       # Total iterations per chain
#   warmup = 1000,     # Burn-in iterations
#   cores = 4,         # Number of CPU cores to use
#   control = list(adapt_delta = 0.9) # Adjust for better convergence if needed
# )
# 
# # Summarize the results
# summary(bayes_model)
# 
# # Check MCMC diagnostics (Rhat values should be close to 1)
# rhat(bayes_model)
# 
# # Examine posterior distributions
# plot(bayes_model)

##### linear mixed effects model #####
habitability_moderator_model <- function(df, hab_area, bhp){
  df.active <- df %>% subset(SHAQ_Hab_Area == hab_area & BHP_Outcome == bhp) %>% na.omit()
  # select(c(How, Why_1, Why_2, Why_3, Why_4, Why_5, Why_6))
  # print(df.active)
  
  # form linear model for the why's
  model <- lme(
    fixed = How ~ Why_1 + Why_2 + Why_3 +
      Why_4 + Why_5 + Why_6 + Campaign + (MissionDay),
    # random = list(ID = pdDiag(~ 1 + MissionDay)),
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
  
  
  # check residuals for normality
  hist(residuals(model))
  # Plot the residuals
  qqnorm(residuals(model))
  qqline(residuals(model))
  
  # multicollinearity assumption check  
  # Remove the How column
  reduced_data <- df %>% 
    subset(SHAQ_Hab_Area == hab_area & BHP_Outcome == bhp) %>% 
    select(c(Why_1, Why_2, Why_3, Why_4, Why_5, Why_6)) %>% na.omit()
  
  corr_matrix = round(cor(reduced_data), 2)
  
  # Compute and show the  result
  ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
             lab = TRUE)
}

# # manual check
# model_res <- habitability_moderator_model(SHAQ_HERA, 4, "Social")
# print(model_res)

##### RADAR PLOTS - PEARSON'S COEFFS #####
# didn't end up using this one; preferred LM coeffs instead 

# setup list of items (habitat areas)
corr_PAH <- list(Sleep = list(corr = data.frame(matrix(ncol = 6, nrow = 6)), 
                              p.val = data.frame(matrix(ncol = 6, nrow = 6))),
                 Hygiene = list(corr = data.frame(matrix(ncol = 6, nrow = 6)), 
                                p.val = data.frame(matrix(ncol = 6, nrow = 6))),
                 Work = list(corr = data.frame(matrix(ncol = 6, nrow = 6)), 
                             p.val = data.frame(matrix(ncol = 6, nrow = 6))),
                 Galley = list(corr = data.frame(matrix(ncol = 6, nrow = 6)), 
                               p.val = data.frame(matrix(ncol = 6, nrow = 6))))
# name df within each hab area
names(corr_PAH$Sleep$corr) <- c("Mood","Stress","Sleep","IndivPerf","TeamPerf","Social") # col names
names(corr_PAH$Sleep$p.val) <- c("Mood","Stress","Sleep","IndivPerf","TeamPerf","Social") # col names
rownames(corr_PAH$Sleep$corr) <- c("privacy","social","efficiency","control","comfort","convenience")
rownames(corr_PAH$Sleep$p.val) <- c("privacy","social","efficiency","control","comfort","convenience")
names(corr_PAH$Hygiene$corr) <- c("Mood","Stress","Sleep","IndivPerf","TeamPerf","Social") # col names
names(corr_PAH$Hygiene$p.val) <- c("Mood","Stress","Sleep","IndivPerf","TeamPerf","Social") # col names
rownames(corr_PAH$Hygiene$corr) <- c("privacy","social","efficiency","control","comfort","convenience")
rownames(corr_PAH$Hygiene$p.val) <- c("privacy","social","efficiency","control","comfort","convenience")
names(corr_PAH$Work$corr) <- c("Mood","Stress","Sleep","IndivPerf","TeamPerf","Social") # col names
names(corr_PAH$Work$p.val) <- c("Mood","Stress","Sleep","IndivPerf","TeamPerf","Social") # col names
rownames(corr_PAH$Work$corr) <- c("privacy","social","efficiency","control","comfort","convenience")
rownames(corr_PAH$Work$p.val) <- c("privacy","social","efficiency","control","comfort","convenience")
names(corr_PAH$Galley$corr) <- c("Mood","Stress","Sleep","IndivPerf","TeamPerf","Social") # col names
names(corr_PAH$Galley$p.val) <- c("Mood","Stress","Sleep","IndivPerf","TeamPerf","Social") # col names
rownames(corr_PAH$Galley$corr) <- c("privacy","social","efficiency","control","comfort","convenience")
rownames(corr_PAH$Galley$p.val) <- c("privacy","social","efficiency","control","comfort","convenience")

# pearson's tests for radar plots
# function that saves correlation results in corr_PAH
pearsons_PAH <- function(data, corr_PAH, hab_area){
  PAH_lab <- c("Why_1", "Why_2", "Why_3","Why_4","Why_5","Why_6")
  hab_areas_lab <- c("Sleep","Hygiene","Work","Galley")
  j <- 1 # thru each bhp outcome
  for (bhp_outcome in c("Mood","Stress","Sleep","IndivPerf","TeamPerf","Social")){
    df.active <- subset(data, SHAQ_Hab_Area == hab_area & BHP_Outcome == bhp_outcome)
    i <- 1 # thru each why
    for(PAH in PAH_lab){
      # run and save results in corr_PAH
      res <- cor.test(df.active[["How"]], df.active[[PAH]])
      corr_PAH[[hab_areas_lab[hab_area]]]$corr[i,j] <- res$estimate
      corr_PAH[[hab_areas_lab[hab_area]]]$p.val[i,j] <- res$p.value
      i <- i+1
    }
    j <- j + 1
  }
  return(corr_PAH)
}

# run for all hab areas
for (i in 1:4){
  corr_PAH <- pearsons_PAH(SHAQ_HERA, corr_PAH, i)
}

# # manual check
# res <- cor.test(subset(SHAQ_HERA, SHAQ_Hab_Area == 1 & BHP_Outcome == "TeamPerf")$How, 
#                 subset(SHAQ_HERA, SHAQ_Hab_Area == 1 & BHP_Outcome == "TeamPerf")$Why_2, 
#                 method = "pearson")
# res$estimate
# res$p.value

# plot radar charts for each area & don't plot depending on lm sig. var
# SLEEP8
sleep_radar <- data.frame(matrix(ncol = 3, nrow = 6))
names(sleep_radar) <- c("Mood","Stress","Sleep")
rownames(sleep_radar) <- c("privacy","social","efficiency","control","comfort","convenience")
sleep_radar$Mood[5] <- corr_PAH$Sleep$corr[5,1]
sleep_radar$Mood[6] <- corr_PAH$Sleep$corr[6,1]
sleep_radar$Stress[4] <- corr_PAH$Sleep$corr[4,2]
sleep_radar$Stress[5] <- corr_PAH$Sleep$corr[5,2]
sleep_radar$Sleep[4] <- corr_PAH$Sleep$corr[4,3]
sleep_radar$Sleep[5] <- corr_PAH$Sleep$corr[5,3]
sleep_radar <- sleep_radar %>% 
  replace(is.na(.),0) %>% 
  t() %>% as.data.frame()
sleep_radar <- cbind(group = as.factor(c("Mood","Stress","Sleep")),sleep_radar)
ggradar(sleep_radar,
        grid.min = 0, grid.mid = 0.5, grid.max = 1,
        values.radar = c("0", "0.5", "1"),
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
hygiene_radar$Mood[5] <- corr_PAH$Hygiene$corr[5,1]
hygiene_radar$Mood[6] <- corr_PAH$Hygiene$corr[6,1]
hygiene_radar <- hygiene_radar %>% 
  replace(is.na(.),0) %>% 
  t() %>% as.data.frame()
hygiene_radar <- cbind(group = as.factor(c("Mood")),hygiene_radar)
ggradar(hygiene_radar,
        grid.min = 0, grid.mid = 0.5, grid.max = 1,
        values.radar = c("0", "0.5", "1"),
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
work_radar$Stress[1] <- corr_PAH$Work$corr[1,2]
work_radar$Stress[5] <- corr_PAH$Work$corr[5,2]
work_radar$IndivPerf[4] <- corr_PAH$Work$corr[4,4]
work_radar$IndivPerf[5] <- corr_PAH$Work$corr[5,4]
work_radar$IndivPerf[6] <- corr_PAH$Work$corr[6,4]
work_radar$TeamPerf[2] <- corr_PAH$Work$corr[2,5]
work_radar <- work_radar %>% 
  replace(is.na(.),0) %>% 
  t() %>% as.data.frame()
work_radar <- cbind(group = as.factor(c("Stress","IndivPerf","TeamPerf")),work_radar)
ggradar(work_radar,
        grid.min = 0, grid.mid = 0.5, grid.max = 1,
        values.radar = c("0", "0.5", "1"),
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
galley_radar$Mood[1] <- corr_PAH$Galley$corr[1,1]
galley_radar$Mood[4] <- corr_PAH$Galley$corr[4,1]
galley_radar$Mood[5] <- corr_PAH$Galley$corr[5,1]
galley_radar$Social[1] <- corr_PAH$Galley$corr[1,6]
galley_radar$Social[2] <- corr_PAH$Galley$corr[2,6]
galley_radar$Social[6] <- corr_PAH$Galley$corr[6,6]
galley_radar <- galley_radar %>% 
  replace(is.na(.),0) %>% 
  t() %>% as.data.frame()
galley_radar <- cbind(group = as.factor(c("Mood","Social")),galley_radar)
ggradar(galley_radar,
        grid.min = 0, grid.mid = 0.5, grid.max = 1,
        values.radar = c("0", "0.5", "1"),
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

##### RADAR PLOTS - LM COEFFS #####
# run lm model, use coeffs as magnitude for radar plots
# 1 sleep
# 2 hygiene
# 3 work
# 4 kitchen
model_res <- habitability_moderator_model(SHAQ_HERA, 4, "TeamPerf")
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

##### ARCHIVE - visualize 'why's' over mission time #####
# to identify the impact of individual variation for motivating mixed effects linear model
# make label vector to reference
PAH_lab <- c("Privacy", "Social Density", "Efficiency","Control","Comfort", "Convenience")
hab_areas_lab <- c("Sleep","Hygiene","Work","Galley")
plot_PAHs <- function(data, hab_area, BHP_outcome, PAH){
   ggplot(data %>% subset(SHAQ_Hab_Area == hab_area & BHP_Outcome == BHP_outcome), 
         aes(x = MissionDay, y = !!sym(PAH), group = ID)) + 
    # geom_point() + 
    geom_line(size = 2.5, alpha = 0.2, position=position_jitter(w=0, h=0.05)) +
    # geom_line(size = 0.3, aes(color = ID)) +
  
   labs(x = "Mission Day", y = PAH_lab[as.numeric(unlist(strsplit(PAH, "_"))[2])]) + 
    ggtitle(paste("[SHAQ]",hab_areas_lab[hab_area],"Area's Effect on",BHP_outcome,"by Individual",sep=" "))
  
}

# plot, assign data source, hab area, bhp outcome, and PAH of interest
plot_PAHs(SHAQ_HERA, 4, "Mood", "Why_2")

##### ARCHIVE - visualize 'why's' over mission time, grouped by variables #####
plot_PAHs_grouped <- function(data, campaign, mission, hab_area, BHP_outcome, PAH){
  ggplot(data %>% subset(SHAQ_Hab_Area == hab_area & BHP_Outcome == BHP_outcome & Campaign == campaign & Mission == mission), 
         aes(x = MissionDay, y = !!sym(PAH)), group_by = Role) + 
    # geom_point() + 
    geom_line(aes(x = MissionDay, y = !!sym(PAH), color = Role), size = 2.5, alpha = 0.5) +
    ylim(c(1,7))+
    # geom_line(size = 0.3, aes(color = ID)) +
    labs(x = "Mission Day", y = PAH_lab[as.numeric(unlist(strsplit(PAH, "_"))[2])]) + 
    ggtitle(paste("C", campaign, "M", mission, hab_areas_lab[hab_area],"Area's Effect on",BHP_outcome,"by Individual")) +
    theme(legend.position="none")
  
}

plot_PAHs_grouped(SHAQ_HERA, 5,4, 2, "Mood", "Why_1")

# for campaigns 5, 6, missions 1-4
for (c in 5:6) {
  for (m in 1:4){
    print(plot_PAHs_grouped(SHAQ_HERA, c,m, 2, "Mood", "Why_5"))
  }
  
}

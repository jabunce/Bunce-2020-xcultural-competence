# Load packages
rm (list = ls(all=TRUE))
library(rstan)
library(rethinking)
library(lattice)
library(MASS)
library(graphics)
library(grid)
library(boot)      #for inverse logit function

# Set local directory: the path to the folder containing the subfolders Code, Plots, and Data
#setwd("/Users/johnbunce/Dropbox/Matsigenka-Mestizo_project_2014/perception_analysis/xcult_dynamics/R_scripts/xcult_comp_dynamics_model_git_1nov20")
setwd("C:/Users/jabunce/Dropbox/Matsigenka-Mestizo_project_2014/perception_analysis/xcult_dynamics/R_scripts/xcult_comp_dynamics_model_git_1nov20")
#setwd("~/xcult_comp_dynamics_model_29oct20")

# Load helper functions
source("./Code/Functions.R")

# Format data
source("./Code/FormatData.R")


# Fit main stan models: m1, m2, m3, and m4 
samps <- 2000 	#number of mcmc samples
num_chains <- 4 #number of mcmc chains
source("./Code/MainStanModels.R")

#load stan output if you've already fit the models
#m1 <- readRDS("m1_fit.rds")
#m2 <- readRDS("m2_fit.rds") 
#m3 <- readRDS("m3_fit.rds") 
#m4 <- readRDS("m4_fit.rds") 

#Make the plots in the manuscript
source("./Code/PlotModelParams.R")


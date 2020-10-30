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
#setwd("/Users/johnbunce/Dropbox/Matsigenka-Mestizo_project_2014/perception_analysis/xcult_dynamics/R_scripts/xcult_comp_dynamics_IRT_git_29oct20")
#setwd("C:/Users/jabunce/Dropbox/Matsigenka-Mestizo_project_2014/perception_analysis/xcult_dynamics/R_scripts/xcult_comp_dynamics_IRT_git_29oct20")
setwd("~/Manu_perceptions")

# Load helper functions
source("./Code/Functions_IRT.R")

# Format data
source("./Code/FormatData_IRT.R")


# Fit main stan models: m4, m5, m14 
samps <- 3000 	#number of mcmc samples, change to 1000 and it will run faster
num_chains <- 4 #number of mcmc chains, change to 2 and it will run faster
source("./Code/MainStanModels_IRT.R")

#load stan output if you've already fit the models
#m4 <- readRDS("m4_fit.rds")
#m5 <- readRDS("m5_fit.rds")
#m14 <- readRDS("m14_fit.rds")

# Process stan output
source("./Code/ProcessM4_IRT.R")
source("./Code/ProcessM5_IRT.R")
source("./Code/ProcessM14_IRT.R")

#Make the plots in the manuscript
source("./Code/CombinedPlots_IRT.R")



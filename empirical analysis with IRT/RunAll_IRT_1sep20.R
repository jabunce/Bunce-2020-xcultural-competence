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
#setwd("C:/Users/jabunce/Dropbox/Matsigenka-Mestizo_project_2014/perception_analysis/xcult_dynamics/R_scripts/xcult_comp_dynamics_IRT_1sep20")
#setwd("~/Manu_perceptions")
setwd("/Users/johnbunce/Dropbox/Matsigenka-Mestizo_project_2014/perception_analysis/xcult_dynamics/R_scripts/xcult_comp_dynamics_IRT_1sep20")

# Load helper functions
source("./Code/Functions_IRT_1sep20.R")

# Format data
source("./Code/FormatData_IRT_1sep20.R")


# Fit main stan models: m1, m4, m11, m19 
samps <- 3000 	#number of mcmc samples, change to 1000 and it will run faster
num_chains <- 4 #number of mcmc chains, change to 2 and it will run faster
source("./Code/MainStanModels_IRT_1sep20.R")

#load stan output if you've already fit the models
#m4 <- readRDS("m4_fit.rds") #to read in stanfit output
#m5 <- readRDS("m5_fit.rds") #to read in stanfit output
#m14 <- readRDS("m14_fit.rds") #to read in stanfit output

# Process stan output and make figures
#source("./Code/ProcessM1.R")
source("./Code/ProcessM4_IRT_1sep20.R")
source("./Code/ProcessM5_IRT_1sep20.R")
source("./Code/ProcessM14_IRT_1sep20.R")
source("./Code/CombinedPlots_IRT_1sep20.R")



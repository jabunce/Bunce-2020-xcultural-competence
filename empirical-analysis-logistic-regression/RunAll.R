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
#setwd("/Users/johnbunce/Dropbox/Matsigenka-Mestizo_project_2014/perception_analysis/xcult_dynamics/R_scripts/xcult_comp_dynamics_28aug20")
setwd("~/Manu_perceptions")

# Load helper functions
source("./Code/Functions.R")

# Format data
source("./Code/FormatData.R")


# Fit stan models: m1 and m2 
samps <- 3000 	#number of mcmc samples
num_chains <- 4 #number of mcmc chains
source("./Code/MainStanModels.R")

# Process stan output and make figures
source("./Code/ProcessM1.R")
source("./Code/ProcessM2.R")

#Make the plots in the manuscript
source("./Code/CombinedPlots.R")



library(ggplot2)
library(nnet)
library(ramify)
library(vcdExtra)
library(irr)

# Load data
nfl_players_clean <- read.csv("dataset/nfl_players_clean.csv")

# Compute the confusion matrix
conf_matrix <- table(nfl_players_clean$position_group, prediction)
print("Confusion Matrix:")
print(conf_matrix)

#compute kappa
fleiss_kappa <- kappam.fleiss(conf_matrix)
print(fleiss_kappa)

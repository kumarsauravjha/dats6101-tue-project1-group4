library(ggplot2)
library(nnet)
library(ramify)
library(vcdExtra)
library(irr)


# Load data
nfl_players_clean <- read.csv("dataset/nfl_players_clean.csv")


# Check accuracy
correct <- subset(nfl_players_clean, position_group == prediction)
accuracy <- nrow(correct) / nrow(nfl_players_clean)

# Print accuracy
print(paste("Accuracy:", round(accuracy, 4)))
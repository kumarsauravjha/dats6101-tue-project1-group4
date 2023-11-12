library(ggplot2)
library(ezids)

#Read data
nfl_data = read.csv("dataset/nfl_players_clean.csv")

#Factorise the correct variables
nfl_data$position_group = as.factor(nfl_data$position_group)
nfl_data$position = as.factor(nfl_data$position)
nfl_data$team_abbr = as.factor(nfl_data$team_abbr)
nfl_data$entry_year = as.factor(nfl_data$entry_year)
nfl_data$rookie_year = as.factor(nfl_data$rookie_year)

print(summary(nfl_data$position_group, maxsum = 40))
print(summary(nfl_data$height, maxsum = 40))
print(summary(nfl_data$weight, maxsum = 40))
print(summary(nfl_data$years_of_experience, maxsum = 40))
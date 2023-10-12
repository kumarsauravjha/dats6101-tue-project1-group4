library(ggplot2)

nfl_data = read.csv("dataset/nfl_players_clean.csv")

nfl_data$position_group = as.factor(nfl_data$position_group)
nfl_data$position = as.factor(nfl_data$position)
nfl_data$team_abbr = as.factor(nfl_data$team_abbr)

plt <- ggplot(nfl_data) + geom_boxplot(aes(x=position_group, y=years_of_experience))
print(plt)
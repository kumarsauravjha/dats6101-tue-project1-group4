library(ggplot2)

nfl_data = read.csv("dataset/nfl_players_clean.csv")

nfl_data$position_group = as.factor(nfl_data$position_group)
nfl_data$position = as.factor(nfl_data$position)
nfl_data$team_abbr = as.factor(nfl_data$team_abbr)

plt1 <- ggplot(nfl_data) + geom_histogram(aes(x=years_of_experience), binwidth = 1)
plt2 <- ggplot(nfl_data) + geom_boxplot(aes(y=years_of_experience))
plt3 <- ggplot(nfl_data) + geom_boxplot(aes(x=position_group, y=years_of_experience))
print(summary(nfl_data$years_of_experience))
print(plt1)
print(plt2)
print(plt3)

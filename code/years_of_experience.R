library(ggplot2)

nfl_data = read.csv("dataset/nfl_players_clean.csv")

nfl_data$position_group = as.factor(nfl_data$position_group)
nfl_data$position = as.factor(nfl_data$position)
nfl_data$team_abbr = as.factor(nfl_data$team_abbr)

plt1 <- ggplot(nfl_data) +
  geom_histogram(aes(x=years_of_experience), color="black", fill="yellow", binwidth = 1, show.legend = F) +
  labs(title = "Histogram plot of years of experience for all players") +
  labs(x = "Years of Experience", y="Number of players")

plt2 <- ggplot(nfl_data) +
  geom_boxplot(aes(y=years_of_experience), fill="yellow", show.legend = F) +
  labs(title = "Boxplot of years of experience for all players") +
  labs(y = "Years of experience")

plt3 <- ggplot(nfl_data) +
geom_boxplot(aes(x=position_group, y=years_of_experience, color=position_group), show.legend = F) +
labs(title = "Boxplot of years of experience for each position group") +
labs(x = "Position Group", y = "Years of experience")


print(plt1)
print(plt2)
print(plt3)

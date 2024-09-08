library(ggplot2)
library(dplyr)

nfl_players <- read.csv("dataset/nfl_players_clean.csv")
nfl_players$position_group = as.factor(nfl_players_clean$position_group)
nfl_players$position = as.factor(nfl_players_clean$position)
nfl_players$team_abbr = as.factor(nfl_players_clean$team_abbr)
nfl_players$entry_year = as.factor(nfl_players_clean$entry_year)

# Group the NFL players by position_group
grouped_nfl_players <- group_by(nfl_players, position_group)

# Calculate the average height and weight for each group
nfl_players_by_position <- summarize(grouped_nfl_players,average_height = mean(height),average_weight = mean(weight), average_years_active = mean(years_of_experience))

# Create a bar chart of the average height for each position
plot1<-ggplot(nfl_players_by_position, aes(x = position_group, y = average_height)) +
  geom_bar(stat = "identity", fill ="darkblue") +
  labs(title = "Average Height of NFL Players by Position", x = "Position Group", y = "Height (inches)")
print(plot1)

# Create a bar chart of the average weight for each position
plot2<-ggplot(nfl_players_by_position, aes(x = position_group, y = average_weight)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Average Weight of NFL Players by Position", x = "Position Group", y = "Weight (pounds)")
print(plot2)

# Create a scatterplot of height and weight for each position
plot3<-ggplot(nfl_players, aes(x = height, y = weight, color = position_group)) + 
  geom_point() +
  labs(title = "Scatterplot of Height and Weight of NFL Players by Position", x = "Height (inches)", y = "Weight (pounds)", color = "Position Group")
print(plot3)

#Group NFL players by team
grouped_nfl_players2 <- group_by(nfl_players, team_abbr)

# Calculate the average height and weight for each team
nfl_players_by_team <- summarize(grouped_nfl_players2, average_height = mean(height), average_weight = mean(weight))

# Calculate the average height and weight for the entire league
league_average_height <- mean(nfl_players$height)
league_average_weight <- mean(nfl_players$weight)

# Create a bar chart of the average height for each team
plot4<-ggplot(nfl_players_by_team, aes(x = team_abbr, y = average_height)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_hline(yintercept = league_average_height, color = "red") +
  labs(title = "Average Height of NFL Players by Team", x = "Team", y = "Height (inches)")
print(plot4)

# Create a bar chart of the average weight for each team
plot5<-ggplot(nfl_players_by_team, aes(x = team_abbr, y = average_weight)) +
  geom_bar(stat = "identity", fill = "purple") +
  geom_hline(yintercept = league_average_weight, color = "red") +
  labs(title = "Average Weight of NFL Players by Team", x = "Team", y = "Weight (pounds)")
print(plot5)

# Create a bar chart of the average number of years that players at each position have been active
plot6<-ggplot(nfl_players_by_position, aes(x = position_group, y = average_years_active)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Average Number of Years Active of NFL Players by Position", x = "Position Group", y = "Years Active")
print(plot6)


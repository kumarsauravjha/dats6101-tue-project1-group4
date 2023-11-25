---
  title: "NFL_data_EDA"
output:
  pdf_document:
  keep_tex: true
---
  
nfl_players <- read.csv("nfl_players_clean.csv")
summary(nfl_players)
library(ggplot2)


# Group the NFL players by position_group
grouped_nfl_players <- group_by(nfl_players, position_group)

# Calculate the average height and weight for each group
nfl_players_by_position <- summarize(grouped_nfl_players,average_height = mean(height),average_weight = mean(weight))

# Create a bar chart of the average height for each position
ggplot(nfl_players_by_position, aes(x = position_group, y = average_height)) +
  geom_bar(stat = "identity", fill ="darkblue") +
  labs(title = "Average Height of NFL Players by Position", x = "Position Group", y = "Height (inches)")

# Create a bar chart of the average weight for each position
ggplot(nfl_players_by_position, aes(x = position_group, y = average_weight)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Average Weight of NFL Players by Position", x = "Position Group", y = "Weight (pounds)")

# Create a scatterplot of height and weight for each position
ggplot(nfl_players, aes(x = height, y = weight, color = position_group)) + 
  geom_point() +
  labs(title = "Scatterplot of Height and Weight of NFL Players by Position", x = "Height (inches)", y = "Weight (pounds)", color = "Position Group")


# Calculate the average height and weight for each team
nfl_players_by_team <- data.frame(
  team_abbr = nfl_players$team_abbr,
  average_height = ave(nfl_players$height, nfl_players$team_abbr),
  average_weight = ave(nfl_players$weight, nfl_players$team_abbr)
)

# Calculate the average height and weight for the entire league
league_average_height <- mean(nfl_players$height)
league_average_weight <- mean(nfl_players$weight)

# Create a bar chart of the average height for each team
ggplot(nfl_players_by_team, aes(x = team_abbr, y = average_height)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_hline(yintercept = league_average_height, color = "red") +
  labs(title = "Average Height of NFL Players by Team", x = "Team", y = "Height (inches)")

# Create a bar chart of the average weight for each team
ggplot(nfl_players_by_team, aes(x = team_abbr, y = average_weight)) +
  geom_bar(stat = "identity", fill = "purple") +
  geom_hline(yintercept = league_average_weight, color = "red") +
  labs(title = "Average Weight of NFL Players by Team", x = "Team", y = "Weight (pounds)")


# Calculate the average number of years that players at each position have been active
nfl_players_by_position <- data.frame(
  position_group = nfl_players$position_group,
  average_years_active <-ave(nfl_players$years_of_experience, nfl_players$position_group)
)
print(nfl_players_by_position)

# Create a bar chart of the average number of years that players at each position have been active
ggplot(nfl_players_by_position, aes(x = position_group, y = average_years_active)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Average Number of Years Active of NFL Players by Position", x = "Position Group", y = "Years Active")


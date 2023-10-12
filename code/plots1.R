

nfl_players_clean=read.csv("dataset/nfl_players_clean.csv")
head(nfl_players_clean)
summary(nfl_players_clean)

library(ggplot2)



# Create a histogram for the "Weight" variable
ggplot(nfl_players_clean, aes(x = weight)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Player Weights", x = "Weight (lbs)", y = "Frequency")

# Create a histogram for the "Height" variable
ggplot(nfl_players_clean, aes(x = height)) +
  geom_histogram(binwidth = 1, fill = "lightcoral", color = "black") +
  labs(title = "Distribution of Player Heights", x = "Height (inches)", y = "Frequency")

#create a bar plot for player positions
ggplot(nfl_players_clean, aes(x = position)) +
  geom_bar(fill = "orange") +
  labs(title = "Position Distribution")


# Create box plots for height by position
ggplot(nfl_players_clean, aes(x = position, y = height, fill = position)) +
  geom_boxplot() +
  labs(title = "Box Plot of Height by Position")

# Create box plots for weight by position
ggplot(nfl_players_clean, aes(x = position, y = weight, fill = position)) +
  geom_boxplot() +
  labs(title = "Box Plot of Weight by Position")


# Summary statistics of the entire dataset
summary(nfl_players_clean$height)
summary(nfl_players_clean$weight)




















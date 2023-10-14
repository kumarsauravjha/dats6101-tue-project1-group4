

nfl_players_clean=read.csv("dataset/nfl_players_clean.csv")
nfl_players_clean$position_group = as.factor(nfl_players_clean$position_group)
nfl_players_clean$position = as.factor(nfl_players_clean$position)
nfl_players_clean$team_abbr = as.factor(nfl_players_clean$team_abbr)
nfl_players_clean$entry_year = as.factor(nfl_players_clean$entry_year)
head(nfl_players_clean)
summary(nfl_players_clean)

library(ggplot2)



# Create a histogram for the "Weight" variable
histw<-ggplot(nfl_players_clean, aes(x = weight)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Player Weights", x = "Weight (lbs)", y = "Frequency")
print(histw)


# Create a histogram for the "Height" variable
histh<-ggplot(nfl_players_clean, aes(x = height)) +
  geom_histogram(binwidth = 1, fill = "lightcoral", color = "black") +
  labs(title = "Distribution of Player Heights", x = "Height (inches)", y = "Frequency")
print(histh)


#create a bar plot for player positions
gg1<-ggplot(nfl_players_clean, aes(x = position_group)) +
  geom_bar(fill = "orange") +
  labs(title = "Position Distribution")
print(gg1)


# Create box plots for height by position
boxh<-ggplot(nfl_players_clean, aes(x = position_group, y = height, fill = position_group)) +
  geom_boxplot() +
  labs(title = "Box Plot of Height by Position")
print(boxh)


# Create box plots for weight by position
boxw<-ggplot(nfl_players_clean, aes(x = position_group, y = weight, fill = position_group)) +
  geom_boxplot() +
  labs(title = "Box Plot of Weight by Position")
print(boxw)

#scatter plot for height,weight and entry year
sca1<-ggplot(nfl_players_clean, aes(x = weight, y = height, color = entry_year)) +
geom_point()
print(sca1)

#scatter plot for height and entry year
sca2<-ggplot(nfl_players_clean, aes(x = entry_year, y = height)) +
  geom_point() +
  labs(x = "Entry Year", y = "Height") +
  ggtitle("Height vs. Entry Year")
print(sca2)



#scatter plot for weight and entry year
sca3<-ggplot(nfl_players_clean, aes(x = entry_year, y = weight)) +
  geom_point() +
  labs(x = "Entry Year", y = "Height") +
  ggtitle("weight vs. Entry Year")
print(sca3)

  library(dplyr)
#Bar plot for average height per year
avgh<-data_summary <- nfl_players_clean %>%
    group_by(entry_year) %>%
    summarize(mean_height = mean(height))
  
print(ggplot(data_summary, aes(x = factor(entry_year), y = mean_height)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(x = "Entry Year", y = "Mean Height") +
    ggtitle("Mean Height per Entry Year"))
  
print(avgh)

  #Bar plot for average weight per year
avgw<- data_summary <- nfl_players_clean %>%
    group_by(entry_year) %>%
    summarize(mean_weight = mean(weight))
  
print(ggplot(data_summary, aes(x = factor(entry_year), y = mean_weight)) +
    geom_bar(stat = "identity", fill = "red") +
    labs(x = "Entry Year", y = "Mean Height") +
    ggtitle("Mean weight per Entry Year"))
print(avgw)
  











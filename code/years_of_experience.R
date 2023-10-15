library(ggplot2)
library(ezids)

#Read data
nfl_data = read.csv("dataset/nfl_players_clean.csv")

#Factorise the correct variables
nfl_data$position_group = as.factor(nfl_data$position_group)
nfl_data$position = as.factor(nfl_data$position)
nfl_data$team_abbr = as.factor(nfl_data$team_abbr)

#Histogram plot league wide
plt1 <- ggplot(nfl_data) +
  geom_histogram(aes(x=years_of_experience), color="black", fill="yellow", binwidth = 1, show.legend = F) +
  labs(title = "Histogram plot of years of experience for all players") +
  labs(x = "Years of Experience", y="Number of players")

#Boxplot league wide
plt2 <- ggplot(nfl_data) +
  geom_boxplot(aes(y=years_of_experience), fill="yellow", show.legend = F) +
  labs(title = "Boxplot of years of experience for all players") +
  labs(y = "Years of experience")

#Boxplot by position
plt3 <- ggplot(nfl_data) +
  geom_boxplot(aes(x=position_group, y=years_of_experience, color=position_group), show.legend = F) +
  labs(title = "Boxplot of years of experience for each position group") +
  labs(x = "Position Group", y = "Years of experience")

#Boxplot by team
plt4 <- ggplot(nfl_data) +
  geom_boxplot(aes(x=team_abbr, y=years_of_experience, color=team_abbr), show.legend = F) +
  labs(title = "Boxplot of years of experience for each team") +
  labs(x = "Team", y = "Years of experience")

print(plt1)
print(plt2)
print(plt3)
print(plt4)

# Take subset of players with highest median
dl_players <- subset(nfl_data, nfl_data$position_group == "DL")
# print(summary(dl_players))

# Take subset of players with lowest median
ol_players <- subset(nfl_data, nfl_data$position_group == "OL")
# print(summary(ol_players))


#Hypothesis tests
#Position group vs YOE
pos_yoe <- aov(years_of_experience ~ position_group, nfl_data)
print(xkabledply(pos_yoe))

pos_yoe_tukey <- TukeyHSD(pos_yoe)
print(pos_yoe_tukey)

#Team vs YOE
team_yoe <- aov(years_of_experience ~ team_abbr, nfl_data)
print(xkabledply(team_yoe))


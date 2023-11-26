

nfl_players_clean=read.csv("dataset/nfl_players_clean.csv")

#correalation plots for position group vs ht and wt

# Create a box plot for weight and position group
box_wt<-boxplot(weight ~ position_group, data = nfl_players_clean, col = "red", main = "Boxplot of weight by position group")
print(box_wt)


#craete a boxplot for height and position group

box_ht<-boxplot(height ~ position_group, data = nfl_players_clean, col = "blue", main = "Boxplot of weight by position group")
print(box_ht)



library(ggplot2)

# Create a violin plot
voilon_wt<-ggplot(nfl_players_clean, aes(x = weight, y = position_group, fill = weight)) +
  geom_violin() +
  labs(title = "Violin Plot of weight and position group")
print(voilon_wt)

voilon_ht<-ggplot(nfl_players_clean, aes(x = height, y = position_group, fill = height)) +
  geom_violin() +
  labs(title = "Violin Plot of height and position group")
print(voilon_ht)



scatter_ht<-ggplot(nfl_players_clean, aes(x = position_group, y = height, color = position_group)) +
  geom_jitter() +
  labs(title = "Jittered Scatter Plot of position group by height")
print(scatter_ht)


scatter_wt<-ggplot(nfl_players_clean, aes(x = position_group, y = weight, color = position_group)) +
  geom_jitter() +
  labs(title = "Jittered Scatter Plot of position group by weight")
print(scatter_wt)


strip_wt<-ggplot(nfl_players_clean, aes(x = position_group, y = weight, color = position_group)) +
  geom_point(position = position_jitter(width = 0.2), size = 3) +
  labs(title = "Strip Chart of position group by weight")
print(strip_wt)

strip_ht<-ggplot(nfl_players_clean, aes(x = position_group, y = height, color = position_group)) +
  geom_point(position = position_jitter(width = 0.2), size = 3) +
  labs(title = "Strip Chart of position group by height")
print(strip_ht)



faceted_wt<-ggplot(nfl_players_clean, aes(x = position_group, y = weight, color = position_group)) +
  geom_point() +
  facet_wrap(~position_group) +
  labs(title = "Faceted Scatter Plots of position group by weight")
print(faceted_wt)

faceted_ht<-ggplot(nfl_players_clean, aes(x = position_group, y = height, color = position_group)) +
  geom_point() +
  facet_wrap(~position_group) +
  labs(title = "Faceted Scatter Plots of position group by height")
print(faceted_ht)


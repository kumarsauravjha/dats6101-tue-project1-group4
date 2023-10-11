
NFL_data <- read.csv("nfl_players_clean.csv")
summary(NFL_data)
library(ggplot2)
scatterplot<-ggplot(NFL_data, aes(x = height, y = weight, color = position)) +
  geom_point() + labs(x = "Height", y = "Weight") + ggtitle("Scatter Plot of Height vs. Weight by Position") +
  theme_minimal()
print(scatterplot)

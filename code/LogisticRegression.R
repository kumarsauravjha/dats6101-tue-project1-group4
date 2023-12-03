library(ggplot2)
library(nnet)
library(ramify)

nfl_players_clean <- read.csv("dataset/nfl_players_clean.csv")

nfl_players_clean$position_group <- factor(nfl_players_clean$position_group)
fit_basic <- multinom(position_group ~ height + weight + years_of_experience, data=nfl_players_clean)

print(summary(fit_basic))
print(exp(coef(fit_basic)))

pp <- fitted(fit_basic)
prediction <- argmax(pp)

prediction[prediction == 1] = "DB"
prediction[prediction == 2] = "DL"
prediction[prediction == 3] = "LB"
prediction[prediction == 4] = "OL"
prediction[prediction == 5] = "QB"
prediction[prediction == 6] = "RB"
prediction[prediction == 7] = "SPEC"
prediction[prediction == 8] = "TE"
prediction[prediction == 9] = "WR"

nfl_players_clean$prediction <- prediction

correct <- subset(nfl_players_clean, nfl_players_clean$position_group == nfl_players_clean$prediction)
accuracy = nrow(correct) / nrow(nfl_players_clean)

print(accuracy)
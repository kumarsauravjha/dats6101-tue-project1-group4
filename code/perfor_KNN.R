library(caret)

nfl_players_clean <- read.csv("dataset/nfl_players_clean.csv")

cm <- confusionMatrix(preds, test_data$position_group)

accuracy <- cm$overall["Accuracy"]

cat("Accuracy:", accuracy, "\n")
print(accuracy)

kappa <- cm$overall["Kappa"]
cat("Kappa:", kappa, "\n")



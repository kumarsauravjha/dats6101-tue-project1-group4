library(ggplot2)
library(randomForest)
library(ezids)

nfl_clean <- read.csv("nfl_players_clean_bmi.csv")

head(nfl_clean)

# Create a scatter plot with points colored by position_group
ggplot(nfl_clean, aes(x = BMI, y = years_of_experience, color = position_group)) +
  geom_point() +
  ggtitle("Scatter Plot of BMI vs. Years of Experience") +
  xlab("BMI") +
  ylab("Years of Experience")

#----------------------------LM

set.seed(123)

# Split the dataset into training (80%) and testing (20%) sets
train_indices <- sample(1:nrow(nfl_clean), 0.8 * nrow(nfl_clean))
train_data <- nfl_clean[train_indices, ]
test_data <- nfl_clean[-train_indices, ]

lm_model <- lm(years_of_experience ~ position_group + BMI, data = train_data)

summary(lm_model)

# Make predictions on the test set
predictions <- predict(lm_model, newdata = test_data)

# Compare predictions with actual values
comparison <- data.frame(Actual = test_data$years_of_experience, Predicted = predictions)
print(head(comparison))

# Calculate RMSE
rmse <- sqrt(mean((predictions - test_data$years_of_experience)^2))

# Print the RMSE
cat("Root Mean Squared Error (RMSE):", rmse, "\n")


#---------------------------Random Forest
# Train the Random Forest model
rf_model <- randomForest(
  years_of_experience ~ position_group + BMI,
  data = train_data,
  ntree = 500,  # Number of trees in the forest
  mtry = 3,     # Number of variables randomly sampled as candidates at each split
  importance = TRUE
)

# Summary of the model
print(rf_model)

# Make predictions on the test set
predictions_rf <- predict(rf_model, newdata = test_data)

# Continue with the comparison and RMSE calculation as before
comparison_rf <- data.frame(Actual = test_data$years_of_experience, Predicted = predictions_rf)
print(head(comparison_rf))

# Calculate RMSE
rmse_rf <- sqrt(mean((predictions_rf - test_data$years_of_experience)^2))
cat("Root Mean Squared Error (RMSE) - Random Forest:", rmse_rf, "\n")




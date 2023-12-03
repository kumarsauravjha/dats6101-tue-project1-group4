library(ggplot2)

#read the cleaned dataset
nfl_clean <- read.csv("nfl_players_clean.csv")
summary(nfl_clean)

head(nfl_clean)

# Check data types of all variables in the dataset
data_types <- sapply(nfl_clean, class)

# Print the result
print(data_types)

#converting into categorical variables into factors
nfl_clean$position_group = as.factor(nfl_clean$position_group)
nfl_clean$position = as.factor(nfl_clean$position)
nfl_clean$team_abbr = as.factor(nfl_clean$team_abbr)

unique_position_groups <- unique(nfl_clean$position_group)

# Get the number of unique position groups
num_unique_position_groups <- length(unique_position_groups)

# Print the result
cat("Number of unique position groups:", num_unique_position_groups, "\n")

#unique position groups print
cat("Unique position groups:", unique_position_groups, "\n")

unique_position <- unique(nfl_clean$position)

# Get the number of unique positions
num_unique_position <- length(unique_position)

# Print the result
cat("Number of unique position groups:", num_unique_position, "\n")

#unique position groups print
cat("Unique position groups:", unique_position, "\n")

#Now we'll model YOE based on the physical characteristic and position played by the player

# Fit linear regression model
lm_model <- lm(years_of_experience ~ position_group + height + weight, data = nfl_clean)

# Summary of the model
summary(lm_model)

#to be continued....

#splitting the dataset for training and testing
# Set a seed for reproducibility
set.seed(123)

# Split the dataset into training (80%) and testing (20%) sets
train_indices <- sample(1:nrow(nfl_clean), 0.8 * nrow(nfl_clean))
train_data <- nfl_clean[train_indices, ]
test_data <- nfl_clean[-train_indices, ]

# Build the linear regression model on the training set
lm_model_new <- lm(years_of_experience ~ position_group + height + weight, data = train_data)

# Summary of the model
summary(lm_model_new)

# Make predictions on the test set
predictions <- predict(lm_model_new, newdata = test_data)

# Compare predictions with actual values
comparison <- data.frame(Actual = test_data$years_of_experience, Predicted = predictions)
print(head(comparison))

# Calculate RMSE
rmse <- sqrt(mean((predictions - test_data$years_of_experience)^2))

# Print the RMSE
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

---------------------------------------------------
  
# Create interaction term between height and weight for the entire dataset
nfl_clean$height_weight_interaction <- nfl_clean$height * nfl_clean$weight

# Split the dataset into training (80%) and testing (20%) sets
#set.seed(123)  # for reproducibility
#train_indices <- sample(1:nrow(nfl_clean), 0.8 * nrow(nfl_clean))
train_data1 <- nfl_clean[train_indices, ]
test_data1 <- nfl_clean[-train_indices, ]

# Build the model with interaction term on the training set
lm_model_interaction <- lm(years_of_experience ~ position_group + height + weight + height_weight_interaction, data = train_data1)

# Summary of the model with interaction term
summary(lm_model_interaction)

# Make predictions on the test set
predictions_new <- predict(lm_model_interaction, newdata = test_data1)

# Continue with the comparison and RMSE calculation as before
comparison_new <- data.frame(Actual = test_data1$years_of_experience, Predicted = predictions_new)
print(head(comparison_new))

# Calculate RMSE
rmse_new <- sqrt(mean((predictions_new - test_data1$years_of_experience)^2))
cat("Root Mean Squared Error (RMSE):", rmse_new, "\n")

#so not much difference in RMSE with or without interaction term

#----------------------------------------------------
# library(randomForest)

# Split the dataset into training (80%) and testing (20%) sets
# set.seed(123)  # for reproducibility
# train_indices <- sample(1:nrow(nfl_clean), 0.8 * nrow(nfl_clean))
# train_data1 <- nfl_clean[train_indices, ]
# test_data1 <- nfl_clean[-train_indices, ]

# Train the Random Forest model without the interaction term
rf_model <- randomForest(
  years_of_experience ~ position_group + height + weight,
  data = train_data1,
  ntree = 500,  # Number of trees in the forest
  mtry = 3,     # Number of variables randomly sampled as candidates at each split
  importance = TRUE
)

# Summary of the model
print(rf_model)

# Make predictions on the test set
predictions_rf <- predict(rf_model, newdata = test_data1)

# Continue with the comparison and RMSE calculation as before
comparison_rf <- data.frame(Actual = test_data1$years_of_experience, Predicted = predictions_rf)
print(head(comparison_rf))

# Calculate RMSE
rmse_rf <- sqrt(mean((predictions_rf - test_data1$years_of_experience)^2))
cat("Root Mean Squared Error (RMSE) - Random Forest:", rmse_rf, "\n")
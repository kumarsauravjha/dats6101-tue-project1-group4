

# Convert height and weight to numeric 
nfl_data_clean$height <- as.numeric(nfl_data_clean$height)
nfl_data_clean$weight <- as.numeric(nfl_data_clean$weight)

# Create a function to compute correlation matrix for height and weight
compute_correlation_matrix <- function(group_data) {
  cor_matrix <- cor(group_data[, c("height", "weight")])
  return(cor_matrix)
}

# Apply the function to each position group
correlation_matrices <- by(
  nfl_data_clean[, c("height", "weight")],
  nfl_data_clean$position_group,
  compute_correlation_matrix
)

# Convert the result to a list for better readability
correlation_list <- as.list(correlation_matrices)

# Print the correlation matrices
print(correlation_list)









# Get unique position groups
position_groups <- unique(nfl_data_clean$position_group)

# Initialize an empty data frame to store correlation coefficients
correlation_results <- data.frame(Position_Group = character(),
                                  Pearson_Correlation_Height_Weight = numeric(),
                                  stringsAsFactors = FALSE)

# Loop through each position group
for (group in position_groups) {
  # Subset data for the current position group
  data_subset <- subset(nfl_data_clean, position_group == group)
  
  # Calculate Pearson correlation coefficient
  correlation_coefficient <- cor(data_subset$height, data_subset$weight, method = "pearson")
  
  # Add results to the data frame
  correlation_results <- rbind(correlation_results,
                               data.frame(Position_Group = group,
                                          Pearson_Correlation_Height_Weight = correlation_coefficient))
}

# Print the results
print(correlation_results)










library(ggplot2)

# Plot the bar graph
ggplot(correlation_results, aes(x = Position_Group, y = Pearson_Correlation_Height_Weight)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  labs(title = "Pearson Correlation Between Height and Weight by Position Group",
       x = "Position Group",
       y = "Pearson Correlation Coefficient") +
  theme_minimal()



# Pairwise correlation plot
correlation_data <- cor(nfl_data_clean[, c("height", "weight")])


# Assuming 'correlation_data' is a matrix
library(corrplot)

corrplot(correlation_data, method = "color", tl.col = "black", tl.srt = 45)





# Create a pairs plot
pairs(nfl_data_clean[, c("height", "weight", "position_group")],
      col = c("lightblue", "darkblue")[as.numeric(nfl_data_clean$position_group)],
      pch = 19)



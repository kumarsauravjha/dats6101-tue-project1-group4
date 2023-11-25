
nfl_data_clean = read.csv("nfl_players.csv")

# Convert height, weight, and years_of_experience to numeric 
nfl_data_clean$height <- as.numeric(nfl_data_clean$height)
nfl_data_clean$weight <- as.numeric(nfl_data_clean$weight)
nfl_data_clean$years_of_experience <- as.numeric(nfl_data_clean$years_of_experience)

# Create a function to compute correlation matrix for height, weight, and years_of_experience
compute_correlation_matrix <- function(group_data) {
  cor_matrix <- cor(group_data[, c("height", "weight", "years_of_experience")])
  return(cor_matrix)
}

# Apply the function to each position group
correlation_matrices <- by(
  nfl_data_clean[, c("height", "weight", "years_of_experience")],
  nfl_data_clean$position_group,
  compute_correlation_matrix
)
print(correlation_matrices)

# Convert the result to a list for better readability
correlation_list <- as.list(correlation_matrices)

# Print the correlation matrices
print(correlation_list)

# Plot the correlation matrices using corrplot
library(corrplot)

# Function to plot correlation matrix
plot_correlation_matrix <- function(cor_matrix, position_group) {
  corrplot(cor_matrix, method = "color", tl.col = "black", tl.srt = 45,
           main = paste("Correlation Matrix for", position_group))
}
print(plot_correlation_matrix)

# Apply the plotting function to each correlation matrix
mapply(plot_correlation_matrix, correlation_list, names(correlation_list))







# Get unique position groups
position_groups <- unique(nfl_data_clean$position_group)

correlation_results <- data.frame(Position_Group = character(),
                                  Pearson_Correlation_Height_Weight = numeric(),
                                  Pearson_Correlation_Height_Years = numeric(),
                                  Pearson_Correlation_Weight_Years = numeric(),
                                  stringsAsFactors = FALSE)
print(correlation_results)

# Loop through each position group
for (group in position_groups) {
  # Subset data for the current position group
  data_subset <- subset(nfl_data_clean, position_group == group)
  
  # Calculate Pearson correlation coefficients
  correlation_height_weight <- cor(data_subset$height, data_subset$weight, method = "pearson")
  correlation_height_years <- cor(data_subset$height, data_subset$years_of_experience, method = "pearson")
  correlation_weight_years <- cor(data_subset$weight, data_subset$years_of_experience, method = "pearson")
  
  # Add results to the data frame
  correlation_results <- rbind(correlation_results,
                               data.frame(Position_Group = group,
                                          Pearson_Correlation_Height_Weight = correlation_height_weight,
                                          Pearson_Correlation_Height_Years = correlation_height_years,
                                          Pearson_Correlation_Weight_Years = correlation_weight_years))
}

# Print the results
print(correlation_results)







# Calculate correlation coefficients for height, weight, and years_of_experience by position group
correlation_results_years <- data.frame(Position_Group = character(),
                                        Pearson_Correlation_Height_Weight = numeric(),
                                        Pearson_Correlation_Height_Years = numeric(),
                                        Pearson_Correlation_Weight_Years = numeric(),
                                        stringsAsFactors = FALSE)
print(correlation_results_years)


# Get unique position groups
position_groups <- unique(nfl_data_clean$position_group)

# Loop through each position group
for (group in position_groups) {
  # Subset data for the current position group
  data_subset <- subset(nfl_data_clean, position_group == group)
  
  # Calculate Pearson correlation coefficients
  correlation_coefficient_height_weight <- cor(data_subset$height, data_subset$weight, method = "pearson")
  correlation_coefficient_height_years <- cor(data_subset$height, data_subset$years_of_experience, method = "pearson")
  correlation_coefficient_weight_years <- cor(data_subset$weight, data_subset$years_of_experience, method = "pearson")
  
  # Add results to the data frame
  correlation_results_years <- rbind(correlation_results_years,
                                     data.frame(Position_Group = group,
                                                Pearson_Correlation_Height_Weight = correlation_coefficient_height_weight,
                                                Pearson_Correlation_Height_Years = correlation_coefficient_height_years,
                                                Pearson_Correlation_Weight_Years = correlation_coefficient_weight_years))
}

# Print the correlation results with years_of_experience
print(correlation_results_years)



# Plot the bar graph with pearson correlation height weight
plot1 <- ggplot(correlation_results_years, aes(x = Position_Group, y = Pearson_Correlation_Height_Weight)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  labs(title = "Pearson Correlation Between Height and Weight by Position Group",
       x = "Position Group",
       y = "Pearson Correlation Coefficient") +
  theme_minimal()

print(plot1)

# Create a data frame for correlation matrix including years_of_experience
correlation_data_years <- cor(nfl_data_clean[, c("height", "weight", "years_of_experience")])

# Plot the correlation plot using corrplot
plot2 <- corrplot(correlation_data_years, method = "color", tl.col = "black", tl.srt = 45)

print(plot2)


library(ggplot2)

nfl <- read.csv("dataset/nfl_players_clean.csv")
summary(nfl)

nfl$position <- as.factor(nfl$position)
nfl$position_group <- as.factor(nfl$position_group)
nfl$team_abbr <- as.factor(nfl$team_abbr)
summary(nfl)

#perform ANOVA test to check significant differences in height across different position groups
anova_model_ht <- aov(height~position_group, nfl)
print(xkabledply(anova_model_ht, title="ANOVA test Height vs Position Group"))
anova_summ_ht <- summary(anova_model_ht)
p_val_ht <- anova_summ_ht[[1]]$`Pr(>F)`[1]

cat("H0: The mean height is same across different position groups")
cat("\nH1: The mean height is not same across different position groups")

if (p_val_ht < 0.05) {
  cat("\nBased on the ANOVA test, we reject the null hypothesis.\n")
  pos_ht_tukey <- TukeyHSD(anova_model_ht)
  print(pos_ht_tukey)
} else {
  cat("\nBased on the ANOVA test, we cannot reject the null hypothesis.")
}

#perform ANOVA test to check significant differences in weight across different position groups
anova_model_wt <- aov(weight~position_group, nfl)
print(xkabledply(anova_model_wt, title="ANOVA test Weight vs Position Group"))
anova_summ_wt <- summary(anova_model_wt)
p_val_wt <- anova_summ_wt[[1]]$`Pr(>F)`[1]

cat("H0: The mean weight is same across different position groups")
cat("\nH1: The mean weight is not same across different position groups")

if (p_val_wt < 0.05) {
  cat("\nBased on the ANOVA test, we reject the null hypothesis.\n")
  pos_wt_tukey <- TukeyHSD(anova_model_wt)
  print(pos_wt_tukey)
} else {
  cat("\nBased on the ANOVA test, we cannot reject the null hypothesis.")
}


#custom palette for the main position groups
my_colors <- c("red", "blue", "green", "purple", "orange", "pink", "cyan", "brown", "gray")

#
height_plot <- ggplot(nfl, aes(x = position_group, y = height)) +
  geom_boxplot(fill=my_colors,show.legend = FALSE, outlier.color = "red", width = 0.5) +
  scale_fill_manual(values = c(my_colors, "gray")) +  # Add a color for "Other"
  labs(title = "Distribution of Height by Position Group",
       x = "Position Group",
       y = "Height (inches)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),  # Adjust title size and center it
        axis.title.x = element_text(size = 14),               # Adjust x-axis label size
        axis.title.y = element_text(size = 14),               # Adjust y-axis label size
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1))  # Rotate x-axis labels

# Print the height plot
print(height_plot)

#
weight_plot <- ggplot(nfl, aes(x = position_group, y = weight)) +
  geom_boxplot(fill=my_colors,show.legend = FALSE, outlier.color = "red", width = 0.5) +
  scale_fill_manual(values = c(my_colors, "gray")) +  # Add a color for "Other"
  labs(title = "Distribution of Weight by Position Group",
       x = "Position Group",
       y = "Weight (pounds)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),  # Adjust title size and center it
        axis.title.x = element_text(size = 14),               # Adjust x-axis label size
        axis.title.y = element_text(size = 14),               # Adjust y-axis label size
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1))  # Rotate x-axis labels

# Print the weight plot
print(weight_plot)



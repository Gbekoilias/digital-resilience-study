# modeling.R

# Load necessary libraries
library(ggplot2)  # For visualizations
library(dplyr)    # For data manipulation
library(broom)    # For tidying model outputs

# Load the dataset
data <- read.csv("data.csv")

# Check if the dataset loaded successfully
if (nrow(data) == 0) {
    stop("Error: The dataset is empty or could not be loaded.")
}

# Fit a linear model
model <- lm(outcome ~ predictor, data = data)

# Display summary of the model
cat("Model Summary:\n")
model_summary <- summary(model)
print(model_summary)

# Model diagnostics
cat("\nModel Diagnostics:\n")
par(mfrow = c(2, 2))  # Set up a 2x2 plotting area
plot(model)           # Diagnostic plots

# Calculate and display R-squared and Adjusted R-squared
cat("\nR-squared: ", model_summary$r.squared, "\n")
cat("Adjusted R-squared: ", model_summary$adj.r.squared, "\n")

# Extract coefficients and confidence intervals
coef_table <- tidy(model)
cat("\nModel Coefficients:\n")
print(coef_table)

conf_int <- confint(model)
cat("\nConfidence Intervals for Coefficients:\n")
print(conf_int)

# Visualize the fitted model
ggplot(data, aes(x = predictor, y = outcome)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(title = "Linear Model Fit", x = "Predictor", y = "Outcome") +
    theme_minimal()

# Save model summary and diagnostics to a text file
output_file <- "model_results.txt"
sink(output_file)

cat("Model Summary:\n")
print(model_summary)

cat("\nR-squared: ", model_summary$r.squared, "\n")
cat("Adjusted R-squared: ", model_summary$adj.r.squared, "\n")

cat("\nModel Coefficients:\n")
print(coef_table)

cat("\nConfidence Intervals for Coefficients:\n")
print(conf_int)

sink()  # Stop redirecting output to the file

cat("\nModel results saved to 'model_results.txt'.\n")

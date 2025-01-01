# analysis.R

# Load necessary libraries
library(ggplot2)  # For data visualization
library(dplyr)    # For data manipulation
library(tidyr)    # For data tidying

# Set options for better output display
options(max.print = 100)

# Load the dataset
data <- read.csv("data.csv")

# Check if the dataset loaded successfully
if (nrow(data) == 0) {
    stop("Error: The dataset is empty or could not be loaded.")
}

# Display basic summary statistics
cat("Summary Statistics:\n")
summary(data)

# Check for missing values in the dataset
missing_values <- colSums(is.na(data))
cat("\nMissing Values:\n")
print(missing_values[missing_values > 0])

# Exploratory Data Analysis (EDA)
cat("\nExploratory Data Analysis:\n")
eda_results <- data %>%
    summarise(across(everything(), list(mean = ~ mean(. , na.rm = TRUE),
                                         sd = ~ sd(. , na.rm = TRUE),
                                         min = ~ min(. , na.rm = TRUE),
                                         max = ~ max(. , na.rm = TRUE)))

print(eda_results)

# Visualizations
# Histogram for each numeric variable
numeric_vars <- sapply(data, is.numeric)
for (var in names(data)[numeric_vars]) {
    p <- ggplot(data, aes_string(x = var)) +
        geom_histogram(binwidth = (max(data[[var]], na.rm = TRUE) - min(data[[var]], na.rm = TRUE)) / 30, fill = "blue", color = "black") +
        labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
        theme_minimal()
    
    print(p)
}

# Save summary statistics and EDA results to a text file
output_file <- "analysis_results.txt"
sink(output_file)

cat("Summary Statistics:\n")
summary(data)

cat("\nMissing Values:\n")
print(missing_values)

cat("\nExploratory Data Analysis Results:\n")
print(eda_results)

sink()  # Stop redirecting output to the file

cat("\nAnalysis results saved to 'analysis_results.txt'.\n")

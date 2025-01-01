# visualization.R

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the dataset
data <- read.csv("data.csv")

# Check if the dataset loaded successfully
if (nrow(data) == 0) {
    stop("Error: The dataset is empty or could not be loaded.")
}

# Display the first few rows of the dataset for a quick overview
cat("First few rows of the dataset:\n")
print(head(data))

# Scatter Plot: Visualizing relationship between variable1 and variable2
scatter_plot <- ggplot(data, aes(x = variable1, y = variable2)) +
    geom_point(color = "blue", alpha = 0.6) +
    labs(title = "Scatter Plot of Variable1 vs Variable2",
         x = "Variable 1",
         y = "Variable 2") +
    theme_minimal()

print(scatter_plot)

# Line Plot: If applicable, visualize trends over time (assuming there's a time variable)
if ("time_variable" %in% colnames(data)) {
    line_plot <- ggplot(data, aes(x = time_variable, y = variable2)) +
        geom_line(color = "red") +
        labs(title = "Line Plot of Variable2 Over Time",
             x = "Time",
             y = "Variable 2") +
        theme_minimal()

    print(line_plot)
} else {
    cat("\nNo 'time_variable' found for line plot.\n")
}

# Histogram: Distribution of a specific variable (e.g., variable1)
histogram <- ggplot(data, aes(x = variable1)) +
    geom_histogram(binwidth = (max(data$variable1, na.rm = TRUE) - min(data$variable1, na.rm = TRUE)) / 30,
                   fill = "lightblue", color = "black") +
    labs(title = "Histogram of Variable1",
         x = "Variable 1",
         y = "Frequency") +
    theme_minimal()

print(histogram)

# Boxplot: Comparing distributions across categories (assuming there's a categorical variable)
if ("categorical_variable" %in% colnames(data)) {
    boxplot <- ggplot(data, aes(x = categorical_variable, y = variable2)) +
        geom_boxplot(fill = "lightgreen") +
        labs(title = "Boxplot of Variable2 by Categorical Variable",
             x = "Categorical Variable",
             y = "Variable 2") +
        theme_minimal()

    print(boxplot)
} else {
    cat("\nNo 'categorical_variable' found for boxplot.\n")
}

# Save visualizations to files
ggsave("scatter_plot.png", plot = scatter_plot)
ggsave("histogram.png", plot = histogram)

if ("time_variable" %in% colnames(data)) {
    ggsave("line_plot.png", plot = line_plot)
}

if ("categorical_variable" %in% colnames(data)) {
    ggsave("boxplot.png", plot = boxplot)
}

cat("\nVisualizations saved as PNG files.\n")

# Load necessary libraries

install.packages("ggplot2")
install.packages("ggpubr")
install.packages("GGally")
install.packages("broom")

library(ggplot2)
library(GGally)
library(broom)
library(ggpubr)

#_________________________________YEARLY DATA___________________________________

# Read the CSV file
data <- read.csv("./YEARLY_DATA_CO.csv")

# List of columns for Pearson correlation
correlation_columns <- c("Female_Lit", "Elevation", "NDBI_MAX", "POP_DENSITY", "Hospital_Dist", "Road_Dist")

# Function to perform Pearson correlation and generate scatter plot
perform_correlation <- function(data, target_column, correlation_column) {
  # Calculate correlation
  correlation_result <- cor.test(data[[target_column]], data[[correlation_column]], method = "pearson")
  
  # Extract results
  correlation <- correlation_result$estimate
  p_value <- correlation_result$p.value
  
  # Calculate R squared
  r_squared <- correlation^2
  
  # Determine significance level
  significance <- ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "ns")))
  
  # Generate scatter plot with R squared and p-value
  plot <- ggplot(data, aes_string(x=target_column, y=correlation_column)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    ggtitle(paste("Scatter Plot:", correlation_column, "vs", target_column)) +
    xlab(target_column) +
    ylab(correlation_column) +
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~~~")), 
             label.x.npc = "left", label.y.npc = "top", 
             method = "pearson", r.digits = 2, p.digits = 3)
  
  list(r_squared=r_squared, p_value=p_value, significance=significance, plot=plot)
}

# Perform correlation for each column and generate scatter plots
results <- lapply(correlation_columns, function(column) perform_correlation(data, "D_2023_10K", column))

# Display results and plots
for (result in results) {
  print(result$r_squared)
  print(result$p_value)
  print(result$significance)
  print(result$plot)
}




#_________________________________SEASONAL DATA___________________________________


# Load necessary libraries
library(ggplot2)
library(GGally)
library(broom)

# Read the CSV file
data <- read.csv("./SEA_CO_DATA.csv")

# Define a function to create a scatter plot with R² and P-value
plot_correlation <- function(data, x, y) {
  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(paste(x, "vs", y))
  
  model <- lm(as.formula(paste(y, "~", x)), data = data)
  model_summary <- summary(model)
  r_squared <- model_summary$r.squared
  p_value <- coef(summary(model))[2,4]
  
  p + annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, label = sprintf("R² = %.4f\nP-value = %.4f", r_squared, p_value), size = 3.5)
}

# Perform Pearson correlation and create plots
pairs <- list(
  c("D_2023_WIN", "T_win"),
  c("D_2023_WIN", "P_win"),
  c("D_2023_PRMO", "T_prms"),
  c("D_2023_PRMO", "P_prms"),
  c("D_2023_MOON", "T_mnsn"),
  c("D_2023_MOON", "P_mnsn"),
  c("D_2023_POMO", "T_poms"),
  c("D_2023_POMO", "P_poms")
)

# Output Pearson correlation results and display plots
results <- data.frame()
for (pair in pairs) {
  cor_test <- cor.test(data[[pair[1]]], data[[pair[2]]])
  results <- rbind(results, data.frame(
    x = pair[1],
    y = pair[2],
    correlation = cor_test$estimate,
    p_value = cor_test$p.value,
    t_statistic = qt(cor_test$p.value/2, cor_test$parameter, lower.tail = FALSE)
  ))
  print(plot_correlation(data, pair[1], pair[2]))
}

# Display results
results

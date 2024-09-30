# Predictive_Modeling_Project
Predictive Modeling for Optimized Customer Engagement and Revenue Growth
## Tayko Database
To optimize Tayko Software's catalog mailing strategy by developing predictive models for customer response and spending. Using key customer data, we aim to enhance target selection, improve response rates, and maximize revenue from catalog campaigns.
Database available here.
## Objectives
- Data Exploration and Transformation: Perform comprehensive data cleaning and transformation to ensure readiness for modeling, including handling missing values and optimizing variable types.
- Advanced Modeling Techniques: Build and compare Multiple Linear Regression, Stepwise Regression and Regression Tree models to predict key housing market trends and affordability metrics.
- Model Evaluation: Assess and interpret model performance using statistical metrics like R-squared, Mean Squared Error (MSE), and cross-validation results to determine the optimal model.
- Actionable Insights: Provide clear recommendations based on model outcomes, offering data-driven solutions for addressing housing affordability and expenditure disparities.
# Project Source Code
## Data Preparation Code (R Studio)
```r
# Load required libraries
library(readxl)
library(tidyverse)
library(naniar)

# Load the dataset
tayko <- read_excel("Tayko.xlsx", sheet = "Data", range = "B1:Y2001")

# Review the structure and data types
glimpse(tayko)

# Rename problematic columns
tayko <- tayko %>%
  rename(
    first_update_days_ago = `1st_update_days_ago`,
    Web_order = `Web order`,
    Gender = `Gender=male`
  )

# Evaluate and visualize missing values
missing_plot <- tayko %>%
  vis_miss() +
  labs(title = "Missing Values Visualization") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(missing_plot)

# Calculate and display percentage of missing values
missing_summary <- tayko %>%
  summarise(across(everything(), ~sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Percentage") %>%
  arrange(desc(Missing_Percentage))

print(missing_summary)

# Check for duplicated rows
duplicate_count <- sum(duplicated(tayko))
cat("Number of duplicated rows:", duplicate_count, "\n")

# Data type conversion and handling
tayko <- tayko %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Display summary statistics
summary(tayko)

# Save cleaned data
write_csv(tayko, "tayko_cleaned.csv")
```
## Data Exploration Code (R Studio)
```r
# Data Exploration
library(tidyverse)
library(scales)
library(patchwork)

# Summary statistics for numeric variables
numeric_summary <- tayko %>%
  select(Freq, last_update_days_ago, first_update_days_ago, Spending) %>%
  summary() %>%
  as.data.frame() %>%
  rownames_to_column("Statistic")

print(numeric_summary)

# Frequency distribution of categorical variables
cat_freq <- tayko %>%
  select(where(is.factor)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  count(Variable, Value) %>%
  group_by(Variable) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create a function for plotting categorical variables
plot_categorical <- function(data, var) {
  ggplot(data %>% filter(Variable == var), aes(x = Value, y = Percentage, fill = Value)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5) +
    labs(title = var, x = NULL, y = "Percentage") +
    theme_minimal() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create plots for each categorical variable
cat_plots <- map(unique(cat_freq$Variable), ~plot_categorical(cat_freq, .x))

# Combine plots using patchwork
combined_plot <- wrap_plots(cat_plots, ncol = 2)
print(combined_plot)

# Purchase analysis
purchase_plot <- tayko %>%
  count(Purchase) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = factor(Purchase), y = Percentage, fill = factor(Purchase))) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", Percentage, n)), vjust = -0.5) +
  scale_fill_manual(values = c("#CCCCCC", "#104E8B")) +
  labs(title = "Purchase Analysis", x = "Purchase (0 = No, 1 = Yes)", y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "none")

print(purchase_plot)

# Web order analysis
web_order_plot <- tayko %>%
  mutate(Order_Type = ifelse(Web_order == 1, "Web Order", "Other Types of Orders")) %>%
  count(Order_Type) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = Order_Type, y = Percentage, fill = Order_Type)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", Percentage, n)), vjust = -0.5) +
  scale_fill_manual(values = c("#CCCCCC", "#104E8B")) +
  labs(title = "Web Order vs. Other Types of Orders", x = NULL, y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "none")

print(web_order_plot)

# Gender analysis
gender_plot <- tayko %>%
  mutate(Gender = ifelse(Gender == 1, "Male", "Female")) %>%
  count(Gender) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = Gender, y = Percentage, fill = Gender)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", Percentage, n)), vjust = -0.5) +
  scale_fill_manual(values = c("#CCCCCC", "#104E8B")) +
  labs(title = "Gender Analysis", x = NULL, y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "none")

print(gender_plot)

# Correlation analysis for numeric variables
cor_matrix <- tayko %>%
  select(where(is.numeric)) %>%
  cor()

corrplot::corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
```
## Data Modeling (R Studio)
```r
# Load required libraries
library(tidyverse)
library(corrplot)
library(caret)
library(pROC)

# Load and preprocess data
Tayko <- read_excel("Tayko.xlsx", sheet = "Data", range = "B1:Y2001") %>%
  rename_with(~gsub(" ", "_", .x)) %>%
  mutate(across(where(is.character), as.factor))

# Correlation analysis
cor_matrix <- cor(Tayko %>% select(where(is.numeric)))
corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black", tl.cex = 0.7,
         addCoef.col = "black", number.cex = 0.5, 
         title = "Correlation Plot for Tayko")

# Select variables for Purchase model
cor_with_purchase <- cor_matrix[, "Purchase"]
selected_vars <- names(cor_with_purchase[abs(cor_with_purchase) > 0.07 & 
                                         names(cor_with_purchase) != "Purchase"])

# Data partition
set.seed(123)
train_index <- createDataPartition(Tayko$Purchase, p = 0.8, list = FALSE)
train_data <- Tayko[train_index, ]
val_data <- Tayko[-train_index, ]

# Build Purchase model
purchase_model <- glm(Purchase ~ ., data = train_data[, c("Purchase", selected_vars)], 
                      family = binomial())

# Model summary
summary(purchase_model)

# Predictions and evaluation
predictions <- predict(purchase_model, newdata = val_data, type = "response")
confusion_matrix <- confusionMatrix(factor(ifelse(predictions > 0.5, 1, 0)), 
                                    factor(val_data$Purchase))
print(confusion_matrix)

# ROC curve
roc_curve <- roc(val_data$Purchase, predictions)
plot(roc_curve, main = "ROC Curve")
auc(roc_curve)

# Probability density plot
ggplot(data.frame(predictions = predictions, Purchase = val_data$Purchase), 
       aes(x = predictions, fill = factor(Purchase))) +
  geom_density(alpha = 0.5) +
  labs(title = "Probability Density Plot", x = "Predicted Probabilities", fill = "Purchase") +
  theme_minimal()
```
## Evaluating The Model (R Studio)
```r
# Load required libraries
library(tidyverse)
library(caret)
library(pROC)

# Function to evaluate model performance
evaluate_model <- function(actual, predicted, model_name) {
  mse <- mean((actual - predicted)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(actual - predicted))
  rsquared <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  
  cat(paste0("\nEvaluation metrics for ", model_name, " model:\n"))
  cat(paste("MSE:", round(mse, 4), "\n"))
  cat(paste("RMSE:", round(rmse, 4), "\n"))
  cat(paste("MAE:", round(mae, 4), "\n"))
  cat(paste("R-squared:", round(rsquared, 4), "\n"))
  
  # Plot predicted vs actual
  plot <- ggplot(data.frame(Actual = actual, Predicted = predicted), aes(x = Actual, y = Predicted)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(title = paste(model_name, "Model: Predicted vs Actual"),
         x = "Actual", y = "Predicted") +
    theme_minimal()
  
  print(plot)
  
  if (model_name == "Purchase") {
    # ROC curve for Purchase model
    roc_obj <- roc(actual, predicted)
    plot(roc_obj, main = "ROC Curve")
    cat(paste("AUC:", round(auc(roc_obj), 4), "\n"))
    
    # Confusion matrix for Purchase model
    conf_matrix <- confusionMatrix(factor(ifelse(predicted > 0.5, 1, 0)), factor(actual))
    print(conf_matrix)
  }
}

# Evaluate Purchase model
evaluate_model(val_data_purchase$Purchase, purchase_predictions, "Purchase")

# Evaluate Spending model
evaluate_model(val_data_spending$Spending, spending_predictions, "Spending")

# Stepwise regression for Spending model
spending_stepwise <- step(lm(Spending ~ ., data = train_data_spending), direction = "both")
spending_predictions_stepwise <- predict(spending_stepwise, newdata = val_data_spending)

# Evaluate stepwise Spending model
evaluate_model(val_data_spending$Spending, spending_predictions_stepwise, "Stepwise Spending")

# Display coefficients for stepwise models
options(scipen = 0, digits = 4)
summary(purchase_stepwise)$coefficients
summary(spending_stepwise)$coefficients
```
## Result Visualization (R Studio)
```r
library(ggplot2)
library(rpart)
library(rpart.plot)

# Function to create and print plots
create_plot <- function(plot, title) {
  plot + 
    labs(title = title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12),
          legend.position = "none")
}

# Residual plot
residuals <- val_data_spending$Spending - spending_predictions
residual_plot <- create_plot(
  ggplot(data.frame(predicted = spending_predictions, residuals = residuals), 
         aes(x = predicted, y = residuals)) +
    geom_point(color = "#3498db", alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#e74c3c"),
  "Residual Plot"
)

# Q-Q plot
qq_plot <- create_plot(
  ggplot(data.frame(residuals = residuals), aes(sample = residuals)) +
    stat_qq() + stat_qq_line(),
  "Q-Q Plot of Residuals"
)

# Print plots
print(residual_plot)
print(qq_plot)

# Regression tree
regression_tree <- rpart(Spending ~ ., data = train_data_spending, method = 'anova')
pruned_tree <- prune(regression_tree, cp = 0.01)
rpart.plot(pruned_tree, extra = 101, fallen.leaves = FALSE)

# Variable importance
importance_data <- data.frame(
  Variable = names(pruned_tree$variable.importance),
  Importance = pruned_tree$variable.importance,
  Percentage_Importance = (pruned_tree$variable.importance / sum(pruned_tree$variable.importance)) * 100
)
print(importance_data)

# Predictions and evaluation
predictions_spending_tree <- predict(pruned_tree, newdata = val_data_spending)
actual <- val_data_spending$Spending

# Evaluation metrics
metrics <- c(
  MSE = mean((actual - predictions_spending_tree)^2),
  RMSE = sqrt(mean((actual - predictions_spending_tree)^2)),
  MAE = mean(abs(actual - predictions_spending_tree)),
  R_squared = 1 - (sum((actual - predictions_spending_tree)^2) / sum((actual - mean(actual))^2))
)

print(metrics)

# Predicted vs Actual plot
predicted_vs_actual_plot <- create_plot(
  ggplot(data.frame(Actual = actual, Predicted = predictions_spending_tree), 
         aes(x = Actual, y = Predicted)) +
    geom_point(color = "#3498db", alpha = 0.7) +
    geom_abline(intercept = 0, slope = 1, color = "#e74c3c", linetype = "dashed"),
  "Predicted vs Actual"
)

print(predicted_vs_actual_plot)
```
# Project Summary: Tayko Marketing Analysis

This project focused on analyzing marketing data from Tayko to develop predictive models for customer purchases and spending. The process included several key steps:

## Data Preparation and Cleaning**
   - Loaded and preprocessed the Tayko dataset
   - Handled missing values and renamed problematic columns
   - Converted data types and imputed missing numeric values

## Exploratory Data Analysis (EDA)
   - Conducted summary statistics for numeric variables
   - Analyzed frequency distributions of categorical variables
   - Visualized key relationships using various plots (e.g., purchase analysis, web order analysis, gender analysis)

## Correlation Analysis
   - Created correlation matrices and visualized them using corrplot
   - Identified key variables correlated with Purchase and Spending

## Feature Selection
   - Used correlation thresholds to select relevant features for models
   - Employed stepwise regression for further feature refinement

## Model Development
   - Built separate models for Purchase (logistic regression) and Spending (linear regression)
   - Utilized both standard and stepwise regression approaches

## Model Evaluation
   - Implemented a comprehensive evaluation function for both models
   - Calculated metrics such as MSE, RMSE, MAE, and R-squared
   - Created visualizations including residual plots, Q-Q plots, and predicted vs. actual plots

## Advanced Techniques
   - Developed a regression tree model for the Spending prediction
   - Analyzed variable importance in the tree model

This project demonstrates a thorough approach to predictive modeling in marketing, covering essential steps from data preparation to model evaluation. The use of various visualization techniques and model types shows a good understanding of data analysis principles.

For future improvements, I would consider:
- Exploring more advanced machine learning algorithms (e.g., Random Forests, Gradient Boosting)
- Implementing cross-validation for more robust model evaluation
- Investigating feature interactions and non-linear relationships

This project provides me valuable insights into customer behavior and spending patterns, which can be instrumental for Tayko's marketing strategies. The models developed can help in targeting potential customers and predicting their spending, potentially improving marketing efficiency and ROI.

Thank you for your time reading this comprehensive analysis. This approach to forecasting and predictive modeling serves as an excellent example of applying data science techniques to real-world marketing challenges.

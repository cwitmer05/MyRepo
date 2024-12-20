#Installing Necessary Packages
install.packages("readxl")
install.packages("caret")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("jtools")
install.packages("leaps")
install.packages("dotwhisker")
install.packages("margins")
#Loading Packages
library(leaps)
library(dplyr)
library(readxl)
library(tidyr)
library(jtools)
library(dotwhisker)
library(ggplot2)
library(caret)
library(margins)
#------------------------------------------------------------------------------
#Read and convert to dataframe
sales_data <- read_excel("~/Downloads/SalesData.xlsx")
sales_data <- as.data.frame(sales_data)

#Ensuring Category is a categorical variable
sales_data$Category <- as.factor(sales_data$Category)

#Number of Rows and Columns Before Cleaning
nrow(sales_data)
ncol(sales_data)

#Missing Value Removal
sum(is.na(sales_data))
sales_data <- drop_na(sales_data)

#OutlierRemoval: Sales
SQ1 <- quantile(sales_data$Sales, 0.25)
SQ3 <- quantile(sales_data$Sales, 0.75)
SIQR <- SQ3 - SQ1
sales_lower_bound <- SQ1 - 1.5 * SIQR
sales_upper_bound <- SQ3 + 1.5 * SIQR
sales_data <- sales_data %>%
  filter(Sales>= sales_lower_bound & Sales<= sales_upper_bound)

#OutlierRemoval: Quantity
QQ1 <- quantile(sales_data$Quantity, 0.25)
QQ3 <- quantile(sales_data$Quantity, 0.75)
QIQR <- QQ3 - QQ1
quantity_lower_bound <- QQ1 - 1.5 * QIQR
quantity_upper_bound <- QQ3 + 1.5 * QIQR
sales_data <- sales_data %>%
filter(Quantity>= quantity_lower_bound & Quantity<= quantity_upper_bound)

#OutlierRemoval: Discount
DQ1 <- quantile(sales_data$Discount, 0.25)
DQ3 <- quantile(sales_data$Discount, 0.75)
DIQR <- DQ3 - DQ1
discount_lower_bound <- DQ1 - 1.5 * DIQR
discount_upper_bound <- DQ3 + 1.5 * DIQR
sales_data <- sales_data %>%
filter(Discount>= discount_lower_bound & Discount<= discount_upper_bound)

#OutlierRemoval: Profit
PQ1 <- quantile(sales_data$Profit, 0.25)
PQ3 <- quantile(sales_data$Profit, 0.75)
PIQR <- PQ3 - PQ1
profit_lower_bound <- PQ1 - 1.5 * PIQR
profit_upper_bound <- PQ3 + 1.5 * PIQR
sales_data <- sales_data %>%
  filter(Profit>= profit_lower_bound & Profit<= profit_upper_bound)

#Duplicate Removal
sales_data <- sales_data %>% distinct()

#Inconsistency Check
unique(sales_data$Sales)
unique(sales_data$Quantity)
unique(sales_data$Discount)
unique(sales_data$Category)
unique(sales_data$SubCategory)

#Cleaned Table with only important variables
sales_data <- sales_data %>%
  select(Category,Sales, Quantity, Discount, Profit)

#Number of Rows and Columns after cleaning
nrow(sales_data)
ncol(sales_data)
#------------------------------------------------------------------------------
# 1) Histogram of Sales Volume
ggplot(sales_data, aes(x = Sales)) +
  geom_histogram(binwidth = 10, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Sales Volume", x = "Number of Sales", y = "Frequency") +
  theme_minimal()

# 2) Histogram of Quantity
ggplot(sales_data, aes(x = Quantity)) +
  geom_histogram(binwidth = 1, fill = "#377EB8", color = "black") +
  labs(title = "Distribution of Quantity", x = "Quantity", y = "Frequency") +
  theme_minimal()

# 3) Histogram of Discount
ggplot(sales_data, aes(x = Discount)) +
  geom_histogram(binwidth = 0.05, fill = "#375", color = "black") +
  labs(title = "Distribution of Discount", x = "Discount Amount", y = "Frequency") +
  theme_minimal()

# 4) Histogram of Profit Margin
ggplot(sales_data, aes(x = Profit)) +
  geom_histogram(binwidth = 0.01, fill = "#924", color = "#924") +
  labs(title = "Distribution of Profit", x = "Profit", y = "Frequency") +
  theme_minimal()

# 5) Number per Category (Bar Graph)
ggplot(sales_data, aes(x = Category)) +
  geom_bar(fill = "#FFD700") +
  labs(title = "Number of Orders per Category", x = "Category", y = "Count") +
  theme_minimal()

# Descriptive statistics for Sales, Quantity, Discount, and Profit Margin

#Average and Median Sales
average_sales <- mean(sales_data$Sales, na.rm = TRUE)
median_sales <- median(sales_data$Sales, na.rm = TRUE)
cat("Average Sales:", round(average_sales, 2), "\n")
cat("Median Sales:", round(median_sales, 2), "\n")

#Average and Median Quantity
average_quantity <- mean(as.numeric(sales_data$Quantity), na.rm = TRUE)
median_quantity <- median(as.numeric(sales_data$Quantity), na.rm = TRUE)
cat("Average Quantity:", round(average_quantity, 2), "\n")
cat("Median Quantity:", round(median_quantity, 2), "\n")

#Average and Median Discount
average_discount <- mean(sales_data$Discount, na.rm = TRUE)
median_discount <- median(sales_data$Discount, na.rm = TRUE)
cat("Average Discount:", round(average_discount, 2), "\n")
cat("Median Discount:", round(median_discount, 2), "\n")

#Average and Median Profit Margin
average_profit <- mean(sales_data$Profit, na.rm = TRUE)
median_profit <- median(sales_data$Profit, na.rm = TRUE)
cat("Average Profit:", round(average_profit, 2), "\n")
cat("Median Profit:", round(median_profit, 2), "\n")

#Summary of key variables
summary(sales_data$Category)
summary(sales_data$Sales)
summary(sales_data$Quantity)
summary(sales_data$Discount)
summary(sales_data$Profit)
#------------------------------------------------------------------------------
#Modeling the Data
model1 <- lm(Profit ~ Sales + Quantity + Discount + Category, data = sales_data)
summ(model1, scale = TRUE, digits = 4)

#Best Subset Selection

#Scaling Data for BIC Since JTools Scaled the Original Dataset
sales_data_scaled <- sales_data
sales_data_scaled$Sales <- scale(sales_data$Sales)
sales_data_scaled$Quantity <- scale(sales_data$Quantity)
sales_data_scaled$Discount <- scale(sales_data$Discount)
# Fit the best subset model
best_model <- regsubsets(Profit ~ ., data = sales_data_scaled, nbest = 1)
# Extract the summary of the model
best_model_summary <- summary(best_model)
# Extract metrics
bic_values <- best_model_summary$bic
# Find the best model indices based on each criterion
best_bic_index <- which.min(bic_values)
# Display the best models based on the chosen criteria)
print(coef(best_model, best_bic_index))
#Best Subset model is the same as the initial model, which is a good sign

# create training and validation sets with 80% and 20% of the data
trainIndex <- createDataPartition(sales_data$Profit, p = 0.8, list = FALSE)
training <- sales_data[trainIndex, ]
testing <- sales_data[-trainIndex, ]
#linear regression model using the training set
model <- lm(Profit ~ Sales + Quantity + Discount + Category, data = training)
# predict on both training and testing sets
predict.train <- predict(model,data=training)
predict.test <- predict(model, newdata = testing)
# calculate RMSE on both sets
RMSE.train <- sqrt(mean((predict.train - training$Profit)^2))
RMSE.test <- sqrt(mean((predict.test - testing$Profit)^2))
# print RMSEs
print(paste0("Training RMSE: ", RMSE.train))
print(paste0("Testing RMSE: ", RMSE.test))
#RMSEs are very similar, indicating the model is a good fit
#------------------------------------------------------------------------------
#Residual Plot of Actual Vs Predicted Profit Values
residuals <- residuals(model)
ggplot(data.frame(predicted = predict.train, residuals = residuals), 
       aes(x = predicted, y = residuals)) +
  geom_point(color = "#948") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(x = "Predicted Profit Values", y = "Residuals", title = "Residual Plot") +
  theme_minimal()+
  theme(panel.grid = element_blank())

# Create a df for training and testing
training_data <- data.frame(actual = training$Profit, predicted = predict.train, dataset = "Training")
testing_data <- data.frame(actual = testing$Profit, predicted = predict.test, dataset = "Testing")

# Combine the training and testing data
combined_data <- rbind(training_data, testing_data)

# Scatter Plot with Different Colors for Training and Testing Data
ggplot(combined_data, aes(x = actual, y = predicted, color = dataset)) +
  geom_point() +
  labs(title = "Actual vs Predicted Profit", x = "Actual Profit", y = "Predicted Profit") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Perfect prediction line
  theme_minimal() +
  scale_color_manual(values = c("Training" = "blue", "Testing" = "green")) + 
                       theme(panel.grid = element_blank())

#DotWhisker Plot for Categorical Variables

# Fit the model with scaled variables
model_std <- lm(Profit ~ Sales + Quantity + Discount + Category, data = sales_data_scaled)
summary(model_std)
#Renaming Coefficients and Including Furniture as the intercept
coef_data <- tidy(model_std) %>%
  mutate(term = case_when(
    term == "(Intercept)" ~ "Furniture (Intercept)",
    term == "CategoryOffice Supplies" ~ "Office Supplies",
    term == "CategoryTechnology" ~ "Technology",
    TRUE ~ term))

#Creating dotwhisker Plot
dwplot(coef_data, vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) +
  labs(title = "Dot Whisker Plot of Model Coefficients",
  subtitle = "Profit ~ Sales + Quantity + Discount + Category",
  x = "Coefficient Estimate",
  y = "Variables") + theme_minimal() + theme(plot.title = element_text(face = "bold"),
                                             legend.position = "none") + 
  #True Coefficient Labels from Model
  geom_text(aes(x = estimate, y = term, label = paste0(round(estimate, 3))), 
            vjust = -1, 
            size = 3)
install.packages("readxl")
install.packages("caret")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
library(dplyr)
library(readxl)
library(tidyr)

#Read and convert to dataframe
sales_data <- read_excel("~/Downloads/SalesData.xlsx")
sales_data <- as.data.frame(sales_data)

#Rows and Columns Before Cleaning
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

#Creation of Profit Margin Column (Target/Outcome variable of model)
sales_data$profit_margin <- sales_data$Profit / sales_data$Sales

#Cleaned Table with only important variables
sales_data <- sales_data %>%
  select(Category, SubCategory, Sales, Quantity, Discount, profit_margin)

#Number of Rows and Columns after cleaning
nrow(sales_data)
ncol(sales_data)


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
ggplot(sales_data, aes(x = profit_margin)) +
  geom_histogram(binwidth = 0.01, fill = "#924", color = "black") +
  labs(title = "Distribution of Profit Margin", x = "Profit Margin", y = "Frequency") +
  theme_minimal()

# 5) Number per Category (Bar Graph)
ggplot(sales_data, aes(x = Category)) +
  geom_bar(fill = "#FFD700") +
  labs(title = "Number of Orders per Category", x = "Category", y = "Count") +
  theme_minimal()

# 6) Number per SubCategory (Bar Graph)
ggplot(sales_data, aes(x = SubCategory)) +
  geom_bar(fill = "#984EA3") +
  labs(title = "Number of Orders per SubCategory", x = "Sub Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Descriptive statistics for Sales, Quantity, Discount, and Profit Margin

# 7) Average and Median Sales
average_sales <- mean(sales_data$Sales, na.rm = TRUE)
median_sales <- median(sales_data$Sales, na.rm = TRUE)
cat("Average Sales:", round(average_sales, 2), "\n")
cat("Median Sales:", round(median_sales, 2), "\n")

# 8) Average and Median Quantity
average_quantity <- mean(as.numeric(sales_data$Quantity), na.rm = TRUE)
median_quantity <- median(as.numeric(sales_data$Quantity), na.rm = TRUE)
cat("Average Quantity:", round(average_quantity, 2), "\n")
cat("Median Quantity:", round(median_quantity, 2), "\n")

# 9) Average and Median Discount
average_discount <- mean(sales_data$Discount, na.rm = TRUE)
median_discount <- median(sales_data$Discount, na.rm = TRUE)
cat("Average Discount:", round(average_discount, 2), "\n")
cat("Median Discount:", round(median_discount, 2), "\n")

# 10) Average and Median Profit Margin
average_profit_margin <- mean(sales_data$profit_margin, na.rm = TRUE)
median_profit_margin <- median(sales_data$profit_margin, na.rm = TRUE)
cat("Average Profit Margin:", round(average_profit_margin, 2), "\n")
cat("Median Profit Margin:", round(median_profit_margin, 2), "\n")

#Summary of key variables
summary(sales_data$Category)
summary(sales_data$SubCategory)
summary(sales_data$Sales)
summary(sales_data$Quantity)
summary(sales_data$Discount)
summary(sales_data$profit_margin)

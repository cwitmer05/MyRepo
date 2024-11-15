install.packages("readxl")
install.packages("caret")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
library(dplyr)
library(readxl)
library(tidyr)

#Read and convert to dataframe
sales_data <- read_excel("~/Desktop/SalesData.xlsx")
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

#Duplicate Removal
sales_data <- sales_data %>% distinct()

#Inconsistency Check (less of a worry since they are all numeric)
unique(sales_data$Sales)
unique(sales_data$Quantity)
unique(sales_data$Discount)

#Creation of Profit Margin Column (Target/Outcome variable of model)
sales_data$profit_margin <- sales_data$Profit / sales_data$Sales

#Cleaned Table with only important variables
sales_data <- sales_data %>%
  select(Sales, Quantity, Discount, profit_margin)
head(sales_data, 20)
#Number of Rows and Columns after cleaning
nrow(sales_data)
ncol(sales_data)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)

# Read and convert to dataframe
sales_data <- read_excel("-/Downloads/SalesData.xlsx")
sales_data <- as.data.frame(sales_data)

# Convert OrderDate and ShipDate to Date type
sales_data$OrderDate <- as.Date(sales_data$OrderDate, format="%m/%d/%Y")
sales_data$ShipDate <- as.Date(sales_data$ShipDate, format="%m/%d/%Y")

# Calculate number of days between OrderDate and ShipDate
sales_data$DaysBetween <- as.numeric(sales_data$ShipDate - sales_data$OrderDate)

# 1) Histogram of Days Between Order Date and Ship Date
ggplot(sales_data, aes(x = DaysBetween)) +
  geom_histogram(binwidth = 1, fill = "#4DAF4A", color = "black", alpha = 0.8) +
  labs(title = "Distribution of Days Between Order Date and Ship Date", x = "Days Between Order and Ship", y = "Frequency") +
  theme_minimal()

# 2) Number per ShipMode (Bar Graph)
ggplot(sales_data, aes(x = ShipMode)) +
  geom_bar(fill = "#377EB8") +
  labs(title = "Number of Orders per Ship Mode", x = "Ship Mode", y = "Count") +
  theme_minimal()

# 3) Number per Segment (Bar Graph)
ggplot(sales_data, aes(x = Segment)) +
  geom_bar(fill = "#FF7F00") +
  labs(title = "Number of Orders per Segment", x = "Segment", y = "Count") +
  theme_minimal()

# 4) Number per State (Bar Graph)
ggplot(sales_data, aes(x = State)) +
  geom_bar(fill = "#984EA3") +
  labs(title = "Number of Orders per State", x = "State", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal()

# 5) Number per Region (Bar Graph)
ggplot(sales_data, aes(x = Region)) +
  geom_bar(fill = "#E41A1C") +
  labs(title = "Number of Orders per Region", x = "Region", y = "Count") +
  theme_minimal()

# 6) Number per Category (Bar Graph)
ggplot(sales_data, aes(x = Category)) +
  geom_bar(fill = "#FFD700") +
  labs(title = "Number of Orders per Category", x = "Category", y = "Count") +
  theme_minimal()

# Descriptive statistics for Sales, Quantity, Discount, and Profit

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

# 10) Average and Median Profit
average_profit <- mean(sales_data$Profit, na.rm = TRUE)
median_profit <- median(sales_data$Profit, na.rm = TRUE)
cat("Average Profit:", round(average_profit, 2), "\n")
cat("Median Profit:", round(median_profit, 2), "\n")

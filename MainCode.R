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

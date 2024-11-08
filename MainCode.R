install.packages("readxl")
install.packages("caret")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
library(readxl)
sales_data <- read_excel("~/Desktop/SalesData.xlsx")
sales_data <- as.data.frame(sales_data)
#Missing Values
sum(is.na(sales_data))
library(tidyr)
sales_data <- drop_na(sales_data)

library(dplyr)
#Outliers~Sales
SQ1 <- quantile(sales_data$Sales, 0.25)
SQ3 <- quantile(sales_data$Sales, 0.75)
SIQR <- SQ3 - SQ1
Slower_bound <- SQ1 - 1.5 * IQR
Supper_bound <- SQ3 + 1.5 * IQR
sales_data <- sales_data %>%
  filter(Sales>= Slower_bound & Sales<= Supper_bound)

#Outliers~Quantity
QQ1 <- quantile(sales_data$Quantity, 0.25)
QQ3 <- quantile(sales_data$Quantity, 0.75)
QIQR <- QQ3 - QQ1
Qlower_bound <- QQ1 - 1.5 * QIQR
Qupper_bound <- QQ3 + 1.5 * QIQR
sales_data <- sales_data %>%
  filter(Quantity>= Qlower_bound & Quantity<= Qupper_bound)

#Outliers~Discount
DQ1 <- quantile(sales_data$Discount, 0.25)
DQ3 <- quantile(sales_data$Discount, 0.75)
DIQR <- DQ3 - DQ1
Dlower_bound <- DQ1 - 1.5 * DIQR
Dupper_bound <- DQ3 + 1.5 * DIQR
sales_data <- sales_data %>%
  filter(Discount>= Dlower_bound & Discount<= Dupper_bound)

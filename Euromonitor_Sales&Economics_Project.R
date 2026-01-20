# --------------------------------------------------------
# Euromonitor Data Analysis Project

# --------------------------------------------------------

# ------------------------------
# 1. Load necessary packages
# ------------------------------
# readxl: to read Excel files
# dplyr: for data manipulation
# ggplot2: for visualization

# Install packages if not already installed 
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("ggplot2")

library(readxl)
library(dplyr)
library(ggplot2)

# ------------------------------
# 2. Load the dataset
# ------------------------------
# Make sure your Excel file is in your working directory


data <- read_excel("euromonitor_catalyst_cleaned.xlsx", sheet = "Cleaned_Data")

# Quick look at the first 6 rows
head(data)

# ------------------------------
# 3. Data validation
# ------------------------------

# 3.1 Check structure of dataset
str(data)

# 3.2 Check for missing values
colSums(is.na(data))

# 3.3 Check for duplicates
sum(duplicated(data))

# 3.4 Summary statistics of clean_price
summary(data$clean_price)

# 3.5 Convert date column from character to Date type
# Assuming your date is in "YYYY-MM" format
data$date <- as.Date(paste0(data$date, "-01"))

# Check first few dates to confirm
head(data$date)

# ------------------------------
# 4. Descriptive Analysis
# ------------------------------

# 4.1 Brand and Category counts
table(data$clean_brand)
table(data$clean_category)

# 4.2 Price statistics by Brand
data %>%
  group_by(clean_brand) %>%
  summarise(
    Average_Price = mean(clean_price),
    Min_Price = min(clean_price),
    Max_Price = max(clean_price)
  )

# 4.3 Price statistics by Category
data %>%
  group_by(clean_category) %>%
  summarise(
    Average_Price = mean(clean_price),
    Min_Price = min(clean_price),
    Max_Price = max(clean_price)
  )

# ------------------------------
# 5. Visualizations
# ------------------------------

# 5.1 Brand-wise bar chart
ggplot(data, aes(x = clean_brand)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Number of Entries per Brand", x = "Brand", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5.2 Category-wise bar chart
ggplot(data, aes(x = clean_category)) +
  geom_bar(fill = "coral") +
  theme_minimal() +
  labs(title = "Number of Entries per Category", x = "Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5.3 Monthly Average Price Trend
monthly_trend <- data %>%
  group_by(date) %>%
  summarise(Average_Price = mean(clean_price))

ggplot(monthly_trend, aes(x = date, y = Average_Price)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Monthly Average Price Trend", x = "Month", y = "Average Price")

# ------------------------------
# 6. Simple Linear Forecast
# ------------------------------

# Fit a linear model for forecasting
trend_model <- lm(Average_Price ~ as.numeric(date), data = monthly_trend)

# Predict next 3 months
future_dates <- seq(max(monthly_trend$date) + 1,
                    by = "month", length.out = 3)

future_pred <- data.frame(
  date = future_dates,
  Average_Price = predict(trend_model, newdata = data.frame(date = as.numeric(future_dates)))
)

# Plot trend with forecast
ggplot() +
  geom_line(data = monthly_trend, aes(x = date, y = Average_Price), color = "blue") +
  geom_point(data = monthly_trend, aes(x = date, y = Average_Price), color = "blue") +
  geom_line(data = future_pred, aes(x = date, y = Average_Price), color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Price Trend with 3-Month Forecast", x = "Date", y = "Average Price")

# ------------------------------



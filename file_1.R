
# install important libraries
install.packages("tidyverse")
install.packages('dplyr')

# import important libraries
library('tidyverse')
library('dplyr')

install.packages('ggplot2')
library(ggplot2)



install.packages('tseries')

getwd()
# Read the CSV file
data <- read.csv("D://data analytics and technologies/Data Science/assignment 2/assignment_2/my_data1.csv", header = FALSE, stringsAsFactors = FALSE)
View(data)

# Extract date_time values from the first row
date_times <- unlist(data[1, seq(1, ncol(data), by = 10)])

# Initialize an empty data frame to store the result
result <- data.frame(date_time = character(), TSK = numeric(), PSFC = numeric(), U10 = numeric(), V10 = numeric(), Q2 = numeric(), RAINC = numeric(), RAINNC = numeric(), SNOW = numeric(), TSLB = numeric(), SMOIS = numeric(), stringsAsFactors = FALSE)

# Loop over the columns, starting from First column and increment by Last column
for (i in seq(1, length(date_times))) {
  # Extract values from the third row
  values <- unlist(data[3, seq((i - 1) * 10 + 1, (i - 1) * 10 + 10)])
  
  # Combine date_time values with column names and values
  temp <- data.frame(date_time = date_times[i], TSK = values[1], PSFC = values[2], U10 = values[3], V10 = values[4], Q2 = values[5], RAINC = values[6], RAINNC = values[7], SNOW = values[8], TSLB = values[9], SMOIS = values[10], stringsAsFactors = FALSE)
  
  # Append the temporary data frame to the result
  result <- rbind(result, temp)
}

# Print the result
print(result)
View(result)

# Data cleaning & Preprocessing

# To remove the X from TIMESTAMP
result$date_time <- gsub("^X", "", result$date_time)

# to get an overview of our dataframe
glimpse(result)

# to check the datatype
typeof(result)

# Check the class of the dataframe
class(result)

# Convert data types of all columns in the dataframe
result<-type.convert(result,s.is=TRUE)
# Using sapply() to check data types of all columns
sapply(result,class)


install.packages('lubridate')
library('lubridate')

# Convert date_time column to POSIXct format
result$date_time<-as.POSIXct(result$date_time,format='%d.%m.%Y.%H.%M')

# To replace the last value(null) of date time column
replace_value <- as.POSIXct("2018-05-31 21:00:00", format = "%Y-%m-%d %H:%M:%S")
# Fill NA values in the first column with the replacement value
result$date_time[is.na(result$date_time)] <- replace_value

View(result)

# HANDLING MISSING VALUES
# Check for missing values in the entire dataframe
missing_values <- is.na(result)
# Count the number of missing values in each column
missing_counts <- colSums(missing_values)
# Print the number of missing values in each column
print(missing_counts)

# Visualize missing values
missing_data <- data.frame(variable = names(missing_counts), missing_values = missing_counts)
ggplot(missing_data, aes(x = variable, y = missing_counts)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Missing Values in Weather Data",
       x = "Variables",
       y = "Number of Missing Values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# To check duplicate values
duplicated(result)

# Replacing NA values in the dataset
names(result)
# Iterate over each column
for (col in names(result)) {
  # Skip the 'date_time' column
  if (col == "date_time") {
    next
  }
  
  # Iterate over each row
  for (i in 1:nrow(result)) {
    # If the value is missing
    if (is.na(result[i, col])) {
      # Find predecessor index
      pred_index <- max(1, i - 1)
      
      # Find successor index
      succ_index <- min(nrow(result), i + 1)
      
      # Replace missing value with predecessor or successor value
      if (!is.na(result[pred_index, col])) {
        result[i, col] <- result[pred_index, col]
      } else if (!is.na(result[succ_index, col])) {
        result[i, col] <- result[succ_index, col]
      }
    }
  }
}
# Print the updated dataframe
print(result)
View(result)

# Calculate wind speed
result$windspeed <- sqrt(result$U10^2 + result$V10^2)

# View the updated dataframe
head(result)

# OUTLIER DETECTION

# outlier detection for TSK
column_name <- "TSK"

# Create a box plot to visualize outliers
ggplot(data = result, aes(x = "", y = .data[[column_name]])) +
  geom_boxplot(fill='yellow', color='blue') +
  labs(title = paste("Box Plot of", column_name)) +
  ylab("Values")

# outlier detection for PSFC
column_name <- "PSFC"
# Create a box plot to visualize outliers
ggplot(data = result, aes(x = "", y = .data[[column_name]])) +
  geom_boxplot(fill='black', color='red') +
  labs(title = paste("Box Plot of", column_name)) +
  ylab("Values")

# outlier detection for U10
column_name <- "U10"
# Create a box plot to visualize outliers
ggplot(data = result, aes(x = "", y = .data[[column_name]])) +
  geom_boxplot(fill='yellow', color='blue') +
  labs(title = paste("Box Plot of", column_name)) +
  ylab("Values")

# outlier detection for V10
column_name <- "V10"
# Create a box plot to visualize outliers
ggplot(data = result, aes(x = "", y = .data[[column_name]])) +
  geom_boxplot(fill='black', color='red') +
  labs(title = paste("Box Plot of", column_name)) +
  ylab("Values")

# outlier detection for Q2
column_name <- "Q2"
# Create a box plot to visualize outliers
ggplot(data = result, aes(x = "", y = .data[[column_name]])) +
  geom_boxplot(fill='yellow', color='blue') +
  labs(title = paste("Box Plot of", column_name)) +
  ylab("Values")

# outlier detection for RAINC
column_name <- "RAINC"
# Create a box plot to visualize outliers
ggplot(data = result, aes(x = "", y = .data[[column_name]])) +
  geom_boxplot(fill='black', color='red') +
  labs(title = paste("Box Plot of", column_name)) +
  ylab("Values")


# outlier detection for RAINNC
column_name <- "RAINNC"
# Create a box plot to visualize outliers
ggplot(data = result, aes(x = "", y = .data[[column_name]])) +
  geom_boxplot(fill='black', color='red') +
  labs(title = paste("Box Plot of", column_name)) +
  ylab("Values")

# outlier detection for SNOW
column_name <- "SNOW"
# Create a box plot to visualize outliers
ggplot(data = result, aes(x = "", y = .data[[column_name]])) +
  geom_boxplot(fill='yellow', color='blue') +
  labs(title = paste("Box Plot of", column_name)) +
  ylab("Values")

# outlier detection for TSLB
column_name <- "TSLB"
# Create a box plot to visualize outliers
ggplot(data = result, aes(x = "", y = .data[[column_name]])) +
  geom_boxplot(fill='yellow', color='blue') +
  labs(title = paste("Box Plot of", column_name)) +
  ylab("Values")

# outlier detection for SMOIS
column_name <- "SMOIS"
# Create a box plot to visualize outliers
ggplot(data = result, aes(x = "", y = .data[[column_name]])) +
  geom_boxplot(colour="black", fill="yellow") +
  labs(title = paste("Box Plot of", column_name)) +
  ylab("Values")


# To check the Mean of the TSK and PSFC
mean(result$TSK)
mean(result$PSFC)

# To check the overview of dataset
summary(result)

# UNIVARIATE ANALYSIS

# Histogram for RAINNC (Cumulative Rainfall at Cumulus)
ggplot(result, aes(x = RAINNC)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Cumulative Rainfall at Cumulus (RAINNC)",
       x = "Cumulative Rainfall at Cumulus (RAINNC)",
       y = "Frequency") +
  theme_minimal()

# Scatter plot for TSK vs. U10 with smooth line
ggplot(result, aes(x = U10, y = TSK)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter plot: TSK vs. U10 with smooth line", x = "U10", y = "TSK")

# Scatter plot for TSK vs. V10 with smooth line
ggplot(result, aes(x = V10, y = TSK)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Scatter plot: TSK vs. V10 with smooth line", x = "V10", y = "TSK")



# Scatter plot for TSK vs. RAINC with smooth line
ggplot(result, aes(x = RAINC, y = TSK)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Scatter plot: TSK vs. RAINC with smooth line", x = "RAINC", y = "TSK")





install.packages("corrplot")
library(corrplot)

# CORRELATION ANALYSIS

# Correlation Analysis
correlation_matrix <- cor(result[c("TSK", "PSFC", "U10", "V10", "Q2", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS", "windspeed")], method="pearson" )
print(correlation_matrix)

# Plot relation matrix
corrplot(correlation_matrix, method = "circle")
corrplot(correlation_matrix, method = "number")

# find the correaltion between variables
head(round(correlation_matrix ,2))

# Load the required library
library(stats)

# H0: There is no significant relationship between TSK and Q2?
# Bivariate analysis on  TSK and Q2 for 'Hypothesis 1':

# Perform correlation coefficient of TSK and Q2
correlation <- cor(result$TSK, result$Q2)

# Print the correlation coefficient
print(paste("Correlation coefficient between TSK and Q2:", correlation))

# Perform a hypothesis test for correlation
cor.test(result$TSK, result$Q2)

# Fit linear regression model
lm_model <- lm(Q2 ~ TSK, data = result)

# Print summary of the regression model
summary(lm_model)

# Visualize scatter plot with linear regression line
ggplot(result, aes(x = TSK, y = Q2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship between Surface Temperature (TSK) and 2-meter Specific Humidity (Q2)",
       x = "Surface Temperature (TSK)",
       y = "2-meter Specific Humidity (Q2)") +
  theme_minimal()


#H0: Thereis no significant linear relationship between wind components (U10 and V10) and soil moisture (SMOIS)?

# MULTIVARIATE ANALYSIS FOR HYPOTHESIS 2

# Generate synthetic data
set.seed(123)  # for reproducibility

data<-data.frame(result$windspeed,result$U10,result$V10,result$SMOIS)
print(names(data))
View(data)


# Fit the linear regression model
mul_model <- lm(result.SMOIS ~ result.windspeed + result.U10 +result.V10, data=data)

# Summary of the model to view coefficients and statistics
summary(mul_model)

# Predict and measure performance
predictions <- predict(mul_model, data)
mse <- mean((data$SMOIS - predictions)^2)
r_squared <- summary(mul_model)$r.squared

# Output MSE and R-squared
print(paste("MSE:", mse))
print(paste("R-squared:", r_squared))


library(ggplot2)

data$predictions <- predict(mul_model, data)

# Plotting soil moisture against wind component (V10)
ggplot(data, aes(x = result.U10, y = result.V10)) +
  geom_point(aes(color = 'Actual'), alpha = 0.5) +  # Actual data points
  geom_line(aes(y = predictions, color = 'Predicted'), size = 1) +  # Regression line
  labs(x = 'Wind Component (V10)', y = 'Soil Moisture (SMOIS)', title = 'Regression Analysis') +
  scale_color_manual("", 
                     breaks = c("Actual", "Predicted"),
                     values = c("blue", "red")) +
  theme_minimal()



# MACHINE LEARNING

# Load necessary libraries
library(caret) 	# For Machine Learning 

install.packages('caTools')
library(caTools)

install.packages('rpart')
library(rpart)

# Set the seed for reproducibility
set.seed(123)
train_indices <- sample(1:nrow(result), 0.8*nrow(result))
train_set <- result[train_indices, ]
test_set<- result[-train_indices, ]

View(train_set)
View(test_set)
nrow(train_set)
nrow(test_set)

# Linear Regression
model_lm <- lm(TSK ~ TSLB + Q2 + RAINC + SMOIS, data = train_set)
predictions_lm <- predict(model_lm, newdata = test_set)

# Evaluate Linear Regression model
mse_lm <- mean((test_set$TSK - predictions_lm)^2)
mae_lm <- mean(abs(test_set$TSK - predictions_lm))
rsquared_lm <- cor(test_set$TSK, predictions_lm)^2



install.packages('randomForest')
library(randomForest)

# Random Forest Regression
model_rf <- randomForest(TSK ~ TSLB + Q2 + RAINC + SMOIS, data = train_set)
predictions_rf <- predict(model_rf, newdata = test_set)

# Evaluate Random Forest model
mse_rf <- mean((test_set$TSK - predictions_rf)^2)
mae_rf <- mean(abs(test_set$TSK - predictions_rf))
rsquared_rf <- cor(test_set$TSK, predictions_rf)^2

# Decision Tree Regression
model_dt <- rpart(TSK ~ TSLB + Q2 + RAINC + SMOIS, data = train_set)
predictions_dt <- predict(model_dt, newdata = test_set)

# Evaluate Decision Tree model
mse_dt <- mean((test_set$TSK- predictions_dt)^2)
mae_dt <- mean(abs(test_set$TSK - predictions_dt))
rsquared_dt <- cor(test_set$TSK, predictions_dt)^2


# Print evaluation metrics
cat("Linear Regression\n")
cat("MSE:", mse_lm, "\n")
cat("MAE:", mae_lm, "\n")
cat("R-squared:", rsquared_lm, "\n\n")

cat("Random Forest Regression\n")
cat("MSE:", mse_rf, "\n")
cat("MAE:", mae_rf, "\n")
cat("R-squared:", rsquared_rf, "\n\n")

cat("Decision Tree Regression\n")
cat("MSE:", mse_dt, "\n")
cat("MAE:", mae_dt, "\n")
cat("R-squared:", rsquared_dt, "\n")


# TIME SERIES FORCASTING

# Load necessary libraries
install.packages('forecast')
library(forecast)


subset<-result[,c("date_time",'TSK')]
subset
tsdata<-ts(subset$TSK,frequency = 8)

# Plot the time series
plot(subset$date_time, tsdata, type = "l", xlab = "Date-Time", ylab = "TSK")
tsdata
summary(tsdata)
class(tsdata)

# Plot the time series to visualize the data
plot(tsdata, main = "TSK Vs Time", col='red')
# Draw a linear line on plot
abline(lm(tsdata~time(tsdata)))

install.packages("tseries")
library(tseries)
# DECOMPOSE THE PLOT before check the stationary
# Decompose Time Series
decomp <- decompose(tsdata)
plot(decomp)

# Perform the Augmented Dickey-Fuller (ADF) test to check the stationary
adf_result <- adf.test(tsdata)
# Print the ADF test result
print(adf_result)

# Diff function to make it stationary
diff_data<-diff(tsdata)
diff_data
plot(diff_data)

# Perform the Augmented Dickey-Fuller (ADF) test to check the stationary
adf_result_diff <- adf.test(diff_data)
print(adf_result_diff)

library(forecast)
# Fit ARIMA model to Diff data
arima_model <- auto.arima(diff_data)
summary(arima_model)
# Validate the model
checkresiduals(arima_model)

# Forecast using the ARIMA model
forecast_result <- forecast(arima_model, h = 10)
plot(forecast_result)

acf(tsdata)
#manually check the value of q
acf(diff_data)
#manually check the value of p
pacf(diff_data)

#manually fit arima model
fit <-  Arima(tsdata, order=c(0,1,1), seasonal=c(1,1,1))

checkresiduals(fit)
# To forecast the model
fit %>%
  forecast(h=10) %>%
  autoplot()
# Print the summary of the fitted model
summary(fit)





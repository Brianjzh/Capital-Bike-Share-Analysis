install.packages("Hmisc")

library(Hmisc)
library(car)
library(dplyr)
library(tidyverse) # tidyverse
library(GGally) # for pairs plots with ggplot
library(caret) # train/test splits and cross validation splits
library(ggplot2)
library(broom)
library(lmtest)
bike_data=read.csv("Capital Bike Sharing data by hour.csv")

### Exploratory Data Analysis --------------------------------------------------
##Summary Statics
summary(bike_data)

##Correlations:
correlation_matrix = bike_data|>
  select_if(is.numeric) |>
  cor()
correlations_with_cnt <- correlation_matrix["cnt", ] |>
  sort(decreasing = TRUE)
print(correlations_with_cnt)

##Multicollinearity

lm_model <- lm(cnt ~ instant + season + yr + mnth + hr + holiday + weekday + workingday + 
                 weathersit + temp + atemp + hum + windspeed + casual + registered, 
               data = bike_data)

vif_values <- vif(lm_model)
print(vif_values)
for (variable in names(vif_values)) {
  vif_value_vector <- vif_values[variable]
  cat(variable, ": VIF =", vif_value_vector, "\n")
  if (vif_value_vector > 10) {
    cat("    --> High multicollinearity detected.\n")
  } else {
    cat("    --> Multicollinearity is within acceptable limits.\n")
  }
}
# Temp, atemp, yr, mnth, instant high multicollinearity

# Create a new variable 'temp_diff' which is the difference between 'temp' and 'atemp'
bike_data$temp_diff <- bike_data$temp - bike_data$atemp

# View the first few rows to confirm the new variable
head(bike_data)


##Visualizations

# Scatter plot for temperature vs. cnt
ggplot(bike_data, aes(x = temp, y = cnt)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue") +
  labs(title = "Relationship between Temperature and Bike Rentals",
       x = "Temperature", y = "Bike Rentals (cnt)")

ggplot(data = data.frame(fitted = lm_model$fitted.values, residuals = lm_model$residuals), 
       aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")

# Boxplot of rentals by season
ggplot(bike_data, aes(x = season, y = cnt)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Bike Rentals by Season", x = "Season", y = "Total Rentals (cnt)")

# Line plot of average rentals by hour for each season
ggplot(bike_data, aes(x = as.numeric(as.character(hr)), y = cnt, group = season, color = season)) +
  geom_line(stat = "summary", fun = mean) +
  labs(title = "Average Bike Rentals by Hour for Each Season", x = "Hour", y = "Average Rentals (cnt)")

# Calculate the correlation matrix
cor_matrix <- cor(select_if(bike_data, is.numeric))

# Convert the correlation matrix to a data frame for ggplot
cor_data <- as.data.frame(as.table(cor_matrix))

# Plot the heatmap
ggplot(cor_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


bikeshare_long <- bike_data %>%
  pivot_longer(cols = c(registered, casual),
               names_to = "user_type",
               values_to = "count")


bikeshare_long = bike_data %>%
  mutate(dteday = as.Date(bike_data$dteday),
         season = as.factor(bike_data$season),
         yr = as.factor(bike_data$yr),
         mnth = as.factor(bike_data$mnth),
         hr = as.factor(bike_data$hr),
         holiday = as.factor(bike_data$holiday),
         weekday = as.factor(bike_data$weekday),
         workingday = as.factor(bike_data$workingday),
         weathersit = as.factor(bike_data$weathersit),
         weatherrecode = as.factor(bike_data$weathersit),
         holidayrecode = as.factor(bike_data$holiday),
         workingdayrecode = as.factor(bike_data$workingday),
         seasonrecode = as.factor(bike_data$season))

str(bikeshare_long)

head(bikeshare_long)

# Recode all necessary variables
bikeshare_long <- bike_data %>%  
  mutate(
    weatherrecode = recode(weathersit,
                           "1" = "Clear, cloudy",
                           "2" = "Mist + cloudy, mist",
                           "3" = "Light snow and rain",
                           "4" = "Heavy rain + ice pallets"),
    holidayrecode = recode(holiday,
                           "1" = "Holiday",
                           "0" = "Non-Holiday"),
    workingdayrecode = recode(workingday,
                              "1" = "if the day is neither a weekend nor holiday",
                              "0" = "not"),
    month = recode(mnth,
                   "1" = "January",
                   "2" = "February",
                   "3" = "March",
                   "4" = "April",
                   "5" = "May",
                   "6" = "June",
                   "7" = "July",
                   "8" = "August",
                   "9" = "September",
                   "10" = "October",
                   "11" = "November",
                   "12" = "December"),
    weekdaynew = recode(weekday,
                        "0" = "Sunday",
                        "1" = "Monday",
                        "2" = "Tuesday",
                        "3" = "Wednesday",
                        "4" = "Thursday",
                        "5" = "Friday",
                        "6" = "Saturday")
  )

#Humidity and Bike Count
str(bikeshare_long)  # To make sure all the expected columns are present
ggplot(bikeshare_long, aes(x = hum, y = cnt)) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  theme_minimal() +
  labs(
    title = "Smoothed Line Plot of Bike Count vs Humidity",
    x = "Humidity",
    y = "Bike Count"
  )

# Bike Demand by Weather Condition
ggplot(bikeshare_long, aes(x = weatherrecode, y = cnt)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Bike Demand by Weather Condition",
    x = "Weather Condition",
    y = "Bike Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Average bike demand on holidays vs. non-holidays
ggplot(bikeshare_long, aes(x = holidayrecode, y = cnt, fill = holidayrecode)) +
  geom_bar(stat = "summary", fun = "mean", alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Average Bike Demand on Holidays vs. Non-Holidays",
    x = "Holiday",
    y = "Average Bike Count"
  ) +
  theme(legend.position = "none")

#Bike Demand on Working Days vs Non-Working Days
ggplot(bikeshare_long, aes(x = workingdayrecode, y = cnt, fill = workingdayrecode)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Bike Demand on Working vs. Non-Working Days",
    x = "Working Day Status",
    y = "Bike Count"
  ) +
  theme(legend.position = "none")

# Bike Demand Per Month
ggplot(bikeshare_long, aes(x = month, y = cnt, group = 1)) +
  geom_line(stat = "summary", fun = "mean", color = "darkblue", size = 1) +
  geom_point(stat = "summary", fun = "mean", color = "darkred", size = 2) +
  theme_minimal() +
  labs(
    title = "Monthly Trend in Bike Demand",
    x = "Month",
    y = "Average Bike Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bike Demand By Day of the Week
ggplot(bikeshare_long, aes(x = weekdaynew, y = cnt, fill = weekdaynew)) +
  geom_bar(stat = "summary", fun = "mean", alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Average Bike Demand by Day of the Week",
    x = "Day of the Week",
    y = "Average Bike Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Bike Demand and Weather Conditions
ggplot(bikeshare_long, aes(x = hr, y = cnt, color = weatherrecode)) +
  geom_line(stat = "summary", fun = "mean") +
  theme_minimal() +
  labs(
    title = "Hourly Bike Demand by Weather Condition",
    x = "Hour of the Day",
    y = "Average Bike Count",
    color = "Weather Condition"
  )

# Heatmap of Bike Count vs Temp
ggplot(bikeshare_long, aes(x = temp, y = cnt)) +
  stat_bin2d(bins = 30) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(
    title = "Heatmap of Bike Count vs Temperature",
    x = "Temperature",
    y = "Bike Count",
    fill = "Count"
  )


# Scatter Plot of Bike Count vs Temperature
ggplot(bikeshare_long, aes(x = temp, y = cnt, color = weatherrecode)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Scatter Plot of Bike Count vs Temperature",
    x = "Temperature",
    y = "Bike Count",
    color = "Weather Condition"
  )

# Adding new user_type 
bikeshare_long <- bike_data %>%
  pivot_longer(cols = c(casual, registered), names_to = "user_type", values_to = "count")

# Bike Demand Casual vs Registered
ggplot(bikeshare_long, aes(x = hr, y = count, fill = user_type)) +
  geom_col(position = "dodge", alpha = 0.7) +
  theme_minimal() +
  labs(
    x = "Hour",
    y = "Count",
    title = "Comparison of Registered vs Casual Riders Across Hours",
    subtitle = "The bar plot shows the count of registered vs casual riders during different hours of the day.",
    fill = "User Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, color = "darkgreen"),
    plot.subtitle = element_text(hjust = 0.5)
  )


### Set up ---------------------------------------------------------------------
##Creating test and training sets

test_indices <- 
  createDataPartition(
    1:nrow(bike_data),
    times = 1,
    p = 0.2
  )
bike_train <- bike_data[-test_indices[[1]], ] # Training set, Validation Set, Test Set
bike_test <- bike_data[test_indices[[1]], ]  #

# Splitting bike_train into training and validation sets
validation_indices <- 
  createDataPartition(
    1:nrow(bike_train), #Split bike_train now
    times = 1, p = 
      0.25
    )
bike_validation <- bike_train[validation_indices[[1]], ]
bike_train <- bike_train[-validation_indices[[1]], ]

summary(bike_train)

#Pairplots
#ggpairs(bike_data[, !names(bike_data) %in% 'dteday'])                                                  #won't load


### Questions to Consider ------------------------------------------------------
##Numeric into categorical
bike_data$season <- as.factor(bike_data$season)
bike_data$weathersit <- as.factor(bike_data$weathersit)
bike_data$yr <- as.factor(bike_data$yr)
bike_data$mnth <- as.factor(bike_data$mnth)
bike_data$hr <- as.factor(bike_data$hr)
bike_data$weekday <- as.factor(bike_data$weekday)
bike_data$holiday <- as.factor(bike_data$holiday)
bike_data$workingday <- as.factor(bike_data$workingday)
str(bike_data) #checking if it turned into a categorical variable

##Outcome varaibles
# cnt (outcome), casual, and registered (since they sum up to cnt).
##Seasonal differences:
# Boxplot of rental counts by season
ggplot(bike_data, aes(x = season, y = cnt)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Bike Rentals by Season", x = "Season", y = "Total Rentals (cnt)")
# Line plot of average rentals by hour for each season
ggplot(bike_data, aes(x = as.numeric(as.character(hr)), y = cnt, group = season, color = season)) +
  geom_line(stat = "summary", fun = mean) +
  labs(title = "Average Bike Rentals by Hour for Each Season", x = "Hour", y = "Average Rentals (cnt)")
# Summary statistics of rentals by season
season_summary <- bike_data |>
  group_by(season) |>
  summarize(
    mean_rentals = mean(cnt, na.rm = TRUE),
    median_rentals = median(cnt, na.rm = TRUE),
    sd_rentals = sd(cnt, na.rm = TRUE)
  )
print(season_summary)
# Summary statistics of rentals by hour
hour_summary <- bike_data |>
  group_by(hr) |>
  summarize(
    mean_rentals = mean(cnt, na.rm = TRUE),
    median_rentals = median(cnt, na.rm = TRUE),
    sd_rentals = sd(cnt, na.rm = TRUE)
  )
print(hour_summary)


### Regression Modeling --------------------------------------------------------

#standardizing the numbers
standardizer <- 
  bike_train |>
  select(
    where(is.numeric), -instant
  ) |>
  preProcess(
    method = c("center", "scale")
  )
bike_train_standardized <- 
  predict(standardizer, bike_train)
bike_validation_standardized <- 
  predict(standardizer, bike_validation)
bike_test_standardized <- 
  predict(standardizer, bike_test)

##Regression model (Removed Multicolinearity variables and unnecessary outcome variables)
bike_multi = lm(cnt ~  season + mnth + temp + hr + holiday + weekday + workingday +   
                      weathersit + hum + windspeed, 
                bike_train_standardized)

summary(bike_multi)

f1_work <- lm(
  cnt ~ 
    season + 
    mnth + 
    temp + 
    hr +
    holiday +
    weekday +
    workingday +
    weathersit +
    windspeed +
    hum +
    I(temp ^ 2) +
    I(hum ^ 2) + 
    I(windspeed ^ 2),
  data = bike_train_standardized
)

summary(f1_work)

#-------------------------------------------------
# Model Violations
# Multicolinearity
vif(f1_work, type="predictor")
# No Multicolinearity detected

# Heteroskedasticity
library(sandwich)             # Load the sandwich package
bptest(f1_work)
# Small P value, reject the null hypothesis
# Calculate Robust Standard Errors:
coeftest(f1_work, vcov = vcovHC(f1_work, type = 'HC0'))
#Remove mnth, workingday, and weathersit

# Residuals vs Fitted plot
plot(f1_work$fitted.values, f1_work$residuals)
abline(h = 0, col = "red")
title("Residuals vs Fitted Values")
# Funnel pattern (Because there's homoskedasticity) 

# Q-Q plot
qqnorm(f1_work$residuals)
qqline(f1_work$residuals, col = "red")
# The tail end goes up a lot
#I will Log CNT

#Influential Points
plot(f1_work, which = 5, main = "Residuals vs. Leverage")
#No outliers or influential points! Yipee!

# Autocorrelation:
durbinWatsonTest(f1_work)
# Because the durbin watson test is < 2 I can reject the null hypothesis and determine there is no Autocorrelation

#-------------------------------------------------

f1 <- lm(
  log(cnt + 1) ~ 
    season + 
    temp + 
    hr +
    holiday +
    weekday +
    windspeed +
    hum +
    I(temp ^ 2) +
    I(hum ^ 2) + 
    I(windspeed ^ 2),
  data = bike_train_standardized
)

summary(f1)

f_back <- step(
  object = f1,
  direction = "back"
)
summary(f_back)
plot(f_back)

f_step <- step(
  object = f1,
  direction = "both"
)
summary(f1)
summary(f_step)
plot(f_step)

#---------------------------
model_summary <- summary(f1)

rmse <- sqrt(mean(model_summary$residuals^2))
cat("RMSE:", rmse, "\n")
range(bike_train$cnt)  # Check range of cnt
sd(bike_train$cnt)      # Check standard deviation of cnt
print(model_summary$coefficients)

# R-squared and Adjusted R-squared
cat("R-squared:", model_summary$r.squared, "\n")
cat("Adjusted R-squared:", model_summary$adj.r.squared, "\n")

# Residuals vs Fitted plot
plot(f1$fitted.values, f1$residuals)
abline(h = 0, col = "red")
title("Residuals vs Fitted Values")

# Q-Q plot
qqnorm(f1$residuals)
qqline(f1$residuals, col = "red")
#-------------------------------------------------------
#model comparison
# Predictions for f1 and f_step
predictions_f1 <- predict(f1, newdata = bike_test_standardized)
predictions_f_step <- predict(f_step, newdata = bike_test_standardized)

# Reverse the log transformation for both sets of predictions
predictions_f1_original <- exp(predictions_f1) - 1
predictions_f_step_original <- exp(predictions_f_step) - 1

# Calculate RMSE for f1
actual <- bike_test_standardized$cnt
rmse_f1 <- sqrt(mean((actual - predictions_f1_original) ^ 2))
print(paste("RMSE for f1:", rmse_f1))

# Calculate RMSE for f_step
rmse_f_step <- sqrt(mean((actual - predictions_f_step_original) ^ 2))
print(paste("RMSE for f_step:", rmse_f_step))

# Base model without polynomial or interaction terms
sum_f_step <- summary(f_step)
p_values_f_step <- sum_f_step$coefficients[, "Pr(>|t|)"]
print(p_values_f_step)

# Model with polynomial terms
sum_f1 <-summary(f1)
p_values_f1 <- sum_f1$coefficients[, "Pr(>|t|)"]
print(p_values_f1)

#f_step is slightly better

### Questions/Analyses to Consider----------------------------------------------
## Marginal Effects (Temp)------ exp(function, put the marginal effect into the exp)
# Extract coefficients
coeffs <- coef(f_step)
beta_temp <- coeffs["temp"]
beta_temp_sq <- coeffs["I(temp^2)"]


# Add a column for the marginal effect of temperature
bike_train_standardized <- bike_train_standardized %>%
  mutate(
    mfx_temp = beta_temp + 2 * beta_temp_sq * temp
  )

# Plot the marginal effect of temperature
bike_train_standardized %>%
  ggplot(aes(x = temp, y = mfx_temp)) +
  geom_line(color = "blue") +
  labs(title = "Marginal Effect of Temperature on cnt", x = "Standardized Temperature", y = "Marginal Effect on cnt") +
  theme_minimal()

# Calculate the critical point for temp
critical_point_estimate <- bike_train_standardized$mfx_temp[which.min(bike_train_standardized$mfx_temp ^ 2)]

# Print the estimated critical point
print(critical_point_estimate)


# Summary of marginal effects before and after the critical point
bike_train_standardized %>%
  filter(temp < critical_point_estimate) %>%
  select(mfx_temp) %>%
  summary()

bike_train_standardized %>%
  filter(temp >= critical_point_estimate) %>%
  select(mfx_temp) %>%
  summary()

summary(bike_data$temp)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 

## Marginal Effects (humidity)------
# Extract coefficients
beta_hum <- coeffs["hum"]
beta_hum_sq <- coeffs["I(hum^2)"]


# Add a column for the marginal effect of temperature
bike_train_standardized <- bike_train_standardized %>%
  mutate(
    mfx_hum = beta_hum + 2 * beta_hum_sq * hum
  )


# Plot the marginal effect of temperature
bike_train_standardized %>%
  ggplot(aes(x = hum, y = mfx_hum)) +
  geom_line(color = "blue") +
  labs(title = "Marginal Effect of Hum on cnt", x = "Standardized Hum", y = "Marginal Effect on cnt") +
  theme_minimal()

# Calculate the critical point for temp
# Estimate the critical point numerically by finding the hum value where mfx_hum is closest to zero
critical_point_estimate_hum <- bike_train_standardized$mfx_hum[which.min(bike_train_standardized$mfx_hum ^ 2)]

# Print the estimated critical point for hum
print(critical_point_estimate_hum)

# Summary of marginal effects before and after the critical point
bike_train_standardized %>%
  filter(hum < critical_point_estimate_hum) %>%
  select(mfx_hum) %>%
  summary()

bike_train_standardized %>%
  filter(hum >= critical_point_estimate_hum) %>%
  select(mfx_hum) %>%
  summary()

summary(bike_train_standardized$hum)



## How do your evluation metrics compar across the trainig, validation and test runs?

# Predictions for training set
train_preds <- predict(f_step, newdata = bike_train_standardized)
# Predictions for validation set
validation_preds <- predict(f_step, newdata = bike_validation_standardized)
# Predictions for test set
test_preds <- predict(f_step, newdata = bike_test_standardized)

# Reverse the log transformation for predictions
train_preds_original <- exp(train_preds) - 1
validation_preds_original <- exp(validation_preds) - 1
test_preds_original <- exp(test_preds) - 1

# Calculate RMSE for the training set
rmse_train_pipe <- sqrt(mean((bike_train_standardized$cnt - train_preds_original) ^ 2))
# Calculate RMSE for the validation set
rmse_validation_pipe <- sqrt(mean((bike_validation_standardized$cnt - validation_preds_original) ^ 2))
# Calculate RMSE for the test set
rmse_test_pipe <- sqrt(mean((bike_test_standardized$cnt - test_preds_original) ^ 2))

# Print RMSE values
cat("RMSE for Training Set:", rmse_train_pipe, "\n")
cat("RMSE for Validation Set:", rmse_validation_pipe, "\n")
cat("RMSE for Test Set:", rmse_test_pipe, "\n")


#Model performance is not changing drastically 

### 4. Interpreation of Results ------------------------------------------------
## Key findings
# 1. Temp has a strong effect, but marginal effect diminishes as temperature increases
# 2. Multicollinearity Identified and addressed
# 3. Seasonal effects, bike rentals vary by season and hour of the day
# 4. Model Selection and complexity. Including polynomial terms improved the model fit. Stepwise didn't help
# 5. Lack of data beyond critical points
# 6. RSME Values are close to one another, my model has good generalization capabilities and there is no overfitting

## Which Variable(s) contributed the most to model fit? 
summary(f1)
# Coefficients 
# 1. Temp has the largest positive coefficient (0.313). Also high T value with Low P value
# 2. Hr has the 2nd largest positive effect (0.287). With very small P value
# 3. Hum has a negative effect on bike rentals (-0.247). High T value (24) and low P value
# 4. Season has a decent positive effect on bike rentals (0.102)
# 5. mnth, workingday, and weathersit do not significantly contribute to the mode

## Changes in independent Variable Affect the outcome variable (Marginal Effect)
# both Hum and Temp are negative and decrease. As Temp and Hum increase bike rentals decrease

#Potential business implications
# yap yap yap yap yap <<<<<<<<<<<<<<<<<<

### Limitations and Assumptions-------------------------------------------------
## Linear Regression assumptions that were violated and how I corrected for them
# Multicollinearity - Removed the variables atemp, yr, mnth, and instant because of high multicolinearity. When running my step model there was no multicolinearity detected
# Heteroskedasticity - There was a strong violation in Heteroskedasticity. In order to combat it I log'd cnt and removed variables based on the Breusch-Pagan test.
# Non-Linearity - Cone Shape Violation
# Residual normality - It's not on the dotted line
# Influential Observations - No Violation
# Omitted Variable Bias - No Violation that I know of (age and gender?)
# Autocorrelation - No Violation
# Marginal Effect - There is a marginal effect for both hum and temp

## Limitations
# 1. Even with polynomial terms my model mainly accounts for linear relationships
# 2. Log Tranformation: Coefficients represent log-counts are less intuitive copmared to actual counts
# 3. Even with log tranformation there's still some heteroskedacity remaining.
# 4. Although there were no influential points, there were strong outliers and extreme obersvations that could pose a risk
# 5. Omitted Variable Bias


# Get GG pair plot
# Explain MArginal effect

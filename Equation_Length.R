#### Plot untransformed data ####
length_subset <- data_extant[data_extant$discription_measurement %in% 
                               c("specimen_length", "lowest_length", "highest_length"), ]
plot(length_subset$measurement_mm, length_subset$body_size_kg)

#### Transforming data ####
hist(length_subset$body_size_kg) # DOES NOT look normally distributed
shapiro.test(length_subset$body_size_kg) # Not Above (p = 3.387e-09)
hist(length_subset$measurement_mm) # DOES look normally distributed 
shapiro.test(length_subset$measurement_mm) # Not Above (p = 0.01206)

#Log Transformed   
log_length_body_mass <- log(length_subset$body_size_kg)
log_length_measurement <- log(length_subset$measurement_mm)
hist(log_length_body_mass) # DOES look normally distributed 
shapiro.test(log_length_body_mass) # Not Above (p = 0.00027)
hist(log_length_measurement) # DOES look normally distributed 
shapiro.test(log_length_measurement) # Just above (p =0.0566)

#Sqrt Transformation
sqrt_length_body_mass <- sqrt(length_subset$body_size_kg)+0.5
sqrt_length_measurement <- sqrt(length_subset$measurement_mm)+0.5
hist(sqrt_length_body_mass) # DOES look normally distributed 
shapiro.test(sqrt_length_body_mass) # Not Above (p = 9.206e-07)
hist(sqrt_length_measurement) # DOES look normally distributed 
shapiro.test(sqrt_length_measurement) # Not above (p = 0.04031)
  
#Changing values of subset table to LOG (best option)
log_length_subset <- data.frame(species = length_subset$species,
                                log_body_size_kg = log(length_subset$body_size_kg),
                                discription_measurement = length_subset$discription_measurement,
                                log_measurement_mm = log(length_subset$measurement_mm))
#Plot log transformed data
plot(log_length_subset$log_measurement_mm, log_length_subset$log_body_size_kg)

#### Perform OLS regression ####
par(mfrow=c(1,1))
m1length <- lm(log_length_subset$log_body_size_kg ~ log_length_subset$log_measurement_mm)
plot(log_length_subset$log_body_size_kg ~ log_length_subset$log_measurement_mm)
abline(m1length)
summary (m1length)
par(mfrow=c(2,2))
plot(m1length)

#### Testing Assumptions of Model (LOG didn't pass normalcy) ####
# Independence: Durbin-Watson test (NOT INDEPENDENT)
library(car)
durbinWatsonTest(m1length) # -> keeps changing......
# Homoscedasticity: Breusch-Pagan test (VARIANCE EQUAL, (p = 0.9863))
install.packages("lmtest")
library(lmtest)
bptest(m1length)
# Homoscedasticity: Visually using Residuals vs. Fitted Plot (NOT VERY CONSTANT)
par(mfrow=c(2,2))
plot(m1length)
abline(h = 0, col = "red")
# Normality: Shapiro-Wilk test (NOT NORMAL (p = 4.887e-05))
shapiro.test(residuals(m1length))
# Normality: Visually using normal Q-Q Plot (Two outliers at extreme end)
par(mfrow=c(2,2))
plot(m1length)
abline(h = 0, col = "red")

#### Perform CLEAN OLS regression (w/out row 5) ####
data_cleaned_length <- log_length_subset[-c(5), ]
m2length <- lm(data_cleaned_length$log_body_size_kg ~ data_cleaned_length$log_measurement_mm)
plot(data_cleaned_length$log_body_size_kg ~ data_cleaned_length$log_measurement_mm,
     xlab = "natural log femur length", ylab = "natural log body size")
abline(m2length)
summary (m2length)
par(mfrow=c(2,2))
plot(m2length)

#switched
m2lengthswitch <- lm(data_cleaned_length$log_measurement_mm ~ data_cleaned_length$log_body_size_kg)
plot(data_cleaned_length$log_measurement_mm ~ data_cleaned_length$log_body_size_kg,
     xlab = "natural log body size", ylab = "natural log femur length")
summary (m2lengthswitch)
#See difference
par(mfrow=c(1,2))
plot(log_length_subset$log_measurement_mm, log_length_subset$log_body_size_kg)
plot(data_cleaned_length$log_body_size_kg ~ data_cleaned_length$log_measurement_mm)

#### Testing Assumptions of CLEAN Model (LOG passed all tests) ####
# Independence: Durbin-Watson test (NOT INDEPENDENT (p = 0.001))
library(car)
durbinWatsonTest(m2length) # -> Keeps changing....................hmmmmmm
# Homoscedasticity: Breusch-Pagan test (Variance equal (p = 0.2229))
install.packages("lmtest")
library(lmtest)
bptest(m2length)
# Homoscedasticity: Visually using Residuals vs. Fitted Plot (NOT VERY CONSTANT)
par(mfrow=c(2,2))
plot(m2length)
abline(h = 0, col = "red")
# Normality: Shapiro-Wilk test (Normal (p = 0.6467))
shapiro.test(residuals(m2length))
# Normality: Visually using normal Q-Q Plot (Very straighttttttt)
par(mfrow=c(2,2))
plot(m2length)
abline(h = 0, col = "red")

#### Creating Equation ####
# Extract coefficients
coefficients_length <- summary(m2length)$coefficients
intercept_length <- coefficients_length[1, 1]
slope_length <- coefficients_length[2, 1]

# Print the equation 
cat("Ln(BM) = ", slope_length, "(Ln(FM)) + ", intercept_length, "\n")

# Extract R-squared
r_squared <- summary(m2length)$r.squared
cat("R-squared:", r_squared, "\n")

# Extract standard error of the regression (residual standard error)
stderr_length <- summary(m2length)$sigma

# Print the values for reference
cat("Intercept:", intercept_length, "\n")
cat("Slope:", slope_length, "\n")
cat("Standard Error:", stderr_length, "\n")

#### PPE, SEE, AIC  ####

# Calculate Mean Percent Prediction Error (PPE)
predicted_values <- predict(m2length)
actual_values <- data_cleaned_length$log_body_size_kg
mean_ppe <- mean(abs((predicted_values - actual_values) / actual_values)) * 100

# Calculate Standard Error of the Estimate (SEE)
residuals <- residuals(m2length)
see <- sqrt(sum(residuals^2) / (length(actual_values) - 2))  # For simple linear regression with one predictor

# Calculate Akaike Information Criterion (AIC)
aic <- AIC(m2length)

# Print or store the results
cat("Mean Percent Prediction Error (PPE):", mean_ppe, "\n")
cat("Standard Error of the Estimate (SEE):", see, "\n")
cat("Akaike Information Criterion (AIC):", aic, "\n")

#### Plot ####
par(mfrow=c(1,1))
m2length <- lm(data_cleaned_length$log_body_size_kg ~ data_cleaned_length$log_measurement_mm)
plot(data_cleaned_length$log_body_size_kg ~ data_cleaned_length$log_measurement_mm,
     xlab = "ln (femur length) - mm", ylab = "ln (body mass) - kg")
abline(m2length)
summary(m2length)
confint(m2length)

pred_conf <- predict(m2length, interval = "confidence")
pred_pred <- predict(m2length, interval = "prediction")
sorted_indices <- order(data_cleaned_length$log_measurement_mm)
sorted_log_measurement_mm <- data_cleaned_length$log_measurement_mm[sorted_indices]
sorted_pred_conf <- pred_conf[sorted_indices, ]
sorted_pred_pred <- pred_pred[sorted_indices, ]

lines(sorted_log_measurement_mm, sorted_pred_conf[, "lwr"], col = "red", lty = 2)
lines(sorted_log_measurement_mm, sorted_pred_conf[, "upr"], col = "red", lty = 2)
lines(sorted_log_measurement_mm, sorted_pred_pred[, "lwr"], col = "blue", lty = 2)
lines(sorted_log_measurement_mm, sorted_pred_pred[, "upr"], col = "blue", lty = 2)

legend("topleft", legend = c("Regression Line", "95% Confidence Interval", "95% Prediction Interval"),
       col = c("black", "red", "blue"), lty = c(1, 2, 2), bty = "o", cex = 0.8, inset = 0.05)

text_box <- 
"Ln(BM) =  3.778659 * (Ln(FL)) - 14.81245 
                                              R2 = 0.9317788
                                         PPE = 30.58405 
                                             SSE = 0.2553061 
                                              AIC = 8.022579"
rect(3.07, 0.05, 3.5, 1.2)
text(3.29,0.62, text_box, cex = 0.8)

#### 95% confidence of coefficients####
coef_conf_intervals <- confint(m2length, level = 0.95)
print(coef_conf_intervals)

#### 95% confidence ####
# 1. Residual Standard Error (RSE)
rse <- summary(m2length)$sigma

# 2. Sample Size (n)
n <- length(m2length$residuals)

# 3. Mean of x values
mean_x <- mean(data_cleaned_length$log_measurement_mm)

# 4. Sum of Squares of x (SS_x)
ss_x <- sum((data_cleaned_length$log_measurement_mm - mean_x)^2)

# Print the values
cat("Residual Standard Error (RSE):", rse, "\n")
cat("Sample Size (n):", n, "\n")
cat("Mean of x values:", mean_x, "\n")
cat("Sum of Squares of x (SS_x):", ss_x, "\n")


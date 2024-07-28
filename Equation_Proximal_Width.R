#### Creating Equation: PROXIMAL WIDTH ####
proximal_width_subset <- data_extant[data_extant$discription_measurement %in% 
                                       c("specimen_proximal_width ", "specimen_proximal_width",
                                         "lowest_proximal_width", "highest_proximal_width"), ]
plot(proximal_width_subset$measurement_mm, proximal_width_subset$body_size_kg)

#### Transforming data ####
hist(proximal_width_subset$body_size_kg) # DOES NOT look normally distributed
shapiro.test(proximal_width_subset$body_size_kg) # Not above (p = 6.332e-06)
hist(proximal_width_subset$measurement_mm) # DOES look normally distributed 
shapiro.test(proximal_width_subset$measurement_mm) # Not Above (p = 0.02871)

#Log Transformed   
log_proximal_body_mass <- log(proximal_width_subset$body_size_kg)
log_proximal_measurement <- log(proximal_width_subset$measurement_mm)
hist(log_proximal_body_mass) # DOES look normally distributed 
shapiro.test(log_proximal_body_mass) # Not Above (0.03088)
hist(log_proximal_measurement) # DOES look normally distributed 
shapiro.test(log_proximal_measurement) # Yes Above (0.2526)

#Sqrt Transformation
sqrt_proximal_body_mass <- sqrt(proximal_width_subset$body_size_kg)+0.5
sqrt_proximal_measurement <- sqrt(proximal_width_subset$measurement_mm)+0.5
hist(sqrt_proximal_body_mass) # DOES NOT look normally distributed 
shapiro.test(sqrt_proximal_body_mass) # Not Above (p = 0.0003196)
hist(sqrt_proximal_measurement) # DOES look normally distributed 
shapiro.test(sqrt_proximal_measurement) # Yes above (0.1092)

#Changing values of subset table to LOG (best option) 
log_proximal_subset <- data.frame(species = proximal_width_subset$species,
                                  log_body_size_kg = log(proximal_width_subset$body_size_kg),
                                  discription_measurement = proximal_width_subset$discription_measurement,
                                  log_measurement_mm = log(proximal_width_subset$measurement_mm))
#Plot log transformed data
plot(log_proximal_subset$log_measurement_mm, log_proximal_subset$log_body_size_kg)

#### Perform OLS regression ####
par(mfrow=c(1,1))
m1prox <- lm(log_proximal_subset$log_body_size_kg ~ log_proximal_subset$log_measurement_mm)
plot(log_proximal_subset$log_body_size_kg ~ log_proximal_subset$log_measurement_mm)
abline(m1prox)
summary (m1prox)
par(mfrow=c(2,2))
plot(m1prox)

#### Testing Assumptions of Model (LOG passed all tests) ####
# Independence: Durbin-Watson test (YES INDEPENDENT (p = 0.846))
library(car)
durbinWatsonTest(m1prox) # -> keeps changing......
# Homoscedasticity: Breusch-Pagan test (VARIANCE EQUAL (p = 0.638))
install.packages("lmtest")
library(lmtest)
bptest(m1prox)
# Homoscedasticity: Visually using Residuals vs. Fitted Plot (NOT VERY CONSTANT)
par(mfrow=c(2,2))
plot(m1prox)
abline(h = 0, col = "red")
# Normality: Shapiro-Wilk test (NOT NORMAL (p = 1.562e-05))
shapiro.test(residuals(m1prox))
# Normality: Visually using normal Q-Q Plot (Not very straight)
par(mfrow=c(2,2))
plot(m1prox)
abline(h = 0, col = "red")

#### Perform CLEAN OLS regression (w/out row 5) ####
data_cleaned_prox <- log_proximal_subset[-c(4), ]
m2prox <- lm(data_cleaned_prox$log_body_size_kg ~ data_cleaned_prox$log_measurement_mm)
par(mfrow=c(1,1))
plot(data_cleaned_prox$log_body_size_kg ~ data_cleaned_prox$log_measurement_mm)
abline(m2prox)
summary (m2prox)
par(mfrow=c(2,2))
plot(m2prox)

#### Testing Assumptions of CLEAN Model (LOG passed all tests) ####
# Independence: Durbin-Watson test (NOT INDEPENDENT (p = 0.001))
library(car)
durbinWatsonTest(m2prox) # -> Keeps changing....................hmmmmmm
# Homoscedasticity: Breusch-Pagan test (Variance equal (p = 0.9737))
install.packages("lmtest")
library(lmtest)
bptest(m2prox)
# Homoscedasticity: Visually using Residuals vs. Fitted Plot (NOT VERY CONSTANT)
par(mfrow=c(2,2))
plot(m2prox)
abline(h = 0, col = "red")
# Normality: Shapiro-Wilk test (Normal (p = 0.6599))
shapiro.test(residuals(m2prox))
# Normality: Visually using normal Q-Q Plot (Very straighttttttt)
par(mfrow=c(2,2))
plot(m2prox)
abline(h = 0, col = "red")

#### Creating Equation ####
# Extract coefficients
coefficients_proximal <- summary(m2prox)$coefficients
intercept_proximal <- coefficients_proximal[1, 1]
slope_proximal <- coefficients_proximal[2, 1]

# Print the equation 
cat("Ln(BM) = ", slope_proximal, "(Ln(FM)) + ", intercept_proximal, "\n")

# Extract R-squared
r_squared <- summary(m2prox)$r.squared
cat("R-squared:", r_squared, "\n")

# Extract standard error of the regression (residual standard error)
stderr_proximal <- summary(m2prox)$sigma

# Print the values for reference
cat("Intercept:", intercept_proximal, "\n")
cat("Slope:", slope_proximal, "\n")
cat("Standard Error:", stderr_proximal, "\n")

#### PPE, SEE, AIC  ####

# Calculate Mean Percent Prediction Error (PPE)
predicted_values <- predict(m2prox)
actual_values <- data_cleaned_prox$log_body_size_kg
mean_ppe <- mean(abs((predicted_values - actual_values) / actual_values)) * 100

# Calculate Standard Error of the Estimate (SEE)
residuals <- residuals(m2prox)
see <- sqrt(sum(residuals^2) / (length(actual_values) - 2))  # For simple linear regression with one predictor

# Calculate Akaike Information Criterion (AIC)
aic <- AIC(m2prox)

# Print or store the results
cat("Mean Percent Prediction Error (PPE):", mean_ppe, "\n")
cat("Standard Error of the Estimate (SEE):", see, "\n")
cat("Akaike Information Criterion (AIC):", aic, "\n")

#### Plot ####
par(mfrow=c(1,1))
m2prox <- lm(data_cleaned_prox$log_body_size_kg ~ data_cleaned_prox$log_measurement_mm)
plot(data_cleaned_prox$log_body_size_kg ~ data_cleaned_prox$log_measurement_mm, 
     xlab = "ln (femur proximal width) - mm", ylab = "ln (body mass) - kg")
abline(m2prox)
summary(m2prox)
confint(m2prox)

pred_conf <- predict(m2prox, interval = "confidence")
pred_pred <- predict(m2prox, interval = "prediction")
sorted_indices <- order(data_cleaned_prox$log_measurement_mm)
sorted_log_measurement_mm <- data_cleaned_prox$log_measurement_mm[sorted_indices]
sorted_pred_conf <- pred_conf[sorted_indices, ]
sorted_pred_pred <- pred_pred[sorted_indices, ]

lines(sorted_log_measurement_mm, sorted_pred_conf[, "lwr"], col = "red", lty = 2)
lines(sorted_log_measurement_mm, sorted_pred_conf[, "upr"], col = "red", lty = 2)
lines(sorted_log_measurement_mm, sorted_pred_pred[, "lwr"], col = "blue", lty = 2)
lines(sorted_log_measurement_mm, sorted_pred_pred[, "upr"], col = "blue", lty = 2)

legend("topleft", legend = c("Regression Line", "95% Confidence Interval", "95% Prediction Interval"),
       col = c("black", "red", "blue"), lty = c(1, 2, 2), bty = "o", cex = 0.8, inset = 0.05)

text_box <- 
  "Ln(BM) =  3.00997 (Ln(FPW)) +  -7.02366  
                                            R2 = 0.9371083 
                                          PPE = 36.31068
                                        SSE =  0.2332372  
                                             AIC = 1.223605 "
rect(3.07, 0.05, 3.5, 1.2)
text(3.29,0.62, text_box, cex = 0.8)

#### 95% confidence of coefficients####
coef_conf_intervals <- confint(m2prox, level = 0.95)
print(coef_conf_intervals)

#### 95% confidence ####
# 1. Residual Standard Error (RSE)
rse <- summary(m2prox)$sigma

# 2. Sample Size (n)
n <- length(m2prox$residuals)

# 3. Mean of x values
mean_x <- mean(data_cleaned_prox$log_measurement_mm)

# 4. Sum of Squares of x (SS_x)
ss_x <- sum((data_cleaned_prox$log_measurement_mm - mean_x)^2)

# Print the values
cat("Residual Standard Error (RSE):", rse, "\n")
cat("Sample Size (n):", n, "\n")
cat("Mean of x values:", mean_x, "\n")
cat("Sum of Squares of x (SS_x):", ss_x, "\n")



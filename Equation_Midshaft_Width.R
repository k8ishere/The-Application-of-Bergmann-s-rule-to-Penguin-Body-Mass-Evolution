#### Creating Equation: MIDSHAFT WIDTH ####
midshaft_width_subset <- data_extant[data_extant$discription_measurement %in% 
                                       c("specimen_midshaft_width", "lowest_midshaft_width", "highest_midshaft_width"), ]
plot(midshaft_width_subset$measurement_mm, midshaft_width_subset$body_size_kg)

#### Transforming data ####
hist(midshaft_width_subset$body_size_kg) # DOES NOT look normally distributed
shapiro.test(midshaft_width_subset$body_size_kg) # You want above 0.05 (NOT ABOVE)
hist(midshaft_width_subset$measurement_mm) # DOES look normally distributed 
shapiro.test(midshaft_width_subset$measurement_mm) # Not Above

#Log Transformed   
log_midshaft_body_mass <- log(midshaft_width_subset$body_size_kg)
log_midshaft_measurement <- log(midshaft_width_subset$measurement_mm)
hist(log_midshaft_body_mass) # DOES look normally distributed 
shapiro.test(log_midshaft_body_mass) # Not Above (0.0002922)
hist(log_midshaft_measurement) # DOES look normally distributed 
shapiro.test(log_midshaft_measurement) # Not Above (0.03489)

#Sqrt Transformation
sqrt_midshaft_body_mass <- sqrt(midshaft_width_subset$body_size_kg)+0.5
sqrt_midshaft_measurement <- sqrt(midshaft_width_subset$measurement_mm)+0.5
hist(sqrt_midshaft_body_mass) # DOES look normally distributed 
shapiro.test(sqrt_midshaft_body_mass) # Not Above (1.133e-06)
hist(sqrt_midshaft_measurement) # DOES look normally distributed 
shapiro.test(sqrt_midshaft_measurement) # Not above (0.002556)

#Changing values of subset table to LOG (best option) 
log_midshaft_subset <- data.frame(species = midshaft_width_subset$species,
                                log_body_size_kg = log(midshaft_width_subset$body_size_kg),
                                discription_measurement = midshaft_width_subset$discription_measurement,
                                log_measurement_mm = log(midshaft_width_subset$measurement_mm))
#Plot log transformed data
plot(log_midshaft_subset$log_measurement_mm, log_midshaft_subset$log_body_size_kg)

#### Perform OLS regression ####
par(mfrow=c(1,1))
m1mid <- lm(log_midshaft_subset$log_body_size_kg ~ log_midshaft_subset$log_measurement_mm)
plot(log_midshaft_subset$log_body_size_kg ~ log_midshaft_subset$log_measurement_mm)
abline(m1mid)
summary (m1mid)
par(mfrow=c(2,2))
plot(m1mid)

#### Testing Assumptions of Model (LOG passed all tests) ####
# Independence: Durbin-Watson test (YES INDEPENDENT (p = 0.05))
library(car)
durbinWatsonTest(m1mid) # -> keeps changing......
# Homoscedasticity: Breusch-Pagan test (VARIANCE EQUAL, assumption met, P-value above 0.05 (0.8726))
install.packages("lmtest")
library(lmtest)
bptest(m1mid)
# Homoscedasticity: Visually using Residuals vs. Fitted Plot (NOT VERY CONSTANT)
par(mfrow=c(2,2))
plot(m1mid)
abline(h = 0, col = "red")
# Normality: Shapiro-Wilk test (NORMAL (P = 0.2153))
shapiro.test(residuals(m1mid))
# Normality: Visually using normal Q-Q Plot (Two outliers at extreme end)
par(mfrow=c(2,2))
plot(m1mid)
abline(h = 0, col = "red")

#### Creating Equation ####
# Extract coefficients
coefficients_midshafts <- summary(m1mid)$coefficients
intercept_midshaft <- coefficients_midshafts[1, 1]
slope_midshaft <- coefficients_midshafts[2, 1]

# Print the equation 
cat("Ln(BM) = ", slope_midshaft, "(Ln(FM)) + ", intercept_midshaft, "\n")

# Extract R-squared
r_squared <- summary(m1mid)$r.squared
cat("R-squared:", r_squared, "\n")

# Extract standard error of the regression (residual standard error)
stderr_mid <- summary(m1mid)$sigma

# Print the values for reference
cat("Intercept:", intercept_midshaft, "\n")
cat("Slope:", slope_midshaft, "\n")
cat("Standard Error:", stderr_mid, "\n")

#### PPE, SEE, AIC  ####

# Calculate Mean Percent Prediction Error (PPE)
predicted_values <- predict(m1mid)
actual_values <- log_midshaft_subset$log_body_size_kg
mean_ppe <- mean(abs((predicted_values - actual_values) / actual_values)) * 100

# Calculate Standard Error of the Estimate (SEE)
residuals <- residuals(m1mid)
see <- sqrt(sum(residuals^2) / (length(actual_values) - 2))  # For simple linear regression with one predictor

# Calculate Akaike Information Criterion (AIC)
aic <- AIC(m1mid)

# Print or store the results
cat("Mean Percent Prediction Error (PPE):", mean_ppe, "\n")
cat("Standard Error of the Estimate (SEE):", see, "\n")
cat("Akaike Information Criterion (AIC):", aic, "\n")

#### Plot ####
par(mfrow=c(1,1))
m1mid <- lm(log_midshaft_subset$log_body_size_kg ~ log_midshaft_subset$log_measurement_mm)
plot(log_midshaft_subset$log_body_size_kg ~ log_midshaft_subset$log_measurement_mm,
     xlab = "ln (femur midshaft width) - mm", ylab = "ln (body mass) - kg")
abline(m1mid)
summary(m1mid)
confint(m1mid)

pred_conf <- predict(m1mid, interval = "confidence")
pred_pred <- predict(m1mid, interval = "prediction")
sorted_indices <- order(log_midshaft_subset$log_measurement_mm)
sorted_log_measurement_mm <- log_midshaft_subset$log_measurement_mm[sorted_indices]
sorted_pred_conf <- pred_conf[sorted_indices, ]
sorted_pred_pred <- pred_pred[sorted_indices, ]

lines(sorted_log_measurement_mm, sorted_pred_conf[, "lwr"], col = "red", lty = 2)
lines(sorted_log_measurement_mm, sorted_pred_conf[, "upr"], col = "red", lty = 2)
lines(sorted_log_measurement_mm, sorted_pred_pred[, "lwr"], col = "blue", lty = 2)
lines(sorted_log_measurement_mm, sorted_pred_pred[, "upr"], col = "blue", lty = 2)

legend("topleft", legend = c("Regression Line", "95% Confidence Interval", "95% Prediction Interval"),
       col = c("black", "red", "blue"), lty = c(1, 2, 2), bty = "o", cex = 0.8, inset = 0.05)

text_box <- 
"Ln(BM) =  2.782303 * (Ln(FWM)) - 4.290021
                                               R2 = 0.8378106
                                              PPE = 58.74393 
                                            SSE = 0.3771924 
                                               AIC = 37.68478"
rect(2.39, 0.05, 2.91, 1.2)
text(2.65,0.62, text_box, cex = 0.8)

#### 95% confidence of coefficients####
coef_conf_intervals <- confint(m1mid, level = 0.95)
print(coef_conf_intervals)

#### 95% confidence of predictions####
# 1. Residual Standard Error (RSE)
rse <- summary(m1mid)$sigma

# 2. Sample Size (n)
n <- length(m1mid$residuals)

# 3. Mean of x values
mean_x <- mean(log_midshaft_subset$log_measurement_mm)

# 4. Sum of Squares of x (SS_x)
ss_x <- sum((log_midshaft_subset$log_measurement_mm - mean_x)^2)

# Print the values
cat("Residual Standard Error (RSE):", rse, "\n")
cat("Sample Size (n):", n, "\n")
cat("Mean of x values:", mean_x, "\n")
cat("Sum of Squares of x (SS_x):", ss_x, "\n")


midshaft_length_proximal <- lm(ln_bodymass ~ ln_midshaft + ln_length + ln_proximal, 
                      data=multiple_regression)
summary(midshaft_length_proximal)
par(mfrow=c(2,2))
plot(midshaft_length_proximal)

#### Testing assumptions
# Homoscedasticity: Breusch-Pagan test (Variance equal (p = 0.2229))
install.packages("lmtest")
library(lmtest)
bptest(midshaft_length_proximal)
# Homoscedasticity: Visually using Residuals vs. Fitted Plot (NOT VERY CONSTANT)
par(mfrow=c(2,2))
plot(midshaft_length_proximal)
abline(h = 0, col = "red")
# Normality: Shapiro-Wilk test (Normal (p = 0.6467))
shapiro.test(residuals(midshaft_length_proximal))
# Normality: Visually using normal Q-Q Plot (Very straighttttttt)
par(mfrow=c(2,2))
plot(midshaft_length_proximal)
abline(h = 0, col = "red")

#### PPE, SEE, AIC
# 1. Calculate Prediction Performance Error (PPE) (assumed to be Mean Absolute Error here)
predictions <- predict(midshaft_length_proximal, multiple_regression)
MAE <- mean(abs(predictions - multiple_regression$ln_bodymass))
cat("Mean Absolute Error (MAE):", MAE, "\n")

# 2. Calculate Standard Error of the Estimate (SEE)
SEE <- summary(midshaft_length_proximal)$sigma
cat("Standard Error of the Estimate (SEE):", SEE, "\n")

# 3. Calculate Akaike Information Criterion (AIC)
AIC_value <- AIC(midshaft_length_proximal)
cat("Akaike Information Criterion (AIC):", AIC_value, "\n")

#### Creating Equation
# Extract the coefficients
coefficients <- coef(midshaft_length_proximal)

# Print the coefficients
print(coefficients)

# Construct the regression equation as a string
intercept <- coefficients[1]
ln_midshaft_coef <- coefficients[2]
ln_length_coef <- coefficients[3]
ln_proximal_coef <- coefficients[4]

equation <- paste0("ln_bodymass = ", round(intercept, 4), 
                   " + ", round(ln_midshaft_coef, 4), " * ln_midshaft", 
                   " + ", round(ln_length_coef, 4), " * ln_length", 
                   " + ", round(ln_proximal_coef, 4), " * ln_proximal")

cat("Regression Equation:\n", equation, "\n")

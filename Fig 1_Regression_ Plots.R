par(mfrow=c(2,2))

#### Graph 1 - Femur Length ####
par(mar = c(2, 4, 2, 1))
length_subset <- data_extant[data_extant$discription_measurement %in% 
                               c("specimen_length", "lowest_length", "highest_length"), ]
log_length_subset <- data.frame(species = length_subset$species,
                                log_body_size_kg = log(length_subset$body_size_kg),
                                discription_measurement = length_subset$discription_measurement,
                                log_measurement_mm = log(length_subset$measurement_mm))
data_cleaned_length <- log_length_subset[-c(5), ]
m2length <- lm(data_cleaned_length$log_body_size_kg ~ data_cleaned_length$log_measurement_mm)
plot(data_cleaned_length$log_body_size_kg ~ data_cleaned_length$log_measurement_mm, 
     cex.lab = 1.1, cex.axis = 1, cex = 1.1, ylab = "ln (body mass) - kg",
     xlab="", xaxt='n', pch = 18)
tick_positions <- pretty(data_cleaned_length$log_measurement_mm)
axis(3, at=tick_positions, labels=tick_positions)
mtext("ln (femur length) - mm", side=3, line=2.8, cex = 0.9)
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

#text_box <- 
#  "Ln(BM) =  3.778659 * (Ln(FL)) - 14.81245 
#                                          R2 = 0.9318 
#                                     PPE = 30.5841 
#                                       SSE = 0.2553 
#                                        AIC = 8.0226"
#rect(4.83, 0.02, 4.40, 1)
#text(4.562,0.4, text_box, cex = 0.6)

#### Graph 2 - Femur Midshaft Width ####
par(mar = c(2, 2, 2, 3))
midshaft_width_subset <- data_extant[data_extant$discription_measurement %in% 
                                       c("specimen_midshaft_width", "lowest_midshaft_width", "highest_midshaft_width"), ]
log_midshaft_subset <- data.frame(species = midshaft_width_subset$species,
                                  log_body_size_kg = log(midshaft_width_subset$body_size_kg),
                                  discription_measurement = midshaft_width_subset$discription_measurement,
                                  log_measurement_mm = log(midshaft_width_subset$measurement_mm))
m1mid <- lm(log_midshaft_subset$log_body_size_kg ~ log_midshaft_subset$log_measurement_mm)
plot(log_midshaft_subset$log_body_size_kg ~ log_midshaft_subset$log_measurement_mm,
     cex.lab = 1.1, cex.axis = 1, cex = 1.1, 
     ylab = "",yaxt='n', xlab="", xaxt='n', pch = 18)
tick_positions <- pretty(log_midshaft_subset$log_measurement_mm)
axis(3, at=tick_positions, labels=tick_positions)
mtext("ln (femur midshaft width) - mm", side=3, line=2.8, cex = 0.9)
tick_positions <- pretty(log_midshaft_subset$log_body_size_kg)
axis(4, at=tick_positions, labels=tick_positions)
mtext("ln (body mass) - kg", side=4, line=2.8, cex = 0.9)
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

#text_box <- 
#  "Ln(BM) =  2.782303 * (Ln(FWM)) - 4.290021
#                                        R2 = 0.8378106
#                                        PPE = 58.74393 
#                                       SSE = 0.3771924 
#                                         AIC = 37.68478"
#rect(2.3, 0.02, 2.9, 1)
#text(2.49,0.38, text_box, cex = 0.5)

#### Graph 3 - Femur Proximal Width ####
par(mar = c(4, 4, 0, 1))

proximal_width_subset <- data_extant[data_extant$discription_measurement %in% 
                                       c("specimen_proximal_width ", "specimen_proximal_width",
                                         "lowest_proximal_width", "highest_proximal_width"), ]
log_proximal_subset <- data.frame(species = proximal_width_subset$species,
                                  log_body_size_kg = log(proximal_width_subset$body_size_kg),
                                  discription_measurement = proximal_width_subset$discription_measurement,
                                  log_measurement_mm = log(proximal_width_subset$measurement_mm))
data_cleaned_prox <- log_proximal_subset[-c(4), ]
m2prox <- lm(data_cleaned_prox$log_body_size_kg ~ data_cleaned_prox$log_measurement_mm)
plot(data_cleaned_prox$log_body_size_kg ~ data_cleaned_prox$log_measurement_mm, 
     cex.lab = 1.1, cex.axis = 1, cex = 1.1, pch = 18,  
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

text_box <- 
  "Ln(BM) =  3.00997 * (Ln(FPW)) - 7.02366  
                                  R2 = 0.9371083 
                                PPE = 36.31068
                                SSE =  0.2332372  
                                   AIC = 1.223605 "
#rect(2.98, 0.02, 3.49, 1)
#text(3.17,0.4, text_box, cex = 0.5)

#### Graph 4 - Legend ####
plot.new()
par(mar = c(4, 4, 2, 3))
legend("topleft", legend = c("Regression Line", "95% Confidence Interval", "95% Prediction Interval"),
       col = c("black", "red", "blue"), lty = c(1, 2, 2), bty = "o", cex = 1.1, inset = 0.05)


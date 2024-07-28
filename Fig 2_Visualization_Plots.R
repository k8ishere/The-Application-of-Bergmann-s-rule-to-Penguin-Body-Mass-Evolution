par(mfrow=c(3,2))

#Cenozoic 
par(mar = c(3, 5.5, 1, 3))
plot(data_extinct_temp$mya_avg,data_extinct_temp$avg_temp,
     type = "l", bty = "u", col = "#92C5DE", 
     xlab = "", ylab = "Temperature (°C)", 
     xlim = c(70, 0), ylim = c(0, 28))
box(lty = "solid")
axis(1, at = seq(0, 70, by = 5), labels = seq(0, 70, by = 5), xaxt = 'n')  
m1 <- lm(data_extinct_temp$avg_temp~data_extinct_temp$mya_avg)
abline(m1, lty = 3)
par(new = TRUE)
plot(data_extinct_temp$mya_avg,data_extinct_temp$body_mass,
     xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     bty = "l", pch = 16, xlim = c(70, 0), ylim = c(0,230))
axis(side = 4, at = pretty(data_extinct_temp$body_mass), 
     labels = pretty(data_extinct_temp$body_mass))
m2 <- lm(data_extinct_temp$body_mass~data_extinct_temp$mya_avg)
abline(m2)
legend("topright", 
       legend = c("Temp", "Temp Line", "Body Mass Line"), 
       col = c("#92C5DE", "black", "black"), 
       lty = c(1, 3, 1), 
       pch = c(NA, NA, NA),
       pt.cex = c(NA, NA, NA), # Adjust the point size if necessary
       bty = "o", cex = 1., inset = c(0.05, 0.05))
summary(m2)

filtered_data <- data_extinct_temp[-c(3), ]
m2.5 <- lm(filtered_data$body_mass~filtered_data$mya_avg)
plot(filtered_data$mya_avg, filtered_data$body_mass,
     xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     bty = "l", pch = 16, xlim = c(70, 0), ylim = c(0,230))
axis(side = 4, at = pretty(filtered_data$body_mass), 
     labels = pretty(filtered_data$body_mass))
abline(m2.5)
summary(m2.5)

#### Paleocene Visual ####
par(mar = c(3, 5.5, 1, 3))
subset_paleocene_data <- subset(data_extinct_temp, mya_avg >= 57 & mya_avg <= 62)
plot(subset_paleocene_data$mya_avg,
     subset_paleocene_data$avg_temp,
     type = "l", bty = "u", col = "#92C5DE", ylim = c(10,25),
     xlab = "Million Years Ago", ylab = "", 
     xlim = c(62, 57), xaxt = 'n')
box(lty = "solid")
axis(1, at = seq(57, 62, by = 1))
m3 <- lm(subset_paleocene_data$avg_temp ~
           subset_paleocene_data$mya_avg)
abline(m3, lty = 3)
par(new = TRUE)
plot(subset_paleocene_data$mya_avg,
     subset_paleocene_data$body_mass,
     pch = 18,
     xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     bty = "l", ylim = c(0,230), xlim = c(62, 57))
axis(side = 4, at = pretty(subset_paleocene_data$body_mass), 
     labels = pretty(subset_paleocene_data$body_mass))
mtext("Body Mass (kg)", side = 4, line = 3, cex = 0.7)
m4 <- lm(subset_paleocene_data$body_mass~
             subset_paleocene_data$mya_avg)
abline(m4)
summary(m4)

filtered_data2 <- subset_paleocene_data[-c(3), ]
m4.5 <- lm(filtered_data2$body_mass ~ filtered_data2$mya_avg)
plot(filtered_data2$mya_avg, filtered_data2$body_mass,
     bty = "l", pch = 16)
abline(m4.5)
summary(m4.5)

# Eocene Visual ####
par(mar = c(3, 5.5, 1, 3))
subset_eocene_data <- subset(data_extinct_temp, mya_avg >= 33 & mya_avg <= 57)
plot(subset_eocene_data$mya_avg,
     subset_eocene_data$avg_temp,
     type = "l", bty = "u", col = "#92C5DE", ylim = c(5,25),
     xlab = "", ylab = "Temperature (°C)", 
     xlim = c(57, 33), xaxt = 'n')
box(lty = "solid")
axis(1, at = seq(33, 57, by = 3))
m5 <- lm(subset_eocene_data$avg_temp ~
           subset_eocene_data$mya_avg)
abline(m5, lty = 3)
par(new = TRUE)
plot(subset_eocene_data$mya_avg,
     subset_eocene_data$body_mass, pch = 18,
     xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     bty = "l", ylim = c(0,100), xlim = c(57, 33))
axis(side = 4, at = pretty(subset_eocene_data$body_mass), 
     labels = pretty(subset_eocene_data$body_mass))
mtext("", side = 4, line = 3, cex = 0.7)
m6 <- lm(subset_eocene_data$body_mass~
             subset_eocene_data$mya_avg)
abline(m6)
summary(m6)

#### Oligocene Visual ####
par(mar = c(3, 5.5, 1, 3))
subset_oligocene_data <- subset(data_extinct_temp, mya_avg >= 23 & mya_avg <= 33)
plot(subset_oligocene_data$mya_avg,
     subset_oligocene_data$avg_temp, 
     type = "l", bty = "u", col = "#92C5DE", ylim = c(2,17),
     xlab = "Million Years Ago", ylab = "",
     xlim = c(34, 24), xaxt = 'n')  
box(lty = "solid")
axis(1, at = seq(24, 34, by = ))
m7 <- lm(subset_oligocene_data$avg_temp ~
           subset_oligocene_data$mya_avg)
abline(m7, lty = 3)
par(new = TRUE)
plot(subset_oligocene_data$mya_avg,
     subset_oligocene_data$body_mass,
     xaxt = "n", yaxt = "n", xlab = "", 
     pch = 18,  xlim = c(33, 23),
     ylab = "", bty = "l", ylim = c(0,125))
axis(side = 4, at = pretty(subset_oligocene_data$body_mass), 
     labels = pretty(subset_oligocene_data$body_mass))
mtext("Body Mass (kg)", side = 4, line = 3, cex = 0.7)
m8 <- lm(subset_oligocene_data$body_mass ~
             subset_oligocene_data$mya_avg)
abline(m8)
summary(m8)
summary(m7)

#### Miocene Visual ####
par(mar = c(3, 5.5, 1, 3))
subset_miocene_data <- subset(data_extinct_temp, mya_avg >= 6 & mya_avg <= 23)
plot(subset_miocene_data$mya_avg,
     subset_miocene_data$avg_temp,
     type = "l", bty = "u", col = "#92C5DE", ylim = c(2,15), 
     xlab = "Million Years Ago", ylab = "Temperature (°C)",
     xlim = c(23, 5))
box(lty = "solid")
m9 <- lm(subset_miocene_data$avg_temp ~
           subset_miocene_data$mya_avg)
abline(m9, lty = 3)
par(new = TRUE)
plot(subset_miocene_data$mya_avg,
     subset_miocene_data$body_mass,
     pch = 18, xlim = c(23, 5),
     xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     bty = "l", ylim = c(0,75))
axis(side = 4, at = pretty(subset_miocene_data$body_mass), 
     labels = pretty(subset_miocene_data$body_mass))
mtext("", side = 4, line = 3, cex = 0.7)
m10 <- lm(subset_miocene_data$body_mass~
             subset_miocene_data$mya_avg)
abline(m10)
summary(m10)


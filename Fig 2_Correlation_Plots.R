
par(mfrow=c(3,2))
#Cenozoic
par(mar = c(3, 5.5, 1, 12))
plot(data_extinct_temp$avg_temp ~ data_extinct_temp$body_mass, 
     bty = "l", pch = 16, 
     ylab = "Temperature (°C)", xlab = "")
box(lty = "solid")
m1 <- lm(data_extinct_temp$avg_temp~data_extinct_temp$body_mass)
abline(m1)

#Paleocene
par(mar = c(3, 5.5, 1, 12))
species7 <- unique(data_extinct_temp$species[data_extinct_temp$epoch_code == "7"])
plot(data_extinct_temp$body_mass [data_extinct_temp$epoch_code == "7"], 
     data_extinct_temp$avg_temp [data_extinct_temp$epoch_code == "7"],
     bty = "l", pch = 18, ylim = c(12.8, 15.5),
     xlab = "", ylab = "", xaxt = 'n')
box(lty = "solid")
axis(1, at = seq(0, 180, by = 20)) 
m2 <- lm(data_extinct_temp$avg_temp [data_extinct_temp$epoch_code == "7"]~
           data_extinct_temp$body_mass [data_extinct_temp$epoch_code == "7"])
abline(m2)

#Eocene
par(mar = c(3, 5.5, 1, 12))
species6 <- unique(data_extinct_temp$species[data_extinct_temp$epoch_code == "6"])
plot(data_extinct_temp$body_mass [data_extinct_temp$epoch_code == "6"], 
     data_extinct_temp$avg_temp [data_extinct_temp$epoch_code == "6"], 
     bty = "l", pch = 18, ylim = c(7.6, 18),
     xlab = "", ylab = "Temperature (°C)", xaxt = 'n')
box(lty = "solid")
axis(1, at = seq(0, 110, by = 10)) 
m3 <- lm(data_extinct_temp$avg_temp [data_extinct_temp$epoch_code == "6"]~data_extinct_temp$body_mass [data_extinct_temp$epoch_code == "6"])
abline(m3)

#Oligocene
par(mar = c(3, 5.5, 1, 12))
species5 <- unique(data_extinct_temp$species[data_extinct_temp$epoch_code == "5"])
plot(data_extinct_temp$body_mass [data_extinct_temp$epoch_code == "5"], 
     data_extinct_temp$avg_temp [data_extinct_temp$epoch_code == "5"], 
     bty = "l", pch = 18, ylim = c(7.399, 9.2),
     xlab = "Body Mass (kg)", ylab = "") 
box(lty = "solid")
m4 <- lm(data_extinct_temp$avg_temp [data_extinct_temp$epoch_code == "5"]~data_extinct_temp$body_mass [data_extinct_temp$epoch_code == "5"])
abline(m4)

#Miocene
par(mar = c(3, 5.5, 1, 12))
species4 <- unique(data_extinct_temp$species[data_extinct_temp$epoch_code == "4"])
plot(data_extinct_temp$body_mass [data_extinct_temp$epoch_code == "4"], 
     data_extinct_temp$avg_temp [data_extinct_temp$epoch_code == "4"], 
     bty = "l", pch = 18, ylim = c(4.9, 9.4),
     xlab = "Body Mass (kg)", ylab = "Temperature (°C)") 
box(lty = "solid")
m5 <- lm(data_extinct_temp$avg_temp [data_extinct_temp$epoch_code == "4"]~data_extinct_temp$body_mass [data_extinct_temp$epoch_code == "4"])
abline(m5)


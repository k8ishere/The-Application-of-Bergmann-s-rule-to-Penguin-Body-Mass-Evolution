plot(temp$age,temp$temperature,
     type = "l", bty = "u", col = "#92C5DE", 
     xlab = "Million Years Ago", ylab = "Temperature (°C)", xlim = c(70, 0))

#Cenozoic
par(mfrow=c(1,1))
shapiro.test(data_extinct$body_mass)
shapiro.test(data_extinct$avg_temp)
cor.test(data_extinct$body_mass, data_extinct$avg_temp, method = "spearman") 
length(data_extinct$body_mass)

boxplot(data_extinct$body_mass)
filtered_data3 <- data_extinct[-c(7), ]
boxplot(filtered_data3$body_mass)
shapiro.test(filtered_data3$body_mass)
cor.test(filtered_data3$body_mass, filtered_data3$avg_temp,
         method = "spearman")
length(filtered_data3$body_mass)

#Paleocene
shapiro.test(data_extinct$body_mass[data_extinct$epoch == "paleocene"])
shapiro.test(data_extinct$avg_temp[data_extinct$epoch == "paleocene"])
cor.test(data_extinct$body_mass [data_extinct$epoch == "paleocene"],
         data_extinct$avg_temp [data_extinct$epoch == "paleocene"],
         method = "spearman")
length(data_extinct$body_mass[data_extinct$epoch == "paleocene"])

filtered_data <- data_extinct[data_extinct$epoch == "paleocene", ]
boxplot(filtered_data$body_mass)
filtered_data2 <- filtered_data[-c(7), ]
boxplot(filtered_data2$body_mass)
shapiro.test(filtered_data2$body_mass)
cor.test(filtered_data2$body_mass, filtered_data2$avg_temp,
         method = "spearman")
length(filtered_data2$body_mass)

#Eocene
shapiro.test(data_extinct$body_mass[data_extinct$epoch == "eocene"])
shapiro.test(data_extinct$avg_temp[data_extinct$epoch == "eocene"])
cor.test(data_extinct$body_mass [data_extinct$epoch == "eocene"], 
         data_extinct$avg_temp [data_extinct$epoch == "eocene"],
         method = "spearman")
length(data_extinct$body_mass[data_extinct$epoch == "eocene"])

#Oligocene
shapiro.test(data_extinct$body_mass[data_extinct$epoch == "oligocene"])
shapiro.test(data_extinct_temp$avg_temp[data_extinct$epoch == "oligocene"])
cor.test(data_extinct$body_mass [data_extinct$epoch == "oligocene"], 
         data_extinct$avg_temp [data_extinct$epoch == "oligocene"],
         method = "spearman")
length(data_extinct$body_mass[data_extinct$epoch == "oligocene"])

#Paleocene
shapiro.test(data_extinct$body_mass[data_extinct$epoch == "paleocene"])
shapiro.test(data_extinct$avg_temp[data_extinct$epoch == "paleocene"])
cor.test(data_extinct$body_mass [data_extinct$epoch == "paleocene"], 
         data_extinct$avg_temp [data_extinct$epoch == "paleocene"],
         method = "spearman")
length(data_extinct$body_mass[data_extinct$epoch == "paleocene"])

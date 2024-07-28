#### Anova Epoch overall + bar plot ####
data_extinct$epoch_code <- as.factor(data_extinct$epoch)
data_extinct$epoch_code <- as.factor(data_extinct$epoch_code)
m1 <-lm(data_extinct$body_mass ~ data_extinct$epoch)

par(mfrow = c(1,1))
data_extinct$epoch <- factor(data_extinct$epoch, 
                             levels = c("paleocene", "eocene", 
                                        "oligocene", "miocene", 
                                        "pliocene", "holocene"))
boxplot(data_extinct$body_mass ~ data_extinct$epoch, 
        ylab = "Body Mass (kg)", xlab = "")

par(mfrow = c(2,2))
plot (m1)
summary(m1)

#### Only between paleocene and eocene ####
filtered_data <- subset(data_extinct, epoch %in% c("paleocene", "eocene"))

par(mfrow = c(1,1))
boxplot(filtered_data$body_mass ~ filtered_data$epoch,
        main="Body Mass by Epoch",
        xlab="Epoch",
        ylab="Body Mass",
        col=c("lightblue", "lightgreen"))

m2 <-lm(filtered_data$body_mass ~ filtered_data$epoch)
par(mfrow = c(2,2))
plot (m2)
summary(m2)
anova(m2)
length(filtered_data$body_mass)
length(filtered_data$epoch)


#### Only between Eocene & Oligocene ####
filtered_data2 <- subset(data_extinct, epoch %in% c("eocene", "oligocene"))

par(mfrow = c(1,1))
boxplot(filtered_data2$body_mass ~ filtered_data2$epoch,
        main="Body Mass by Epoch",
        xlab="Epoch",
        ylab="Body Mass",
        col=c("lightblue", "lightgreen"))

m3 <-lm(filtered_data2$body_mass ~ filtered_data2$epoch)
par(mfrow = c(2,2))
plot (m3)
summary(m3)
anova(m3)
length(filtered_data2$body_mass)
length(filtered_data2$epoch)

#### Only between oligocene and miocene ####
filtered_data3 <- subset(data_extinct, epoch %in% c("miocene", "oligocene"))

par(mfrow = c(1,1))
boxplot(filtered_data3$body_mass ~ filtered_data3$epoch,
        main="Body Mass by Epoch",
        xlab="Epoch",
        ylab="Body Mass",
        col=c("lightblue", "lightgreen"))
m4 <-lm(filtered_data3$body_mass ~ filtered_data3$epoch)
par(mfrow = c(2,2))
plot (m4)
summary(m4)
anova(m4)
length(filtered_data3$body_mass)
length(filtered_data3$epoch)

#### ANOVA tests locations by Epoch ####
m5 <-lm(data_extinct$body_mass ~ data_extinct$loc_col)
boxplot(data_extinct$body_mass ~ data_extinct$loc_col)
summary(m5)

m6 <-lm(data_extinct$body_mass [data_extinct_temp$epoch_code == "7"]
        ~ data_extinct$loc_col [data_extinct_temp$epoch_code == "7"])
boxplot(data_extinct$body_mass [data_extinct_temp$epoch_code == "7"]
        ~ data_extinct$loc_col [data_extinct_temp$epoch_code == "7"])
summary(m6)

m7 <-lm(data_extinct$body_mass [data_extinct_temp$epoch_code == "6"]
        ~ data_extinct$loc_col [data_extinct_temp$epoch_code == "6"])
boxplot(data_extinct$body_mass [data_extinct_temp$epoch_code == "6"]
        ~ data_extinct$loc_col [data_extinct_temp$epoch_code == "6"])
summary(m7)
filtered_data4 <- subset(data_extinct, loc_col %in% c("blue", "green"))
boxplot(filtered_data4$body_mass [data_extinct_temp$epoch_code == "6"]
        ~ filtered_data4$loc_col [data_extinct_temp$epoch_code == "6"])
m7.5 <- lm(filtered_data4$body_mass [data_extinct_temp$epoch_code == "6"]
        ~ filtered_data4$loc_col [data_extinct_temp$epoch_code == "6"])
summary(m7.5)

m8 <-lm(data_extinct$body_mass [data_extinct_temp$epoch_code == "5"]
        ~ data_extinct$loc_col [data_extinct_temp$epoch_code == "5"])
boxplot(data_extinct$body_mass [data_extinct_temp$epoch_code == "5"]
        ~ data_extinct$loc_col [data_extinct_temp$epoch_code == "5"])
summary(m8)

m9 <-lm(data_extinct$body_mass [data_extinct_temp$epoch_code == "4"]
        ~ data_extinct$loc_col [data_extinct_temp$epoch_code == "4"])
boxplot(data_extinct$body_mass [data_extinct_temp$epoch_code == "4"]
        ~ data_extinct$loc_col [data_extinct_temp$epoch_code == "4"])
summary(m9)

m10 <-lm(data_extinct$body_mass [data_extinct_temp$epoch_code == "3"]
        ~ data_extinct$loc_col [data_extinct_temp$epoch_code == "3"])
boxplot(data_extinct$body_mass [data_extinct_temp$epoch_code == "3"]
        ~ data_extinct$loc_col [data_extinct_temp$epoch_code == "3"])
summary(m10)

#### TABLE locations ####
par(mfrow = c(1,1))
data_extinct$epoch <- factor(data_extinct$epoch, 
                             levels = c("paleocene", "eocene", 
                                        "oligocene", "miocene", 
                                        "pliocene", "holocene"))
boxplot(data_extinct$body_mass ~ data_extinct$epoch,
        ylab = "Body Mass (kg)", xlab = "", , cex.lab = 1.2, cex.axis = 1.1)

par(mfrow = c(2,3))
colors <- c("#276AB3", "#4F9153", "#e9c716")
par(mar = c(3, 5, 1, 1))
boxplot(data_extinct$body_mass ~ data_extinct$loc_col,
        xlab = "", ylab = "Body Mass (kg)", col = colors, cex.lab = 1.2, cex.axis = 1.2)
boxplot(data_extinct$body_mass [data_extinct_temp$epoch_code == "7"]
        ~ data_extinct$loc_col [data_extinct_temp$epoch_code == "7"],
        xlab = "", ylab = "", col = colors, cex.axis = 1.2)
boxplot(data_extinct$body_mass [data_extinct_temp$epoch_code == "6"]
        ~ data_extinct$loc_col [data_extinct_temp$epoch_code == "6"],
        xlab = "", ylab = "", col = colors, cex.axis = 1.2, ylim = c(0,130))
boxplot(data_extinct$body_mass [data_extinct_temp$epoch_code == "5"]
        ~ data_extinct$loc_col [data_extinct_temp$epoch_code == "5"],
        xlab = "", ylab = "Body Mass (kg)", col = "#4F9153", ylim = c(0,130), 
        cex.lab = 1.2, cex.axis = 1.2)
boxplot(data_extinct$body_mass [data_extinct_temp$epoch_code == "4"]
        ~ data_extinct$loc_col [data_extinct_temp$epoch_code == "4"],
        xlab = "", ylab = "", col = "#4F9153", cex.axis = 1.2)
boxplot(data_extinct$body_mass [data_extinct_temp$epoch_code == "3"]
        ~ data_extinct$loc_col [data_extinct_temp$epoch_code == "3"],
        xlab = "", ylab = "", col = "#4F9153", cex.axis = 1.2)

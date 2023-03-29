#############################################################################
#############################################################################

png(file="one_way_EDA.png", width=900, height=1100)
par(mfrow = c(4, 3))

barplot(table(co2_df$econ.class),
        main='Economic Classes',
        col = coul)

barplot(table(co2_df$drive),
        main='Drive Types',
        col = coul)

barplot(table(co2_df$Vehicle.Class),
        main='Vehicle Classes',
        col = coul)

barplot(table(co2_df$transmission.type),
        main='Transmission Type',
        col = coul)

barplot(table(co2_df$Fuel.Type),
        main='Fuel Type',
        col = coul)

hist(co2_df$Engine.Size, col=coul[1], 
     main = "Histogram For Engine Size",
     xlab = "Engine Size")


hist(co2_df$Cylinders, col=coul[1], 
     main = "Histogram For Cylinders",
     xlab = "Cylinders", 
     breaks = 5)

hist(co2_df$gear.num,
     main='Histogram For Number of Gears',
     xlab = "Number of Gears",
     col = coul[1])

hist(co2_df$Fuel.Consumption.City, col=coul[1], 
     main = "Histogram For Fuel Consumption L/100km (City)",
     xlab = "Fuel Consumption L/100km (City)")


hist(co2_df$Fuel.Consumption.Hwy, col=coul[1], 
     main = "Histogram For Fuel Consumption L/100km (Hwy)",
     xlab = "Fuel Consumption L/100km (Hwy)")

hist(co2_df$CO2.Emissions, col=coul[1], 
     main = "Histogram For CO2 Emissions",
     xlab = " CO2 Emissions")


dev.off()


#############################################################################
#############################################################################

png(file="two_way_EDA.png", width=900, height=1100)
par(mfrow = c(4, 3))

boxplot(co2_df$CO2.Emissions~co2_df$econ.class,  
        main="CO2 Emissions vs. Economic Classes",
        xlab = "",
        ylab = "CO2 Emissions",
        col = coul[1])

boxplot(co2_df$CO2.Emissions~co2_df$drive,  
        main="CO2 Emissions vs. Drive Type",
        xlab = "",
        ylab = "CO2 Emissions",
        col = coul[1])

boxplot(co2_df$CO2.Emissions~co2_df$Vehicle.Class,  
        main="CO2 Emissions vs Vehicle Classes",
        xlab = "",
        ylab = "CO2 Emissions",
        col = coul[1])

boxplot(co2_df$CO2.Emissions~co2_df$transmission.type,  
        main= "CO2 Emissions vs. Transmission Type",
        xlab = "Transmission Type",
        ylab = "CO2 Emissions",
        col = coul[1])

boxplot(co2_df$CO2.Emissions~co2_df$Fuel.Type,  
        main="CO2 Emissions vs. Fuel Type",
        xlab = "Fuel Type",
        ylab = "CO2 Emissions",
        col = coul[1])

plot(co2_df$CO2.Emissions~co2_df$Engine.Size,
     main="CO2 Emissions vs. Engine Size",
     xlab = "Engine Size",
     ylab = "CO2 Emissions",
     col = coul[1])

plot(co2_df$CO2.Emissions~co2_df$Cylinders,  
     main="CO2 Emissions vs. Cylinders",
     xlab = "Cylinders",
     ylab = "CO2 Emissions",
     col = coul[1])

plot(co2_df$CO2.Emissions~co2_df$gear.num,  
     main="CO2 Emissions vs. Number of Gears",
     xlab = "Number of Gears",
     ylab = "CO2 Emissions",
     col = coul[1])

plot(co2_df$CO2.Emissions~co2_df$Fuel.Consumption.City,
     main="CO2 Emissions vs Fuel Consumption L/100km (City)",
     xlab = "Fuel Consumption L/100km (City)",
     ylab = "CO2 Emissions",
     col = coul[1])

plot(co2_df$CO2.Emissions~co2_df$Fuel.Consumption.Hwy,
     main="CO2 Emissions vs Fuel Consumption L/100km (Hwy)",
     xlab = "Fuel Consumption L/100km (Hwy)",
     ylab = "CO2 Emissions",
     col = coul[1])

dev.off()


#############################################################################
#############################################################################

png(file="corr.png", width=500, height=700)
par(mfrow = c(1, 1))
M <- cor(data.num, method = "pearson")
colnames(M) <- c("Engine Size", "Fuel Consumption (City)", "Cylinders", "Fuel Consumption (Hwy)", "CO2 Emissions")
rownames(M) <- c("Engine Size", "Fuel Consumption (City)", "Cylinders", "Fuel Consumption (Hwy)", "CO2 Emissions")
corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45)
dev.off()

png(file="vif.png", width=700, height=700)
par(mar=c(5,10,4,1)+.4)
vnames <- c("Vehicle Class", "Engine Size", "Cylinders", "Fuel Type", "Fuel Consumption (City)",
            "Fuel Consumption (Hwy)", "Economic Class", "Drive", "Transmission",
            "Number of Gears")

barplot(vif_values$`GVIF^(1/(2*Df))`, 
        main = "VIF Values (Generalized)", 
        names.arg = vnames,
        horiz = TRUE,
        las=1,
        col = coul)

dev.off()


#############################################################################
#############################################################################

png(file="screePC1.png", width=600, height=500)
par(mfrow = c(1, 1))

plot(1:length(PCs$sdev), var_explained,
     type = "l", lty = 1, lwd = 3,
     main = "PCA -- Scree Plot (One-hot / Pearson's)",
     xlab = "Sample Principal Component Number",
     ylab = "Variance Explained")
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)      # Grid line width

dev.off()


png(file="screePC2.png", width=600, height=500)
par(mfrow = c(1, 1))

plot(1:length(PCs$sdev), var_explained,
     type = "l", lty = 1, lwd = 3,
     main = "PCA -- Scree Plot (Mixed Correlations)",
     xlab = "Sample Principal Component Number",
     ylab = "Variance Explained")
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)      # Grid line width

dev.off()



png(file="screeFA1.png", width=600, height=500)
par(mfrow = c(1, 1))

plot(1:12, var_exp_mle,
     type = "l", lty = 1, lwd = 3,
     main = "FA (MLE) -- Scree Plot (One-hot / Pearson's)",
     xlab = "Factor Number",
     ylab = "Variance Explained")
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)      # Grid line width


dev.off()


png(file="screeFA2.png", width=600, height=500)
par(mfrow = c(1, 1))

plot(1:9, var_exp_mle,
     type = "l", lty = 1, lwd = 3,
     main = "FA (MLE) -- Scree Plot (Mixed Correlations)",
     xlab = "Factor Number",
     ylab = "Variance Explained")
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)      # Grid line width


dev.off()



png(file="pca_lm_diag.png", width=900, height=700)

par(mfrow=c(2,2))
plot(pca.lm, which = c(1, 2, 4, 5))

dev.off()
















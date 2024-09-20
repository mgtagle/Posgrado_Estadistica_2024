# Clase Anova

sitios <- read.csv("Datos_Rascon_Anova.csv", header = T)

tapply(sitios$DAP, sitios$Paraje, mean)
tapply(sitios$DAP, sitios$Paraje, var)

boxplot(sitios$DAP ~ sitios$Paraje)

tapply(sitios$EDAD, sitios$Paraje, mean)

shapiro.test(sitios$DAP)
bartlett.test(sitios$DAP ~ sitios$Paraje)

# Los datos de DAP no son normales, aunque sus varianzas si son 
# homogeneas

sitios$dap_t <- sqrt(sitios$DAP)

shapiro.test(sitios$dap_t)

boxplot(sitios$dap_t ~ sitios$Paraje)

mean(sitios$dap_t^2)
mean(sitios$DAP)


sit.aov <- aov(sitios$dap_t ~ sitios$Paraje)
summary(sit.aov)

TukeyHSD(sit.aov)
plot(TukeyHSD(sit.aov))

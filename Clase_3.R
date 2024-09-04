
# Descarga de sitios abiertos ---------------------------------------------


prof_url <- "http://www.profepa.gob.mx/innovaportal/file/7635/1/accionesInspeccionfoanp.csv"
profepa <- read.csv(prof_url,  encoding = "latin1")
head(profepa)

summary(profepa)

mean(profepa$Inspección)

ins <- subset(profepa, profepa$Inspección >= mean(profepa$Inspección))

bajo <- subset(profepa, profepa$Inspección <= mean(profepa$Inspección))

cero <- subset(profepa, profepa$Inspección == 0)

todos <- subset(profepa, profepa$Inspección != 0)


# Descarga de datos seguros -----------------------------------------------

library(repmis)
conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")
conjunto$Especie <- as.factor(conjunto$Especie)
conjunto$Clase <- as.factor(conjunto$Clase)
summary(conjunto)

boxplot(conjunto$Diametro ~ conjunto$Especie)

SpFH <- subset(conjunto, conjunto$Especie != "C")
tapply(SpFH$Diametro, SpFH$Especie, mean)

boxplot(SpFH$Diametro ~ SpFH$Especie,
        xlab = "Especies",
        ylab = "Diámetro",
        col = "indianred")

# Normalidad de datos -----------------------------------------------------
shapiro.test(SpFH$Diametro)


# Homogeneidad de varianzas -----------------------------------------------

bartlett.test(SpFH$Diametro, SpFH$Especie)


# Prueba de t independientes ----------------------------------------------

t.test(SpFH$Diametro ~ SpFH$Especie, var.equal = TRUE)

# prueba de t de una muestra ----------------------------------------------

t.test(conjunto$Diametro, mu = 16.4)


prod <- read.csv("Tareas/produccion1.csv", header = T)

boxplot(prod$Kgsem ~ prod$Tiempo)

t.test(prod$Kgsem ~ prod$Tiempo, paired = T)
tapply(prod$Kgsem, prod$Tiempo, mean)

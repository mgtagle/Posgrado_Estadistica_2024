# Ejercicio datos de madera MET
# Importar datos

url <- "https://raw.githubusercontent.com/mgtagle/Met_Est_2024/main/Datos_Madera_MET.csv"
madera <- read.csv(url, header = T)


# Establecer la hipotesis de partida
# H0 = No existe una diferencia entre el peso de la madera de las especies
# Barreta y Gavia
# H1 =  Existe una diferencia entre el peso de la madera de las especies
# Barreta y Gavia


# Representación gráfica de la variable y sus niveles de factor

boxplot(madera$Peso_g ~ madera$Especie,
        xlab = "Niveles de factor",
        ylab = "Peaso (gr)",
        col = "indianred")

# Estadísticas descriptivas de la variable en común (peso_g)
tapply(madera$Peso_g, madera$Especie, mean)
tapply(madera$Peso_g, madera$Especie, var)
# La varianza del peso de la especies Gavia es 7 veces mayor que 
# la varianza de el peso observado en la barreta.

# ¿Cuántos niveles de factor existen?
# Existen dos niveles de factor "Especies: Barreta y Gavia"

# Realizar la prueba de t, así como revisar si los supuestos de normalidad
# y homogeneidad de varianzas se cumple

shapiro.test(madera$Peso_g)
bartlett.test(madera$Peso_g ~ madera$Especie)
madera$peso_t <- log(madera$Peso_g + 1)
tapply(madera$peso_t, madera$Especie, var)
boxplot(madera$peso_t ~  madera$Especie)

t.test(madera$Peso_g ~ madera$Especie, var.equal = F)

# ¿Qué hipotesis se acepta?

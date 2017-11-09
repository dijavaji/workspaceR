
# Document   : Correccion de la prueba de Diseno de experimentos
# Created on : 04-07-2017
# Author     : dvasquez


#EJERCICIO 1
#Comparamos 4 tratamientos clínicos (A, B, C, D) asignando al azar 30 sujetos a los mismos. 
#Las puntuaciones de los sujetos en la VD (un cuestionario de escala de 0 a 150 puntos) fueron:
  
#A: 42, 100, 63, 55, 83, 45
#B: 45, 74, 33, 29, 48, 55, 81, 31
#C: 64, 82, 64, 74, 56, 77, 96 ,55
#D: 109, 20, 35, 97, 101, 98, 74, 88

#verificamos libreria para importar archivo de datos spss
library('Hmisc')

#install.packages('Hmisc')
library('Hmisc')
library('car')
install.packages('car')
install.packages('nloptr')

help("spss.get")
# importamos los datos.
tratamiento.data <- spss.get('/home/thc/Documents/Diego/DisenoExperimentos/Prueba2/Ejercicio 1.sav', use.value.labels=FALSE, to.data.frame = TRUE)

print(tratamiento.data)

print(summary(tratamiento.data))

#1.-Compara si las varianzas de los 4 grupos son similares
#Prueba de homogeneidad de varianzas
#estadistico de levene
with(tratamiento.data, leveneTest(Puntuacion, Tratamiento))

help(leveneTest)

#2.-Analiza si hay diferencias entre los grupos 

tratamiento.data$Tratamiento = factor(tratamiento.data$Tratamiento,
                        labels = c("tratamiento A", "tratamiento B", "tratamiento C","tratamiento D"))

#creamos un grafico de cajas
ggplot(tratamiento.data, aes(x = Tratamiento, y = Puntuacion)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Grupo de tratamiento") +
  ylab("Puntuacion")

#investigamos las diferencias que se ajustan al modelo utilizando ANOVA 
#miramos las estimaciones de parametros y errores estandar para los tratamientos.

tratamiento.mod1 <- lm(Puntuacion ~ Tratamiento, data = tratamiento.data)

summary(tratamiento.mod1)

#la tabla del analisis de varianza se produce con el comando
anova(tratamiento.mod1)

#el siguiente comando utilizamos para generar un intervalo de confianza por defecto al 95%
confint(tratamiento.mod1)

tratamiento.mod = data.frame(Ajustado = fitted(tratamiento.mod1),
                       Residuos = resid(tratamiento.mod1), Tratamiento = tratamiento.data$Tratamiento)

ggplot(tratamiento.mod, aes(Ajustado, Residuos, colour = Tratamiento)) + geom_point()

#3.- Cual es el grupo que rinde mejor? ¿Y el peor?

#Agrupar informacion utilizando el metodo de Tukey y una confianza de 95%
library(agricolae)

tratamiento.av <- aov(tratamiento.mod1)

summary(tratamiento.av)

tratamientoTukeyTest <- TukeyHSD(tratamiento.av)
print(tratamientoTukeyTest)

plot(tratamientoTukeyTest)

#Agrupar informacion utilizando el metodo LSD de Fisher y una confianza de 95%
library(agricolae)
install.packages('agricolae')

tratamientoFisherTest <- LSD.test(tratamiento.data$Puntuacion, tratamiento.data$Tratamiento, 30.5, 1.13)
print(tratamientoFisherTest)

#rm(yield)
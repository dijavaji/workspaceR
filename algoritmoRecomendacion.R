# Document   : Tarea 2 analisis exploratorio
# Created on : 07-10-2017
# Author     : dvasquez

#Algoritmos de recomendacion
library(FactoMineR)

#Abra desde “FactoMineR” el archivo “EjemploAlgoritmosRecomendación.csv y visualizar

getwd()  # get current working directory

setwd('/home/thc/Documents/Diego/mineriaDatos/material/')  # set working directory 
data.file <- file.path('/home/thc/Documents/Diego/mineriaDatos/material', 'EjemploAlgoritmosRecomendacion.csv')

#leemos el archivo
#df <- read.delim("EjemploAlgoritmosRecomendacion.csv", header=TRUE, sep = ";") 
#df <- read.csv(data.file, header = TRUE, sep = ';')
df <- read.csv2("EjemploAlgoritmosRecomendacion.csv")

View(df) #mostramos los datos
print(df)
str(df)  #tomamos la estructura del df
head(df) #preview df

#quitamos la columna  X de nombres
sdf <- subset(df, select = c(Velocidad.Entrega, Precio, Durabilidad, Imagen.Producto, Valor.Educativo, Servicio.Retorno, Tamano.Paquete, Calidad.Producto, Numero.Estrellas))
#df$X <- NULL 
#df[2] <- NULL    
#df[[2]] <- NULL  
# Extract Specific columns.
result <- data.frame(df$Velocidad.Entrega,df$Precio)
print(result)

nrow(df)    # numero de filas
ncol(df)  #numero de columnas

#calcule la Media y la Desviación Estándar para todas las variables
result.mean <- colMeans(sdf)
sapply(sdf, mean)
print(summary(df)) 

var(df)

var(df$Velocidad.Entrega)
 
sd(df$Velocidad.Entrega)
#4.- Calcule la Matriz de Correlaciones e interprete un par de resultados.
cor(sdf,  method="pearson")

#Se interpreta que las variables Durabilidad vs Precio no estan correlacionadas por que 
#su coeficiente de correlacion no se aproxima a -1 y 1.

#Se interpreta que las variables Numero.Estrellas vs Valor.Educativo estan correlacionadas por que 
#su coeficiente de correlacion se aproxima a -1 y 1 eso significa que existe dependencia lineal entre las variables Y =AX + b.


#5.- Grafique un histograma para dos de las variables e interprete
#El gráfico de las variables Velocidad.Entrega, Precio, Durabilidad e Imagen.Producto  indicando
#si siguen una distribución normal sesgada a la derechao a la izquierda.

library('ggplot2')
#data.file <- file.path('data', '01_heights_weights_genders.csv')
#heights.weights <- read.csv(data.file, header = TRUE, sep = ',')

ggplot(df, aes(x = Precio)) + geom_histogram(binwidth = 1)

ggplot(df, aes(x = Precio)) + geom_histogram(binwidth = 0.1)

ggplot(df, aes(x = Calidad.Producto)) + geom_histogram(binwidth = 0.1)


#density plots
ggplot(df, aes(x = Precio)) + geom_density()

ggplot(df, aes(x = Precio, fill = X)) + geom_density()

#6.- Para cada una de las variables identifique los datos atípicos, luego márquelos en el archivo Excel
#diagrama de cajas
qplot(data = df, x = Calidad.Producto, y = Precio) + 
  scale_color_brewer(palette = 'Accent')



#7.-Cargue el paquete Rattle como se muestra a continuación
library(rattle)
#rattle()   #abre programa ejecutable

#Lea en Rattle el archivo generado por usted “EjemploAlgoritmosRecomendación.csv”
df <- read.csv2("EjemploAlgoritmosRecomendacion.csv")

#Calcule para todas las variables los siguientes índices: Min, 1st Qu., Median, Mean 
#3rd Qu., Max.
print(summary(df))

#8.-Con Rattle grafique la distribución de probabilidad (Histograma) de las
#variables Velocidad.Entrega, Precio, Durabilidad e
#Imagen.Producto. y dé una interpretación del tipo de distribución.



#Con Rattle calcule la matriz de correlaciones.
cor(sdf,  method="pearson")

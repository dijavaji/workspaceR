# Document   : Tarea 3 analisis de componentes principales (ACP) hacer recomendaciones de compras
# Created on : 15-10-2017
# Author     : dvasquez

rm(list = ls())
getwd()  # get current working directory

setwd('/home/thc/Documents/Diego/mineriaDatos/material/acp/')  # seteamos directorio

#cambiar el directorio del archivo
data.file <- file.path('/home/thc/Documents/Diego/mineriaDatos/material/acp', 'EjemploAlgoritmosRecomendacion.csv')
data.file
df <- read.csv2(data.file, header = TRUE, sep = ';', dec = ',' )
#df <- read.csv(data.file, header = TRUE, sep = ';')
head(df,3)
str(df)


df <- df[,-which(sapply(df, class) == "factor")]

#df[, sapply(df, class) != "factor"]    #forma elimina
#df[,sapply(df, is.numeric)]  #forma elimina

#na.omit(crs$df[crs$sample, crs$numeric])
#x = na.omit(crs$dataset[crs$sample, crs$numeric])

#Analisis componentes principales con factorminer utilizamos los parametros
#ncp: numero de dimenciones 5
#scale.unit: escala las variable por defecto true
#graph: true el grafico se muestra
library(FactoMineR)

res.pca = PCA(df, scale.unit=TRUE, ncp=5, graph=T) 

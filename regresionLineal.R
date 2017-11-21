# Document   : Regresion Lineal simple
# Created on : 13-10-2017
# Author     : dvasquez

rm(list = ls())
getwd()  # get current working directory

setwd('/home/thc/Downloads/')  # seteamos directorio

#cambiar el directorio del archivo
data.file <- file.path('/home/thc/Downloads', 'Actuals.csv')
data.file
#df <- read.csv2("Actuals.csv")
df <- read.csv(data.file, header = TRUE, sep = ',')

df.anio <- c(1:32)
df.anio
str(df.anio)

#df.subset <- subset(df, select = c())
print(df[,5:40][])
df.venta.anio <- data.frame(df.anio,df[24,5:40][], row.names = NULL)

A <- df[24,5:40][]

final_df <- as.data.frame(t(A))
final_df$anio <- c(1:36)

colnames(final_df) <- c("ventas", "anio")

relation <- lm(final_df$ventas ~ final_df$anio)

print(summary(relation))

library(ggplot2)

#regresion lineal simple
ggplot(data.frame(anio,ventas), aes(x=anio,y=ventas)) + geom_point() + geom_smooth(method="lm") + ylim(0, 300) + xlim(0, 20)

Urate<-ts(final_df$ventas , start=c(2015,1), freq=12)
print(Urate)

#regression on time dimension
Urate.reg<-lm(Urate~time(Urate))
summary(Urate.reg)
acf(resid(Urate.reg))
pacf(resid(Urate.reg))


#max(final_df$ventas)
# df.venta.anio <- data.frame(rn=rownames(A),df[24,5:40][], row.names = NULL)
# print(df.venta.anio)
# 
# rm(df.venta.anio)
# 
# df[,5:40][]

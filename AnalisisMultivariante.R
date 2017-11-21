#Analisis multivariante
#Objetivo es de utilizar dos técnicas de análisis multivariante y encontrar hallazgos-resultados, 
#realizar todo el proceso y aplicar las técnicas correspondientes consideradas.

#Identificacion de valores atipicos multivariados
#Importacion de archivos de exel
#install.packages("RODBC", dependencies=TRUE); #error al importar con esta libreria archivo de exel necesita plugin de microsoft
#library('RODBC')

#install.packages("gdata")
library('gdata')

require(gdata)
conn = read.xls("/home/thc/Documents/Diego/AnalisisDatos/prueba analisis datos/companias.xls",sheet = 1, header = TRUE)

boxplot(conn$Ventas...M., main="Valores atipicos de las ventas", xlab="algo", ylab="ventas")

identify(rep(1,length(conn$Ventas...M.)),conn$Ventas...M.,rownames(conn))
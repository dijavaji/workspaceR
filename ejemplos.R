height <- c(176, 154, 138, 196, 132, 176, 181, 169, 150, 175)
bodymass <- c(82, 49, 53, 112, 47, 69, 77, 71, 62, 78)
height
bodymass
plot(bodymass, height)
plot(bodymass, height, pch = 16, cex = 1.3, col = "blue", main = "HEIGHT PLOTTED AGAINST BODY MASS", xlab = "BODY MASS (kg)", ylab = "HEIGHT (cm)")
lm(height ~ bodymass)


fit1 <- lm(height ~ bodymass )
sd2 <- sd(fit1$residuals)
plot(bodymass, height)
plot(bodymass, height, pch = 16, cex = 1.3, col = "blue", main = "HEIGHT PLOTTED AGAINST BODY MASS", xlab = "BODY MASS (kg)", ylab = "HEIGHT (cm)")
lm(height ~ bodymass)

#install.packages("ggplot2")
library(ggplot2)


ggplot(data.frame(height,bodymass), aes(x=height,y=bodymass)) + geom_point() + geom_smooth(method="lm")

help("ggplot2")

#Metodos de calculo para el pronostico de ventas
anio <- c(1,2,3,4,5)
ventas <- c(220,245,250,258,273.5)
#y=a+bx
lm(ventas ~ anio)
#y=213.3 +  12.0x
ggplot(data.frame(ventas,anio), aes(x=ventas,y=anio)) + geom_point() + geom_smooth(method="lm")

ggplot(data.frame(anio,ventas), aes(x=anio,y=ventas)) + geom_point() + geom_smooth(method="lm")

str(anio)
getwd()  # get current working directory

setwd('Users/christian/Downloads/')  # set working directory 

#data.file <- file.path('C:\Users\christian\Downloads', 'companias.csv')

################################################
#Analisis componentes principales
#install.packages("HSAUR2")
library("HSAUR2")
#sacamos datos del paquete hsaur2
data("heptathlon", package = "HSAUR2")

#recodificamos las pruebas a las 3 carrera hurdles, run200m, 
heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m
head(heptathlon, 3)

#diagrama de dispersion de la matriz de resultados
#podemos observar que existe una relacion positiva entre cada par de pruebas con la exepcion de el lanzamiento de javalina
score <- which(colnames(heptathlon) == "score")
plot(heptathlon[,-score])

#matriz de correlaciones, comprobamos los resultados anteriores de la grafica
round(cor(heptathlon[,-score]), 2)
#verificamos que existe un valor outlier en todas las pruebas excepto en la de la javalina el cual es la participante PNG que tiene
#peores marcas excepto de en la prueba de jabalina
#puede ser interesante excluir este participante en un nuevo estudio del diagrama de dispersion de la matriz y de la matriz de correlaciones
#para ver si los datos son senciblemente distinto

heptathlon <- heptathlon[-grep("PNG",rownames(heptathlon)),]
print(heptathlon)
plot(heptathlon[,-score])
round(cor(heptathlon[,-score]), 2)
#efectivamente las correlaciones cambian sustancialmente y en el diagrama de dispersion de la matriz no se observan ningunos valores extremos
#a partir de aqui trabajaremos excluyendo al participante de PNG
#al estar los resultados en diferentes escalas (metros, segundos) vamos a realizar el analisis a partir de la matriz de correlaciones 
#es decir vamos a realizar un ACP normalizado, para esto utilizaremos la funcion prcomp que lleva TRUE en el argumento scale para
#asegurarnos que el analisis se realiza a travez de la matriz de correlaciones
heptathlon_pca <- prcomp(heptathlon[, -score], scale. = TRUE)
heptathlon_pca
#sacamos un resumen del analisis para observar mas detalles
summary(heptathlon_pca)
#los pesos de la primera componente principal son
a1 <- heptathlon_pca$rotation[,1]
a1

#graficamos un diagrama de barras de la varianza explicada por las componentes principales es decir diagrama de barras de los autovalores
plot(heptathlon_pca)
#las dos primeras componentes representan el 75% de la varianza total por lo que podemos realizar una representacion biplot utilizando
#estos 2 ejes principales
#graficamos los 24 atletas sobre los 2 primeros ejes principales
biplot(heptathlon_pca, col=c("red","blue") )

#analisis componentes principales con factorminer
library(FactoMineR)
#data(decathlon)
#res.pca = PCA(decathlon[,1:10], scale.unit=TRUE, ncp=5, graph=T)
res.pca = PCA(heptathlon, scale.unit=TRUE, ncp=5, graph=T) 


#Analisis componentes principales
data(iris)
head(iris, 3)
iris
install.packages("factoextra")

library(factoextra)

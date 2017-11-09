#instalar y cargar ggplot package
install.packages('ggplot2')
library(ggplot2)

#instalar y cargar paquete de color
install.packages('RcolorBrewer')
library(RColorBrewer)

#cargamos los datos de diamantes
data(diamonds)

#creamos grafico de precio vs corte color 

qplot(data = diamonds, x = carat, y = price, color =cut) + 
  scale_color_brewer(palette = 'Accent')

#prueba de carga de datos iris
View(iris)

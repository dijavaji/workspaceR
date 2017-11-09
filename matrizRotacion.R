# Ejercicio sacar la Matriz de rotacion dada la matriz X

X <- matrix(c(10,23,7,19,35,1.8,3.2,1.4,2.6,4.4), nrow = 5,byrow = FALSE);

print(X);

#accedo a la columna 1
print(X[,1]);

#accedo a la columna 2
print(X[,2]);

# vector de promedio o media
vm <- c(mean(X[,1]),mean(X[,2]));

#funcion para sacar las medias de una matriz
medias <-colMeans(X);

print(vm);

#matriz centrada X'
Xc <- X - rep(vm, rep.int(nrow(X), ncol(X)));


#matriz de varianza covarianza
#covarianza 

n <- nrow(X); #numero de sujetos
print(n);

Covx <- (n-1)^-1*t(Xc)%*%Xc;  #utilizando formula

Varx <- var(X); #utilizando funcion R

print(Covx);

print(Varx);

remove(V);

#Definimos un nombre a las columnas y las filas
rownombres = c("row1", "row2", "row3", "row4","row5");
colnombres = c("col1", "col2");
P <- matrix(Xc, nrow = 5,byrow = FALSE, dimnames = list(rownombres, colnombres));


#graficar la matriz centrada
#diagrama de dispersion matricial

# damos al archivo grafico un nombre.
png(file = "scatterplot_X.png");

# Plot the matrices between 4 variables giving 12 plots.

# One variable with 3 others and total 4 variables.

pairs(~col1+col2,data = P,main = "Diagrama de dispersion");

# Save the file.
dev.off();

#diagrama de dispersion con ggplot2
install.packages('ggplot')
library(ggplot)

qplot(col1,col2,data = P,main = "Diagrama de dispersion");

im <- qplot(P, aes(col1, col2))
im + geom_point()


#valores propios y vectores propios

ev <- eigen(Covx);

#eigen valores
(values <- ev$values)
#eigen vectores 
(vectors <- ev$vectors)





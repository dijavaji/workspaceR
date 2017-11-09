#Matriz de distancia mahalanobis

X <- matrix(c(1.68,1.7,1.75,1.67,82,75,89,80,820,1310,950,800), nrow = 4,byrow = FALSE);

print(X)

#D^2 = (x – μ)’ Σ^-1 (x – μ)
#calculamos la matriz de medias
mean<-colMeans(X);

#calculo la matriz de covarianzas

Sx<-cov(X)

print(Sx)

#calculo la matriz de distancias

D2<-mahalanobis(X,mean,Sx)

print(D2)





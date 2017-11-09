#maching learnig for hackers
#2 data exploration
#creacion de funciones
#funcion media
my.mean <- function(x) {
  return(sum(x) / length(x))
}

#funcion mediana
my.median <- function(x) {
  sorted.x <- sort(x)
  
  if (length(x) %% 2 == 0)
  {
    indices <- c(length(x) / 2, length(x) / 2 + 1)
    return(mean(sorted.x[indices]))
  }
  else
  {
    index <- ceiling(length(x) / 2)
    return(sorted.x[index])
  }
}

my.vector <- c(0, 100)
my.vector
mean(my.vector)

v <- LETTERS[1:4]
for ( i in v) {
  print(i)
}

for (i in sdf) {
  #c(min(sdf[i]),max(sdf[i]))  
}

min(sdf[2])
max(sdf[2])
#rango
c(min(sdf[1]),max(sdf[1]))  
range(sdf[1])

velocidad.entrega <- with(df, Velocidad.Entrega)
#cuartiles 
quantile(velocidad.entrega)

c(quantile(velocidad.entrega, probs = 0.25), quantile(velocidad.entrega, probs = 0.75))

help("quantile")

#funcion varianza
my.var <- function(x) {
  m <- mean(x)
  return(sum((x - m) ^ 2) / (length(x) - 1))
}

my.varianza(velocidad.entrega) - var(velocidad.entrega)

c(mean(velocidad.entrega) - var(velocidad.entrega), mean(velocidad.entrega) + var(velocidad.entrega))
my.sd <- function(x) {
  return(sqrt(my.var(x)))
}

my.sd(velocidad.entrega) - sd(velocidad.entrega)

c(my.sd(velocidad.entrega) - sd(velocidad.entrega), my.sd(velocidad.entrega) + sd(velocidad.entrega))

range(velocidad.entrega)

c(quantile(velocidad.entrega, probs = 0.25), quantile(velocidad.entrega, probs = 0.75))
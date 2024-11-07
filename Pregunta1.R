#Sera un modelo --> en este caso: normal (media, desv. tipica)
media <- 95.3
desv <- 5.7
# dibujamos la curva 
curve(dnorm(x, mean=media, sd=desv), xlim=c(80,120)) #--> #funcióm de densidad (normal)

#P(X<90)=F(90):
rnorm(1,media, desv)
pnorm(90,media, desv)

#set.seed(123) --> con esto nos daria a todos el mismo numero 
#muestra aleatoria de tamaño 4, coge 4 muestras random, cada vez q lo repita me dara num diferentes
rnorm(4,media, desv)

#a) n=4 valor esperado de la suma de les resist de la muestra? --> E(sumatorio(xi))
  #suma de les resist de la muestra: Y=sumatorio(xi)
sum(rnorm(4,media, desv)) #experimento aleatorio, ira cambiando
Y <- function(i)(sum(rnorm(4,media, desv)))
Y10000<-sapply(1:10000, Y) #lo repite 10.000 veces 
hist(Y10000) # se empieza a parecer a la funcion de densidad normal de la suma de las resistencias 
mean(Y10000) #el valor esperado de Y 

#en teoria la media de la suma muestral de tamaño n =4
4*media

#varianza de la suma muestral en dos formas 
4*desv^2 #teorica
var(Y10000)#practica

###
hist(Y10000, freq=FALSE)
curve(dnorm(x, mean=4*media, sd=sqrt(4)*desv), add=TRUE)

#b) lo mismo pero cambiando el 4 por un 100 y haciendo la varianza 

#c)
1-pnorm(103, media, desv)

#otra forma 
Y <- function(i)(sum(rnorm(1,media, desv))) #la suma de las resistencias 
Y10000<-sapply(1:10000, Y)
mean(Y10000>103)



#d) 
Xbar <- function(i)(mean(rnorm(4,media, desv))) #la media de las resistencias 
Xbar100000<-sapply(1:100000, Xbar)
hist(Xbar100000)
mean(Xbar100000<98)

#otra forma pero hay q saberselo de memoria 
mean(Xbar100000<98)
pnorm(98, media, desv/sqrt(4))

#e)
Ssq <- function(i)(var(rnorm(100,media, desv)))
Ssq100000<- sapply(1:100000, Ssq)
hist(Ssq100000)
mean(Ssq100000>32)

#de forma teorica 
1-pchisq((100-1)*32/desv^2, 100-1)
hist(Ssq100000*(100-1)/desv^2, prob=TRUE)
curve(dchisq(x, 100-1), add=TRUE, col='red')
(100-1)*32/desv^2

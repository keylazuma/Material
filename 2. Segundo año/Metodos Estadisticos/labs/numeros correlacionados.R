###install.packages("MultiRNG")
library(MultiRNG)
library(car)

###Se genera la matriz de correlaciones mat.cor
###Y el vector de medios vector.medias
###numeros es la matriz de variables aleatorias
###y mom.0 y mom.1 las dos variables por separado

mat.cor <- as.matrix(rbind(c(1,0.75),c(0.75,1)))
vector.medias <- c(2,3)
numeros <- draw.d.variate.normal(no.row=20,d=2,mean.vec=vector.medias,cov.mat=mat.cor)

mom.0 <- numeros[,1]
mom.1 <- numeros[,2]


###Noten la normalidad y la linealidad
##Cuanto mayor sea la correlaci???n, la linealidad es m???s clara.
hist(mom.0)
qqPlot(mom.0)
hist(mom.1)
qqPlot(mom.1)
scatterplot(mom.0,mom.1)
plot(mom.0,mom.1)

cor(mom.0,mom.1)

###Noten la variacion en el valor de la correlaci???n
###Si van corriendo el c???digo una y otra vez.

###El siguiente ciclo es que, en promedio, se obtiene la correlaci???n deseada

mat.cor <- as.matrix(rbind(c(1,0.75),c(0.75,1)))
vector.medias <- c(2,3)

almacen.cor <- rep(NA,10000)

for (i in 1:10000) {
  
  numeros <- draw.d.variate.normal(no.row=20,d=2,mean.vec=vector.medias,cov.mat=mat.cor)
  
  mom.0 <- numeros[,1]
  mom.1 <- numeros[,2]
  
  almacen.cor[i] <- cor(mom.0,mom.1)
  
  
}

mean(almacen.cor)
median(almacen.cor)
hist(almacen.cor)

###Ahora, qu??? pasa si se usa una distribuci???n uniforme

mat.cor <- as.matrix(rbind(c(1,0.75),c(0.75,1)))


almacen.cor.pearson <- rep(NA,10000)
almacen.cor.spearman <- rep(NA,10000)
almacen.media.mom.0 <- rep(NA,10000)
almacen.media.mom.1 <-rep(NA,10000)


for (i in 1:10000) {
  
  numeros <- draw.d.variate.uniform(no.row=20,d=2,cov.mat=mat.cor)
  
  mom.0 <- numeros[,1]
  mom.1 <- numeros[,2]
  
  
  almacen.cor.pearson[i] <- cor(mom.0,mom.1,method="pearson")
  almacen.cor.spearman[i] <- cor(mom.0,mom.1, method="spearman")
  
  almacen.media.mom.0[i] <- mean(mom.0)
  almacen.media.mom.1[i] <- mean(mom.1)
  
}

mean(almacen.cor.pearson)
median(almacen.cor.spearman)
hist(almacen.cor.pearson)
hist(almacen.cor.spearman)

mean(almacen.media.mom.0)
mean(almacen.media.mom.1)





###Ahora, qu??? pasa si se usa una distribuci???n Wishart

mat.cor <- as.matrix(rbind(c(1,0.75),c(0.75,1)))
vector.medias <- c(2,3)
numeros <- draw.wishart(no.row=20,d=2,nu=3,sigma=mat.cor)

mom.0 <- numeros[,1]
mom.1 <- numeros[,2]
mean(mom.0)
mean(mom.1)

##Procedimiento para que tengan aproximadamente la misma media
media.igual <- 5
mom.0 <- 5+mom.0/mat.cor[1,1]
mom.1 <- 5+mom.1/mat.cor[1,2]
mean(mom.0)
mean(mom.1)

###Noten la normalidad y la linealidad
##Cuanto mayor sea la correlaci???n, la linealidad es m???s clara.
hist(mom.0)
qqPlot(mom.0)
hist(mom.1)
qqPlot(mom.1)
scatterplot(mom.0,mom.1)
plot(mom.0,mom.1)

cor(mom.0,mom.1)
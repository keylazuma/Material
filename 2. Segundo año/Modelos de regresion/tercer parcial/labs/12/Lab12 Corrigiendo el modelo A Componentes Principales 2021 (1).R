setwd("C:/Users/Gilbert BC/Documents/Teletrabajo/regresion 2022/Labs/Lab 9/")



load("gastoocio3.Rdata")
attach(gastoocio3)
names(gastoocio3)
library(car)
library(lmtest)
library(tseries)
library(e1071)
library(dglm)


library(MASS)
##library(faraway)

###Para ejemplificar el uso del Análisis de Componentes Principales, retomaré el ejemplo del laboratorio
###sobre multicolinealidad, agregando una variable más.


round(cor(gastoocio3[,5:18]),4)


###Primero vamos a ilustrar el efecto de la multicolinealidad

round(cor(gastoocio3[,c(5,6,10,11,18)]),4)

mod.grande.multi=lm(gasto_ocio~tamhogar+miembrosocup+miembrospercep+urbano)
summary(mod.grande.multi)

vif(mod.grande.multi)
mod.tamhogar <- lm(gasto_ocio~tamhogar); summary(mod.tamhogar)
###Note que los VIFS más altos son para tamhogar, miembrosocup y miembrospercep
###Note además que ninguna de los coeficientes es significativamente distinto de cero.

###con prcomp

nuevos=prcomp(gastoocio3[,c(6,10,11,18)],center=TRUE,scale=TRUE)
round(cor(nuevos$x),4)
porc.var=(nuevos$sdev**2)/sum(nuevos$sdev**2)
porc.var
acum=porc.var
acum

k=length(porc.var)

for (i in 2:k) {
  acum[i]=acum[i-1]+porc.var[i]
}

cbind((1:k),porc.var,acum)


###Noten que los dos primeros componentes explican el 81.8% de la variabilidad del gasto en ocio

nuevos$rotation

###Noten que el primer componente carga en tamhogar, miembrosocup y miembrospercep, mientras que el segundo componente
###se refiere principalmente a la variable urbano.
###Dado que sabemos que urbano no genera poblemas de multicolinealidad, tomamos el primer componente.



###con princomp

###Al escribir scores=TRUE, le estamos pidiendo a R que grabe los componentes en un objeto.

nuevos2=princomp(gastoocio3[,c(6,10,11,18)],scores=TRUE, cor=TRUE)
nuevos2$loadings

###La regresión se especifica con las cargas del primer componente.


(comp1=nuevos2$scores[,1])


modelo.pca.1=lm(gasto_ocio~comp1+urbano)
vif(modelo.pca.1)
cor(comp1,urbano)

summary(modelo.pca.1)

###El modelo se especificaría de la siguiente forma:

###gastoocio3_techo=    

####           31366
###           +12769*    comp1
###           +16716*(urbano)


###gastoocio3_techo=    

####           31366
###           +12769*    {0.515*[tamhogar-mean(tamhogar)]/sd(tamhogar) 
###                       +0.604*[miembrosocup-mean(miembrosocup)]/sd(miembrosocup) 
###                       +0.607*[miembrospercep-mean(miembrospercep)]/sd(miembrospercep)  }
###           +16716*(urbano)



###gastoocio3_techo=    

####           31366
###           +6576.0*[tamhogar-mean(tamhogar)]/sd(tamhogar) 
###           +7712.5*[miembrosocup-mean(miembrosocup)]/sd(miembrosocup) 
###           +7750.8*[miembrospercep-mean(miembrospercep)]/sd(miembrospercep)  
###           +16716*(urbano)



comp2=nuevos2$scores[,2]
comp3=nuevos2$scores[,3]

modelo.pca.2=lm(gasto_ocio~comp1+comp2+comp3)

summary(modelo.pca.2)




###gastoocio3_techo=    

####           42232
###           +13061*    {0.515*[tamhogar-mean(tamhogar)]/sd(tamhogar) 
###                       +0.604*[miembrosocup-mean(miembrosocup)]/sd(miembrosocup) 
###                       +0.607*[miembrospercep-mean(miembrospercep)]/sd(miembrospercep)  }
###
###           -7990* {-0.998*[urbano-mean(urbano)]/sd(urbano)}
###
###           -2152*    {0.856*[tamhogar-mean(tamhogar)]/sd(tamhogar) 
###                       -0.380*[miembrosocup-mean(miembrosocup)]/sd(miembrosocup) 
###                       -0.349*[miembrospercep-mean(miembrospercep)]/sd(miembrospercep)  }


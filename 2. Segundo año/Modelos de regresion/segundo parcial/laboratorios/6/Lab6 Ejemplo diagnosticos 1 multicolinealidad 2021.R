setwd("C:/Users/Gilbert BC/Documents/Teletrabajo/regresion 2022/Labs/Lab 6/")

###gastoocio3 <- read_dta("gastoocio3.dta")

###gastoocio3=gastoocio3[,-6]

###save(gastoocio3,file="gastoocio3.Rdata")




###load("gastoocio3.Rdata")
attach(gastoocio3)
names(gastoocio3)

library(car)
library(lmtest)
library(tseries)
library(e1071)


##library(MASS)
##library(faraway)


###Se quiere plantear un modelo en el que el gasto en ocio sea función 
###del tamaño del hogar, el sexo del jefe (1=hombre),
###edad y escolaridad del jefe, miembros ocupados y perceptores, ingresototal del hogar
###Proporción del gasto que se destina a alimentación, si la vivienda que se habita
### es alquilada o todavía se debe, el número de vehículos, la población de menores de 
###15 años y si el hogar está en zona urbana
###Nótese el tamaño de muestra y las correlaciones entre las variables

dim(gastoocio3)

head(gastoocio3)



round(cor(gastoocio3[,5:18]),3)


###Primero vamos a ilustrar el efecto de la multicolinealidad

round(cor(gastoocio3[,c(5,6,10,11)]),3)

mod.tamhogar=lm(gasto_ocio~tamhogar)
summary(mod.tamhogar)


mod.miembrosocup=lm(gasto_ocio~miembrosocup)
summary(mod.miembrosocup)


mod.miembrospercep=lm(gasto_ocio~miembrospercep)
summary(mod.miembrospercep)

###Noten que en los modelos de regresión lineal simple, las pendientes son
###significativas al 5%, pero en el modelo de regresión múltiple, ninguna lo es.

mod.grande=lm(gasto_ocio~tamhogar+miembrosocup+miembrospercep)
summary(mod.grande)

###Calculos de VIF

vif(mod.grande)


###El procedimiento de stepwise es una manera rápida de resolver problemas de multicolinealidad
mod.reducido=step(mod.grande)


###Calculo de VIFs

reg.tamhogar=lm(tamhogar~miembrosocup+miembrospercep)

reg.miembrosocup=lm(miembrosocup~tamhogar+miembrospercep)

reg.miembrospercep=lm(miembrospercep~tamhogar+miembrosocup)

summary(reg.tamhogar)$r.squared
summary(reg.miembrosocup)$r.squared
summary(reg.miembrospercep)$r.squared

1/(1-summary(reg.tamhogar)$r.squared)
1/(1-summary(reg.miembrosocup)$r.squared)
1/(1-summary(reg.miembrospercep)$r.squared)

vif(mod.grande)

###Ahora, explorar con gráficas de regresión parcial

###Los gráficos de regresión parcial no son para diagnóstico de multicolinealidad,
###sino para saber cuáles variables descartar.  Generalmente, se descartan
###las que tengan una línea menos inclinada

reg.sin.tamhogar=lm(gasto_ocio~miembrosocup+miembrospercep)

reg.sin.miembrosocup=lm(gasto_ocio~tamhogar+miembrospercep)

reg.sin.miembrospercep=lm(gasto_ocio~tamhogar+miembrosocup)


par(mfrow=c(2,2))

plot(residuals(reg.tamhogar),residuals(reg.sin.tamhogar))
abline(lm(residuals(reg.sin.tamhogar)~residuals(reg.tamhogar)))

plot(residuals(reg.miembrosocup),residuals(reg.sin.miembrosocup))
abline(lm(residuals(reg.sin.miembrosocup)~residuals(reg.miembrosocup)))

plot(residuals(reg.miembrospercep),residuals(reg.sin.miembrospercep))
abline(lm(residuals(reg.sin.miembrospercep)~residuals(reg.miembrospercep)))

avPlots(mod.grande)

avPlots(reg.sin.miembrosocup)

mod.khersttyn=lm(gasto_ocio~miembrosocup+miembrospercep)
summary(mod.khersttyn)






###Creación de índices o variables compuestas
###para corregir multicolinealidad

###Es otra forma de arreglar el problema de multicolinealidad.

porcocup=miembrosocup/tamhogar*100

cor(cbind(gasto_ocio,miembrospercep,tamhogar,porcocup))

summary(cbind(gasto_ocio,miembrospercep,tamhogar,porcocup))

mod.alter=lm(gasto_ocio~porcocup+miembrospercep)
summary(mod.alter)

vif(mod.alter)

###También se puede ver qué pasa si hay un procedimiento de selección de variables
###pues el stepwise es una solución a la multicolinealidad.

mod.red1=step(mod.grande)
summary(mod.red1)
summary(mod.alter)
summary(mod.grande)


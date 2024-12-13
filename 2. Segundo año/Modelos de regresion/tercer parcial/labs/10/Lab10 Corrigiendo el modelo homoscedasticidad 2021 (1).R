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


mod.grande=lm(gasto_ocio~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores)
summary(mod.grande)

###Primer modelo con selección de variables

###Cálculo de residuos

estandariz=rstandard(mod.grande)
estudentiz=rstudent(mod.grande)
residuos=residuals(mod.grande)



mod.reducido=step(mod.grande)
summary(mod.reducido)

estandariz.peq=rstandard(mod.reducido)
estudentiz.peq=rstudent(mod.reducido)
residuos.peq=residuals(mod.reducido)


####Heteroscedasticidad
par(mfrow=c(1,2))
plot(mod.grande$fitted,estandariz)
plot(mod.reducido$fitted,estandariz.peq)

par(mfrow=c(1,2))
plot(mod.grande$fitted,estandariz^2, ylim=c(0,9))
plot(mod.reducido$fitted,estandariz.peq^2,ylim=c(0,9))

###Prueba de Breusch-Pagan

bptest(mod.grande)
bptest(mod.reducido)


ncvTest(mod.grande)
ncvTest(mod.reducido)

###Transformación  ln###
lnpropalim <- log(propalim+1)
lngastoocio=log(gasto_ocio)


mod.grande3=lm(lngastoocio~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+lnpropalim+alquilada+prestamcasa+numvehic+pobmenores)
summary(mod.grande3)
round(summary(mod.grande3)$coefficients,4)
mod.reducido3=step(mod.grande3)
summary(mod.reducido3)
round(summary(mod.reducido3)$coefficients,4)


###Transformación Box-Cox


gasto.lambda1=I(gasto_ocio^(0.1))


mod.grande5=lm(gasto.lambda1~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores)
summary(mod.grande5)

mod.reducido5=step(mod.grande5)
summary(mod.reducido5)


###Transformación para controlar heteroscedasiticidad

spreadLevelPlot(mod.grande)

##price.het=I(price^(-1/3))

gasto.lambda2=I(gasto_ocio^(1/8))


mod.grande6=lm(gasto.lambda2~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores)
summary(mod.grande6)

mod.reducido6=step(mod.grande6)
summary(mod.reducido6)


tabla.het <- matrix(rep(NA,8),nrow=4,ncol=2)

colnames(tabla.het) <- c("Grande","Reducido")
rownames(tabla.het) <- c("Original","log","raiz.10","raiz.8")

tabla.het[1,1] <- ncvTest(mod.grande)$p
tabla.het[1,2] <- ncvTest(mod.reducido)$p

tabla.het[2,1] <- ncvTest(mod.grande3)$p
tabla.het[2,2] <- ncvTest(mod.reducido3)$p

tabla.het[3,1] <- ncvTest(mod.grande5)$p
tabla.het[3,2] <- ncvTest(mod.reducido5)$p

tabla.het[4,1] <- ncvTest(mod.grande6)$p
tabla.het[4,2] <- ncvTest(mod.reducido6)$p


round(tabla.het,3)



####Otra forma de resolver el problema de heteroscedasticidad: 
####Mínimos Cuadrados Ponderados



###Análisis de homoscedasticidad por predictor

par(mfrow=c(2,2))
plot(estandariz~tamhogar)
abline(lm(estandariz~tamhogar))
boxplot(estandariz~sexojefe)
#abline(lm(estandariz~sexojefe))
plot(estandariz~edadjefe)
abline(lm(estandariz~edadjefe))
plot(estandariz~escoljefe)
abline(lm(estandariz~escoljefe))

par(mfrow=c(2,2))
plot(estandariz~miembrosocup)
abline(lm(estandariz~miembrosocup))
plot(estandariz~miembrospercep)
abline(lm(estandariz~miembrospercep))
plot(estandariz~ingresototal)
abline(lm(estandariz~ingresototal))
plot(estandariz~propalim)
abline(lm(estandariz~propalim))

par(mfrow=c(1,2))
plot(estandariz~numvehic)
abline(lm(estandariz~numvehic))
plot(estandariz~pobmenores)
abline(lm(estandariz~numvehic))

par(mfrow=c(1,1))
scatterplot(estandariz~mod.grande$fitted.values)
scatterplot(rstandard(mod.grande6)~mod.grande6$fitted.values)

###Evidentemente la transformación ayuda.

###Pero si no queremos la transformación,
###se puede estimar un modelo con mínimos cuadrados ponderados

abs.res1=abs(residuals(mod.grande))
mod.ponde1=lm(abs.res1~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores)
ponde1=1/abs(fitted(mod.ponde1))
###Hasta aquí lo que se hizo es crear el ponderador
###Como el inverso del valor predicho
###El siguiente modelo ya es ponderado


mod.grande7=lm(gasto_ocio~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup
               +miembrospercep+ingresototal+propalim+alquilada+prestamcasa
               +numvehic+pobmenores, weights=I(ponde1))
summary(mod.grande7)
summary(mod.grande)

ncvTest(mod.grande)
ncvTest(mod.grande7)

round(cbind(mod.grande$coef,mod.grande7$coef),4)
###Se comparan los coeficientes para analizar si cambian mucho o no.
###Noten los cambios en tamhogar, escoljefe, alquilada, prestamcasa y pobmenores

###2 iteración

abs.res2=abs(residuals(mod.grande7))
mod.ponde2=lm(abs.res2~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores)
ponde2=1/abs(fitted(mod.ponde2))

mod.grande8=lm(gasto_ocio~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup
               +miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic
               +pobmenores, weights=I(ponde2))
summary(mod.grande8)
summary(mod.grande)

round(cbind(mod.grande$coef,mod.grande7$coef,mod.grande8$coef),4)

###3 iteración

abs.res3=abs(residuals(mod.grande8))
mod.ponde3=lm(abs.res3~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores)
ponde3=1/abs(fitted(mod.ponde3))

mod.grande9=lm(gasto_ocio~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup
               +miembrospercep+ingresototal+propalim+alquilada+prestamcasa
               +numvehic+pobmenores, weights=I(ponde3))
summary(mod.grande9)
summary(mod.grande)

round(cbind(mod.grande$coef,mod.grande7$coef,mod.grande8$coef,mod.grande9$coef),4)


###4 iteración

abs.res4=abs(residuals(mod.grande9))
mod.ponde4=lm(abs.res4~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores)
ponde4=1/abs(fitted(mod.ponde4))

mod.grande10=lm(gasto_ocio~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores, weights=I(ponde4))
summary(mod.grande10)
summary(mod.grande)

round(cbind(mod.grande$coef,mod.grande9$coef,mod.grande10$coef),4)
##Los coeficientes parecen ser más similares entre iteración 3 y 4.


###El problema es la regla de decisión para terminar.


####Ahora con fitted values como predictores

abs.res11=abs(residuals(mod.grande))
mod.ponde11=lm(abs.res1~mod.grande$fitted.values)
ponde11=1/abs(fitted(mod.ponde11))

mod.grande11=lm(gasto_ocio~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores, weights=I(ponde11))
summary(mod.grande11)
summary(mod.grande)


round(cbind(mod.grande$coef,mod.grande11$coef),4)


###2 iteracion

abs.res12=abs(residuals(mod.grande11))
mod.ponde12=lm(abs.res12~mod.grande11$fitted.values)
ponde12=1/abs(fitted(mod.ponde12))

mod.grande12=lm(gasto_ocio~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores, weights=I(ponde12))
summary(mod.grande12)
summary(mod.grande)

round(cbind(mod.grande$coef,mod.grande11$coef,mod.grande12$coef),4)




###3 iteracion

abs.res13=abs(residuals(mod.grande12))
mod.ponde13=lm(abs.res13~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores)
ponde13=1/abs(fitted(mod.ponde13))

mod.grande13=lm(gasto_ocio~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores, weights=I(ponde13))
summary(mod.grande13)
summary(mod.grande)

round(cbind(mod.grande$coef,mod.grande11$coef,mod.grande12$coef,mod.grande13$coef),4)








###Nos quedamos con el procedimiento ponderado con los predictores.


###Dificultad de MCP: No está claro cuánto corrige.  Se supone que corrigió.  
###Gráficos ponderados nos dan una leve idea.



###Otra forma de resolver los problemas de heteroscedasticidad:
###Modelos heteroscedásticos


mod.heter=dglm(gasto_ocio~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup
               +miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic
               +pobmenores, 
                  dformula~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup
               +miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic
               +pobmenores, family=gaussian)


###El problema es que converja, pues tiene muchos parámetros que estimar

###Por eso se recomienda con los modelos reducidos
summary(mod.reducido)

mod.heter.reducido=dglm(gasto_ocio~edadjefe+miembrospercep+ingresototal, 
                dformula~edadjefe+miembrospercep+ingresototal, family=gaussian)


(sum=summary(mod.heter.reducido))




round(sum$coefficients,2)
round(summary(mod.reducido)$coefficients,2)

AIC(mod.reducido)
AIC(mod.heter.reducido)
(AIC.mod.heter <- 4770.402 +2*8)

###Otra forma de corregir problemas de heteroscedasticidad generados
###por valores extremos.
###Regresión Robusta vía Mínimos Cuadrados Reponderados Iterativamente.

library(MASS)

###Primero con corrección de Huber

mod.huber=rlm(gasto_ocio~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores)
summary(mod.huber, cor=FALSE)
###El tcalculado se compara con el t tabular
qt(0.975,187)

summary(mod.grande)

round(cbind(mod.grande$coef,mod.huber$coef),4)




plot(estandariz,mod.huber$w)
identify(estandariz,mod.huber$w, identif)



shapiro.test(mod.huber$residuals)
jarque.bera.test(mod.huber$residuals)
###No se puede saber si corrige, a partir de pruebas.

ncvTest(mod.huber)

###Pero sí a trav?s de gráficos

plot(mod.huber$fitted.values,mod.huber$residuals,cex=mod.huber$w)

###Note que los valores extremos son representados por puntos que casi no se ven





###Ahora con corrección bicuadrática


###Dos formas de ponderar con el método bicuadrático

###Con el m?todo MM de Tukey

mod.bisq=rlm(gasto_ocio~tamhogar+sexojefe+edadjefe+escoljefe
             +miembrosocup+miembrospercep+ingresototal+propalim
             +alquilada+prestamcasa+numvehic+pobmenores, method='MM')
summary(mod.bisq, cor=FALSE)

plot(estandariz,mod.bisq$w, xlim=c(-4,8),ylim=c(-.5,1.5))
identify(estandariz,mod.bisq$w, identif)

###Procedimiento directo del bicuadratico

mod.bisq2=rlm(gasto_ocio~tamhogar+sexojefe+edadjefe+escoljefe
              +miembrosocup+miembrospercep+ingresototal+propalim
              +alquilada+prestamcasa+numvehic+pobmenores, 
              psi=psi.bisquare)
summary(mod.bisq2, cor=FALSE)

plot(estandariz,mod.bisq2$w, xlim=c(-4,8),ylim=c(-.5,1.5))
identify(estandariz,mod.bisq2$w, identif)

mod.huber.red=rlm(gasto_ocio~edadjefe+miembrospercep+ingresototal)
summary(mod.huber.red, cor=FALSE)

round(cbind(summary(mod.reducido)$coef[,c(1,3)],summary(mod.huber.red)$coef[,c(1,3)],
      summary(mod.heter.reducido)$coef[,c(1,3)]),3)

round(summary(mod.reducido6)$coefficients[,c(1,3)],3)


###Noten la similitud en las conclusiones con el modelo de Huber y el heteroscedástico
###Y las diferencias con el modelo con transformaciones
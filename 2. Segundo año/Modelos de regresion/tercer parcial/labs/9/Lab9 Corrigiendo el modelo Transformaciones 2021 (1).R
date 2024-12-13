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



###Valores influyentes###

###Leverage###
lev.mod.grande=hatvalues(mod.grande)

###DFFIT
dffit.mod.grande=dffits(mod.grande)

###D de Cook
cook.mod.grande=cooks.distance(mod.grande)


###DFBETAs
dfbeta.mod.grande=dfbetas(mod.grande)


summary(cbind(estandariz,lev.mod.grande,dffit.mod.grande,cook.mod.grande))

summary(dfbeta.mod.grande)





###Calculos de VIF

vif(mod.grande)


mod.reducido=step(mod.grande)
summary(mod.reducido)

vif(mod.reducido)

###Cálculo de residuos reducido

estandariz.peq=rstandard(mod.reducido)
estudentiz.peq=rstudent(mod.reducido)
residuos.peq=residuals(mod.reducido)
dffit.mod.peq=dffits(mod.reducido)




###Normalidad de los residuos
###Modelo grande

shapiro.test(residuos)
jarque.bera.test(residuos)
qqPlot(residuos)


###Modelo pequeño
shapiro.test(residuos.peq)
jarque.bera.test(residuos.peq)
qqPlot(residuos.peq)

###O sea, no podemos suponer normalidad




####Heteroscedasticidad
par(mfrow=c(1,2))
plot(mod.grande$fitted,estandariz)
plot(mod.reducido$fitted,estandariz.peq)
par(mfrow=c(1,1))
###Prueba de Breusch-Pagan

bptest(mod.grande)
bptest(mod.reducido)

###Dado que no podemos suponer normalidad, preferimos en este caso la prueba ncvTest

ncvTest(mod.grande)
ncvTest(mod.reducido)

###Valores extremos y más gráficos de residuos

###Para DFFITs

plot(mod.grande$fitted,estandariz, cex=0.2*abs(dffit.mod.grande)/mean(abs(dffit.mod.grande)),xlim=c(-50000,250000),ylim=c(-3,8))
abline(h=0, col=6)
title("Mod.grande: Residuos por valores predichos, ponderados por DFFIT")
text(mod.grande$fitted[estandariz>4],estandariz[estandariz>4],identif[estandariz>4])

plot(mod.reducido$fitted,estandariz.peq, cex=0.2*abs(dffit.mod.peq)/mean(abs(dffit.mod.peq)),xlim=c(-50000,250000),ylim=c(-3,8))
abline(h=0, col=6)
title("Mod.reducido: Residuos por valores predichos, ponderados por DFFIT")
text(mod.reducido$fitted[estandariz.peq>4],estandariz.peq[estandariz.peq>4],identif[estandariz.peq>4])


###Análisis de linealidad

par(mfrow=c(2,2))
plot(gasto_ocio~tamhogar)
abline(lm(gasto_ocio~tamhogar))
plot(gasto_ocio~sexojefe)
abline(lm(gasto_ocio~sexojefe))
plot(gasto_ocio~edadjefe)
abline(lm(gasto_ocio~edadjefe))
plot(gasto_ocio~escoljefe)
abline(lm(gasto_ocio~escoljefe))

par(mfrow=c(2,2))
plot(gasto_ocio~miembrosocup)
abline(lm(gasto_ocio~miembrosocup))
plot(gasto_ocio~miembrospercep)
abline(lm(gasto_ocio~miembrospercep))
plot(gasto_ocio~ingresototal)
abline(lm(gasto_ocio~ingresototal))
plot(gasto_ocio~propalim)
abline(lm(gasto_ocio~propalim))

par(mfrow=c(1,2))
plot(gasto_ocio~numvehic)
abline(lm(gasto_ocio~numvehic))
plot(gasto_ocio~pobmenores)
abline(lm(gasto_ocio~numvehic))




###Plantear transformaciones

###Relaciónn curvilínea con propalim, miembrosocup y miembrospercep

lnpropalim=log(propalim+1)
miembrosocup.cuad=miembrosocup^2
miembrospercep.cuad=miembrospercep^2

par(mfrow=c(2,2))
plot(gasto_ocio~miembrosocup.cuad)
abline(lm(gasto_ocio~miembrosocup.cuad))
plot(gasto_ocio~miembrospercep.cuad)
abline(lm(gasto_ocio~miembrospercep.cuad))
plot(gasto_ocio~ingresototal)
abline(lm(gasto_ocio~ingresototal))
plot(gasto_ocio~lnpropalim)
abline(lm(gasto_ocio~lnpropalim))

###Note que en el gráfico de propalim, ya no hay valores acumulados o agrupados en 
###la esquina inferior izquierda.  O sea, la transformación logarítmica genera resultados.


###Qué pasa si incorporamos esas variables y hacemos un nuevo stepwise

mod.grande2=lm(gasto_ocio~tamhogar+sexojefe+edadjefe+escoljefe
               +miembrosocup+miembrosocup.cuad+miembrospercep+
                 miembrospercep.cuad+ingresototal+lnpropalim+alquilada+
                 prestamcasa+numvehic+pobmenores)
summary(mod.grande2)

mod.reducido2=step(mod.grande2)
summary(mod.reducido2)
summary(mod.reducido)

###No se obtiene un gran cambio con estas transformaciones

###Probar con logaritmo natural en el gasto###
###Y se mantienen la transformación logarítmica en la proporción de alimentos.

lngastoocio=log(gasto_ocio)


mod.grande3=lm(lngastoocio~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup
               +miembrospercep+ingresototal+lnpropalim+alquilada+prestamcasa
               +numvehic+pobmenores)
summary(mod.grande3)

mod.reducido3=step(mod.grande3)
summary(mod.reducido3)

###Noten que el R cuadrado ajustado sube: Se mejora el ajuste

###Por qué se mejora el modelo con la transformación?  Veamos los gráficos de linealidad.

par(mfrow=c(2,2))
plot(lngastoocio~edadjefe)
abline(lm(lngastoocio~edadjefe))
plot(lngastoocio~escoljefe)
abline(lm(lngastoocio~escoljefe))
plot(lngastoocio~miembrospercep)
abline(lm(lngastoocio~miembrospercep))
plot(lngastoocio~lnpropalim)
abline(lm(lngastoocio~lnpropalim))

par(mfrow=c(1,2))
plot(lngastoocio~pobmenores)
abline(lm(lngastoocio~pobmenores))
plot(lngastoocio~ingresototal)
abline(lm(lngastoocio~ingresototal))

###La relación con ingresototal sigue siendo curvilínea.
###Se puede transformar ingresototal

summary(ingresototal)

lning=log(ingresototal+1250000)   #Se suma 1250000 para que todos los valores sean positivos
par(mfrow=c(1,2))
plot(lngastoocio~pobmenores)
abline(lm(lngastoocio~pobmenores))
plot(lngastoocio~lning)
abline(lm(lngastoocio~lning))

###Aparentemente hay un valor extremo que no mejora el ajuste

mod.grande4=lm(lngastoocio~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup
               +miembrospercep+lning+lnpropalim+alquilada
               +prestamcasa+numvehic+pobmenores)
summary(mod.grande4)

mod.reducido4=step(mod.grande4)
summary(mod.reducido4)

crPlots(mod.grande3)

###Los crPlots permiten analizar la linealidad en ls relaciones.  SE observa con el modelo 3


###Sabemos que no hay normalidad.  Se puede probar la transformación boxcox

###Transformacion Box-Cox


boxcox(gasto_ocio~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores)

bcobjeto=boxcox(gasto_ocio~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores)

cbind(bcobjeto$x,bcobjeto$y)

bcobjeto$x[bcobjeto$y==max(bcobjeto$y)]

###El lambda ideal es 0.14, pero se puede usar 0.1


gasto.lambda1=I(gasto_ocio^(0.1))


mod.grande5=lm(gasto.lambda1~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores)
summary(mod.grande5)

mod.reducido5=step(mod.grande5)
summary(mod.reducido5)

###Note la diferencia en las p-values de la prueba de Shapiro para analizar normalidad

tabla.norm <- matrix(rep(NA,6),nrow=3,ncol=2)

colnames(tabla.norm) <- c("Grande","Reducido")
rownames(tabla.norm) <- c("Original","log","raiz.10")

tabla.norm[1,1] <- shapiro.test(mod.grande$residuals)$p.value
tabla.norm[1,2] <- shapiro.test(mod.reducido$residuals)$p.value

tabla.norm[2,1] <- shapiro.test(mod.grande3$residuals)$p.value
tabla.norm[2,2] <- shapiro.test(mod.reducido3$residuals)$p.value

tabla.norm[3,1] <- shapiro.test(mod.grande5$residuals)$p.value
tabla.norm[3,2] <- shapiro.test(mod.reducido5$residuals)$p.value

round(tabla.norm,3)

###La transformación de Box Cox nos permite suponer normalidad

###Transformación para controlar heteroscedasiticidad
par(mfrow=c(1,1))
spreadLevelPlot(mod.grande)



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


##Tanto la transformación para corregir normalidad como la transforamción para corregir
##heteroscedasticidad permiten suponer ahora homoscedasticidad.

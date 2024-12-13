setwd("C:/Users/Gilbert BC/Documents/Teletrabajo/regresion 2022/Labs/Lab 7/")

##gastoocio3 <- read_dta("gastoocio3.dta")
###save(gastoocio3,file="gastoocio3.Rdata")




load("gastoocio3.Rdata")
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


mod.grande1=lm(gasto_ocio~tamhogar+sexojefe+edadjefe+escoljefe+miembrosocup+miembrospercep+ingresototal+propalim+alquilada+prestamcasa+numvehic+pobmenores)
summary(mod.grande1)

###Primer modelo con selección de variables

mod.redux1=step(mod.grande1)
summary(mod.redux1)



###Valores extremos y más gráficos de residuos

###Comparación entre residuos corrientes, residuos estandarizados 
###y residuos estudentizados

estandariz=rstandard(mod.redux1)
estudentiz=rstudent(mod.redux1)
residuos=residuals(mod.redux1)


###La siguiente matriz de gráficos no se incluye en una investigación con datos.
###Tiene únicamente fines didácticos

par(mfrow=c(1,3))
boxplot(residuos, horizontal=TRUE)
title("Residuos")
boxplot(estandariz, horizontal=TRUE, col=2)
title("Residuos estandarizados")
boxplot(estudentiz, horizontal=TRUE,col=3)
title("Residuos estudentizados")

par(mfrow=c(1,1))  #Para restablecer a un gráfico por panel#



###Valores extremos en x

###Leverage###

lev.mod.redux=hatvalues(mod.redux1)


###Valores influyentes###

###DFFIT
dffit.mod.redux=dffits(mod.redux1)

###D de Cook
cook.mod.redux=cooks.distance(mod.redux1)


###DFBETAs
dfbeta.mod.redux=dfbetas(mod.redux1)
dfbeta.mod.redux2=dfbeta(mod.redux1)

summary(cbind(lev.mod.redux,dffit.mod.redux,cook.mod.redux))

summary(dfbeta.mod.redux)
summary(dfbeta.mod.redux2)

###Los dfbetas generados con la función "dfbeta" genera los cambios absolutos en los betas
###Los dfbetas generados con la función "dfbetas" genera los cambios estandarizados
###que son acordes con las fórmulas provistas en clase


###Definir los límites para considerar como valor extremo

##Para leverage

(lim.lev=2*length(mod.redux1$coef)/length(lev.mod.redux))

plot(mod.redux1$fitted,lev.mod.redux,xlim=c(-20000,250000),ylim=c(0,0.3))
abline(h=lim.lev, col=5)
title("Leverage por valores predichos")
identify(mod.redux1$fitted,lev.mod.redux,identif)
text(mod.redux1$fitted[lev.mod.redux>0.15],lev.mod.redux[lev.mod.redux>0.15],identif[lev.mod.redux>0.15],cex=0.7)


plot(mod.redux1$fitted,estandariz, cex=lev.mod.redux/mean(lev.mod.redux))
abline(h=0, col=6)
title("Residuos por valores predichos, ponderados por leverage")
identify(mod.redux1$fitted,estandariz,identif)

###Para DFFITs

plot(mod.redux1$fitted,estandariz, cex=abs(dffit.mod.redux)/mean(abs(dffit.mod.redux)),xlim=c(-20000,250000),ylim=c(-8,8))
abline(h=0, col=6)
title("Residuos por valores predichos, ponderados por DFFIT")
identify(mod.redux1$fitted,estandariz,identif)


(lim.dffit=2*((length(mod.redux1$coef)/length(lev.mod.redux))^.5))




plot(estandariz, dffit.mod.redux,xlim=c(-8,8),ylim=c(-3,3))
abline(h=lim.dffit, col=6)
abline(h=0)
abline(h=-lim.dffit, col=6)
title("DFFIT vs. Residuos")
identify(estandariz, dffit.mod.redux,identif)


###Para D de Cook

###Gráfico ponderado por D de Cook

plot(mod.redux1$fitted,estandariz, cex=0.1*cook.mod.redux/mean(cook.mod.redux),xlim=c(-200000,260000),ylim=c(-10,10))
abline(h=0, col=6)
title("Residuos por valores predichos, ponderados por D de Cook")
identify(mod.redux1$fitted,estandariz,identif)

lim.dcook2=qf(0.5,length(mod.redux1$coef),(length(cook.mod.redux)-length(mod.redux1$coef)))
lim.dcook1=qf(0.2,length(mod.redux1$coef),(length(cook.mod.redux)-length(mod.redux1$coef)))


###Gráfico para identificar límite de D de Cook

plot(estandariz, cook.mod.redux,xlim=c(-8,8),ylim=c(0,1))
abline(h=lim.dcook1, col=6)
abline(h=lim.dcook2, col=6)
title("D de Cook vs. Residuos")
identify(estandariz, cook.mod.redux,identif)


###Exploración de DFBetas solo para ingreso

###Gráfico ponderado por dfbeta

dfbeta.mod.redux

plot(ingresototal,estandariz, cex=0.1*abs(dfbeta.mod.redux[,4])/mean(abs(dfbeta.mod.redux[,4])),xlim=c(-125000,6750000),ylim=c(-8,8))
abline(h=0, col=6)
title("Residuos por ingreso, ponderados por DFBETAS")
identify(ingresototal,estandariz,identif)

###Gráfico para identificar límite de Dfbeta
###Falta estandarizar los Dfbetas.
plot(ingresototal,dfbeta.mod.redux[,4],xlim=c(-125000,6750000))
abline(h=0, col=6)
abline(h=2/sqrt(length(ingresototal)), col=8)
abline(h=-2/sqrt(length(ingresototal)), col=8)

title("DFBETAS por ingresototal")
identify(ingresototal,dfbeta.mod.redux[,4],identif)


###Ahora DFBetas para miembrospercep

plot(miembrospercep,estandariz, cex=0.1*abs(dfbeta.mod.redux[,3])/mean(abs(dfbeta.mod.redux[,3])),xlim=c(0,8),ylim=c(-8,8))
abline(h=0, col=6)
title("Residuos por perceptores, ponderados por DFBETAS")
identify(miembrospercep,estandariz,identif)

###Hace falta estandarizar
plot(miembrospercep,dfbeta.mod.redux[,3],xlim=c(0,8))
abline(h=0, col=6)
abline(h=2/sqrt(length(miembrospercep)), col=8)
abline(h=-2/sqrt(length(miembrospercep)), col=8)

title("DFBETAS por miembrospercep")
identify(miembrospercep,dfbeta.mod.redux[,3],identif)


summary(cbind(gasto_ocio,edadjefe,miembrospercep,ingresototal))
gastoocio3[identif==192 | identif==2 | identif==158,c(1,5,8,11,12)]

###Comparamos los valores promedio con los valores extremos No.192 y No.2 

###Quitamos el caso No. 2 para volver a correr el modelo reducido
gastoocio32=gastoocio3[-2,]

mod.redux2=lm(gasto_ocio~edadjefe+miembrospercep+ingresototal, data=gastoocio32)
summary(mod.redux2)
summary(mod.redux1)
##Note que sube el R2 ajustado en el modelo sin el caso 2

round(mod.redux1$coefficients,2)
round(mod.redux2$coefficients,2)
###Por el contrario, los coeficientes no cambian mucho


###Se hace lo mismo pero se quitan los casos 192 y 78
gastoocio33=gastoocio3[c(-192,-78),]

mod.redux3=lm(gasto_ocio~edadjefe+miembrospercep+ingresototal, data=gastoocio33)
summary(mod.redux3)

round(mod.redux1$coefficients,2)
round(mod.redux3$coefficients,2)

###Los coeficientes cambian especialmente la pendiente de miembrosperceptores













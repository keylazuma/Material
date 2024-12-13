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



###Calculos de VIF

vif(mod.grande1)

vif(mod.redux1)


###Se reitera que los avPlots no son diagnósticos, sino una forma previa
###de quitar variables que produzcan multicolinealidad.
avPlots(mod.grande1)

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
abline(lm(gasto_ocio~pobmenores))

###Note que no es necesario analizar linealidad con las otras variables binarias:
###alquilada, prestamcasa y urbano

###Note además que las tendencias aparentan ser lineales, pero hay valores extremos
###que ponen en duda esa linealidad



###Ahora se analiza normalidad


###Normalidad de los residuos


qqPlot(mod.grande1$residuals)
hist(mod.grande1$residuals)

###H0: Errores se distribuyen normalmente


shapiro.test(mod.grande1$residuals)
jarque.bera.test(mod.grande1$residuals)

(cme=anova(mod.grande1)[4,3])

ks.test(mod.grande1$residuals,"pnorm",mean=0,sd=(cme)^0.5)

##Reproduciendo el test de jarque.bera

res2.grande=rstudent(mod.grande1)

kurtosis(res2.grande)

skewness(res2.grande)

jb=length(res2.grande)*((I(skewness(res2.grande)^2)/6)+(I(kurtosis(res2.grande)^2)/24))
jb

1-pchisq(jb,2)

###La prueba no puede reproducir exactamente los cálculos
###por correcciones que hace R. 

res.grande=mod.grande1$residuals

kurtosis(res.grande)

skewness(res.grande)

jb=length(res.grande)*((I(skewness(res.grande)^2)/6)+(I(kurtosis(res.grande)^2)/24))
jb

1-pchisq(jb,2)



par(mfrow=c(1,1))
####Ahora heteroscedasticidad con mod.grande y mod.reducido

plot(mod.grande1$fitted,mod.grande1$residuals)

scatterplot(mod.grande1$fitted,mod.grande1$residuals)

plot(mod.redux1$fitted,mod.redux1$residuals)

scatterplot(mod.redux1$fitted,mod.redux1$residuals)

###También se puede analizar la heteroscedasticidad con cada uno
###de los predictores

###Para ello voy a tomar el modelo reducido


scatterplot(edadjefe,mod.redux1$residuals)

scatterplot(miembrospercep,mod.redux1$residuals)

scatterplot(ingresototal,mod.redux1$residuals)


##Estos gráficos sugieren que la heteroscedasticidad está asociada al ingreso.

###Se pueden hacer pruebas de hipótesis

###H0: Sigma_i =  Sigma
###H1: Sigma_i <> Sigma

###Prueba de Breusch-Pagan

bptest(mod.grande1)


###Prueba de White
###La prueba de White para el modelo grande ser?a muy extensa

###Por eso no la voy a plantear.  

### Vean lo que pasa con el modelo reducido



###Prueba de Breusch-Pagan

bptest(mod.redux1)

###Prueba de White

bptest(mod.redux1,   ~ edadjefe*miembrospercep
                    +edadjefe*ingresototal+miembrospercep*ingresototal 
                    + I(edadjefe^2)+I(miembrospercep^2)+I(ingresototal^2)) 


###Obteniendo paso a paso la prueba de Breusch-Pagan
###Para el modelo reducido

res.redux1=residuals(mod.redux1)

anova(mod.redux1)

q=(res.redux1^2)/(anova(mod.redux1)[4,2]/length(res.redux1))

summary(q)

mod.bp.redux1=lm(q~edadjefe +miembrospercep +ingresototal )
anova(mod.bp.redux1)

cita=0.5*sum(anova(mod.bp.redux1)[c(1,2,3),2])
cita

bptest(mod.redux1)
bptest(mod.redux1, studentize=FALSE)

1-pchisq(cita,length(mod.bp.redux1$coef)-1)

###Note que se logra reproducir el estadístico chi-cuadrado de Jarque Bera
###cuando se pide "studentize=FALSE".  Esto significa que el default de la prueba de Jarque Bera
###es con residuos estudentizados que estudiaremos un poco más adelante.


###Para hacer el test de White, sencillamente se estima un modelo polinomial de orden 2


mod.white.redux1=lm(q~edadjefe*miembrospercep
                    +edadjefe*ingresototal+miembrospercep*ingresototal 
                    + I(edadjefe^2)+I(miembrospercep^2)+I(ingresototal^2))
anova(mod.white.redux1)

cita=0.5*sum(anova(mod.white.redux1)[(1:9),2])
cita

bptest(mod.redux1, studentize=FALSE,   ~ edadjefe*miembrospercep
       +edadjefe*ingresototal+miembrospercep*ingresototal 
       + I(edadjefe^2)+I(miembrospercep^2)+I(ingresototal^2)) 


1-pchisq(cita,length(mod.white.redux1$coef)-1)

####Otra prueba de homoscedasiticidad, que es una variante de la prueba de White
###Pues se basa en estimar los residuos con una ecuación en función de los valores predichos


ncvTest(mod.grande1)

ncvTest(mod.redux1)

mod.bp.redux2=lm(q~mod.redux1$fitted.values)
anova(mod.bp.redux2)

cita=0.5*sum(anova(mod.bp.redux2)[1,2])
cita



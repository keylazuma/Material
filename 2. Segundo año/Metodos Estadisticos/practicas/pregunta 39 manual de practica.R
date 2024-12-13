##Suponga que se está analizando los gastos de consumo de electricidad de una muestra 
#de 50 hogares (use el archivo electricidad.Rdata).  Se desea analizar si el número de
#miembros del hogar, si el hogar es propio (alquilado=0) o alquilado (alquilado=1) y los 
#metros cuadrados de construcción de la vivienda determinan el gasto en electricidad (en Kw).



#a.	Haga gráficos de dispersión entre cada variable independiente y el consumo de electricidad.

attach(electricidad)
names(electricidad)

par(mfrow=c(1,3))
plot(gastokw~alquilado)
plot(gastokw~as.factor(alquilado))
plot(gastokw~miembroshogar)
plot(gastokw~metroscuad)

#b.	Estime un modelo de regresión con la variable dependiente y las independientes antes señaladas.
##Escriba la ecuación.

mod.39 <-  lm(gastokw~alquilado+miembroshogar+metroscuad)
summary(mod.39)


#c.	Sin mirar los p-values de los coeficientes, interprete los 4 coeficientes de regresión
#que tiene en la ecuación.





#d.	Plantee las hipótesis nulas y alternativas tanto de los coeficientes como la hipótesis 
#nula global (la prueba F) implícitas en la salida de regresión.
#H0:beta0=0    h1:beta0<>0
#H0:beta1=0    h1:beta1<>0
#H0:beta2=0    h1:beta2<>0
#H0:beta3=0    h1:beta3<>0
##H0: Beta1=Beta2=Beta3=0; H1: Al menos un Betai<>0


#e.	Interprete los resultados de cada una de las pruebas de hipótesis usando 
#como indicador los p-values.

# se rechaza h0 que beta0=0
# se rechaza h0 que beta1=0
#  no se rechaza h0 que beta2=0
# se rechaza h0 que beta3=0
# en la prueba global rechazo h0: beta1=beta2=beta3=0

#los buenos predictores son los que se rechazan


#f.	Interprete el coeficiente de determinación.
#el modelo explica el 64% de la variabliddad del gasto de electricidad


##g.	Interprete el gráfico de residuos contra los predichos (puede suponer homoscedasticidad).
par(mfrow=c(1,2))
plot(mod.39$fitted.values,mod.39$residuals)
plot(mod.39$residuals~mod.39$fitted.values)
#si se podria suponer homocedasticidad porque la nube de puntos
#forma un rectangulo horizontal a pesar de que tengo dos valores
#extremos


#h.	Realice la prueba de Shapiro para los residuos.  Se rechaza o no se rechaza H0, con un ???=0.05.
shapiro.test(mod.39$residuals)
#no hay suficiente evidencia estadistica pra rechazar de de que los 
#la hn de que los errores se dsitribuyen normalente

library(car)
qqPlot(mod.39$residuals)

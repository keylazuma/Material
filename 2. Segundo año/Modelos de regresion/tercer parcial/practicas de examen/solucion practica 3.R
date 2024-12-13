attach(cigarros)
cigar <- cigarros$cigarros
###1.	Haga una matriz de gráficos con el gráfico de dispersión de la variable 
###dependiente (cigarros) vs. hatcalor, hatalco y deprescale (3 ptos.)
par(mfrow=c(1,3))
plot(cigar~hatcalor)
abline(lm(cigar~hatcalor),col="red")
plot(cigar~hatalco)
abline(lm(cigar~hatalco),col="red")
plot(cigar~deprescale)
abline(lm(cigar~deprescale),col="red")

library(car)
mod1 <- lm(cigar~hatcalor+hatalco+deprescale)
crPlots(mod1)
###2.	Según los gráficos de dispersión, ¿en cuáles asociaciones puede argumentar 
###linealidad? (2 ptos.)
##R/ En realidad en todos.


###3.	Estime una ecuación de regresión en la que prediga el número de cigarros en 
###función de la escala de depresión, las calorías consumidas y la cantidad de alcohol consumido (3 ptos.)

summary(mod1)

###4.	Interprete los coeficientes de estas tres variables explicativas (3 ptos.)


###R/ Por cada aumento de una Kilocaloria en el consumo, el número de 
###cigarros dismimuye en -0.000072 cigarros, manteniendo constante las demás variables
###Por cada gramo adicional de alcohol consumido, el número de cigarrillos 
###aumenta en 0.18 cigarros, manteniendo constante las demás variables
###Por cada aumento de un punto en la escala de depresión, el número de cigarros aumenta
###en 0.08 cigarros, manteniendo las demás variables constantes.


###5.	Haga un gráfico de residuos estandarizados vs. leverage y, si hay valores 
###extremos y/o influyentes, identifique los casos con la variable cigarros (4 ptos.).  

estandariz <- rstandard(mod1)
leverage <- hatvalues(mod1)

plot(estandariz~leverage)
abline(h=c(-2,2),col="red")
abline(v=2*4/165)


###6.	¿Hay valores extremos en el consumo de cigarros que tengan influencia en 
###los resultados de la regresión?  ¿Diga por qué?  (2 ptos.)
summary(index(cigarros))
dedeCook=cooks.distance(mod1)
plot(estandariz~leverage,cex=0.1*dedeCook/mean(dedeCook))
abline(h=c(-2,2),col="red")
abline(v=2*4/165,col="blue")

id <- index
text(leverage[leverage>0.10 & estandariz>2],
     estandariz[leverage>0.10 & estandariz>2],
     index(cigarros)[leverage>0.10 & estandariz>2])


###7.	Analice el supuesto de homoscedasticidad con una prueba de Breusch-Pagan, 
###con un ???=0.05.  ¿Se puede mantener el supuesto de homoscedasticidad? (3 ptos.)
library(lmtest)
bptest(mod1)
###Sí se rechaza la H0: Sigma_i=Sigma

###8.	Analice la normalidad de los residuos con una prueba de Shapiro-Wilks, 
###con un ???=0.05.  ¿Qué concluye? (3 ptos.)

###R/ H0: Errores provienen de una distribucion normal
shapiro.test(estandariz)

###Hay suficiente evidencia estadística para rechazar la H0 de que los errores 
###provienen de una distribución normal


###9.	Haga una transformación de BoxCox a la variable dependiente escogiendo la 
###transformación que más se ajuste a los datos (3 ptos.)

bc <- boxCox(cigar~hatcalor+hatalco+deprescale)
bc$x[bc$y==max(bc$y)]

###10.	Vuelva a estimar el modelo en el que la variable transformada de cigarros 
###es predicha por las calorías consumidas, el alcohol consumido y la escala de depresión. Usando como base las pruebas de hipótesis para los coeficientes de regresión, cómo cambia la intepretación de los factores que determinan la cantidad de cigarros al usar la variable transformada en comparación con la variable sin transformar? (3 ptos.)

raiz5.cigarros <- (cigar)^(0.2)
mod.bc <- lm(raiz5.cigarros~hatcalor+hatalco+deprescale)
summary(mod.bc)



###11.	Haga un gráfico de residuos vs. predichos y argumente si la forma de dicho
###gráfico sugiere o no homoscedasticidad (3 ptos.)

scatterplot(rstandard(mod.bc)~fitted(mod.bc))

###12.	Estime una prueba de Breusch-Pagan para el modelo con la variable 
###transformada, con un ???=0.05 y concluya apropiadamente (3 ptos.).  

bptest(mod.bc)

###13.	Conteste: Si la transformación de Box-Cox fue creada para corregir el 
###supuesto de normalidad de los residuos, cómo una transformación de BoxCox 
###puede afectar los resultados de una prueba de homoscedasticidad (ya sea White y Breusch-Pagan)?  Nota: No estoy buscando que digan que la prueba de Breusch-Pagan supone normalidad y la de White es más robusta ante tal violación. (3 ptos)


##R/ En realidad, no lo afecta en este caso porque se sigue concluyendo heteroscedasticidad
##Pero una transformación de BoxCox puede controlar valores extremos
##que afectan a la vez el supuesto de normalidad y el supuesto
##de homoscedasticidad



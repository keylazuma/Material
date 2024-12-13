rm(list=ls(all=T))

setwd("C:/Users/Gilbert BC/Documents/Teletrabajo/regresion 2020/Labs/Lab 5/") 

load("vida.Rdata")

###En la investigaci�n nutricional de adultos mayores se est� utilizando la circunferencia 
###de la esper como indicador de problemas de peso.  Se le da el archivo 
###esper.Rdata que contiene las variables que se describen abajo.  
###Se desea analizar si la altura de la Population, la Income, el peso en libras, 
##los d�as para hacer ejercicios, sexo, condici�n de Frost y HS.Grad predicen 
###la circunferencia de la esper para validar su uso como indicador nutricional.

###Sin embargo, como la ecuaci�n se quiere usar para predecir la circunferencia de esper,
###se busca un modelo m�s peque�o que sea m�s f�cil de programar.




library(car)
library(olsrr)
library(qpcR)

###Explorando las asociaciones lineales con un grafico###

base=as.data.frame(base)
attach(base)


base$Population=base$pop   
base$Income=base$ingre   
base$Illiteracy=base$analf   
base$Murder=base$crim 
base$HS.Grad=base$grad  
base$Frost=base$temp 
base$Area=base$area

detach(base)
attach(base)
names(base)

par(mfrow=c(2,4))


plot(Population, esper)
plot(Income, esper)
plot(Illiteracy, esper)
plot(Murder, esper)
plot(HS.Grad, esper)
plot(Frost, esper)
plot(Area, esper)
###Primero se estima un modelo grande y un modelo nulo

mod.nulo=lm(esper~1)

modgrande=lm(esper~Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area)

summary(modgrande)


####Se experimenta primero con un modelo stepwise basado en p-values.

###Procedimiento forward
###Se incorporan las variables cuyo coeficiente tenga un pvalue menor a 0.05

ols_step_forward_p(modgrande, penter=0.05, progress=TRUE)

###Procedimiento backward
###Se excluyen las variables cuyo coeficiente tenga un pvalue mayor a 0.05

ols_step_backward_p(modgrande, prem=0.05, progress=TRUE)

###Procedimiento stepwise forward backward
###Combinaci�n de ambos.

ols_step_both_p(modgrande, penter=0.05, prem=0.05, progress=TRUE)

mod1.both=ols_step_both_p(modgrande, penter=0.05, prem=0.05, progress=TRUE)
summary(mod1.both$model)




####Ahora procedimiento usando AIC
###Se prefiere el modelo con el AIC � el BIC m�s bajo


###Procedimiento forward

step(mod.nulo, scope=~Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area, direction="forward")


###Procedimiento backward

step(modgrande, direction="backward")

###Procedimiento stepwise forward/backward
step(modgrande, direction="both")
modelos.step= step(modgrande)
summary(modelos.step)

summary(mod1.both$model)



library(leaps)
library(faraway)

###Un procedimiento en que se escoge el modelo con el mejor BIC
###Para cada subconjunto de variables desde 1 hasta p-1
###Se puede observar en un gr�fico

solovariables=as.data.frame(esper[,c(2,3,4,5,6,7,8,9)])

conjuntos=regsubsets(esper~Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area, data=solovariables,nbest=1)

subsets(conjuntos)


###C�lculo a pie del BIC y el AIC

mod.peque�o=lm(esper~Murder+HS.Grad+Frost)

logLik(mod.peque�o)
logLik(modgrande)

AIC(mod.peque�o)
BIC(mod.peque�o)

-2*logLik(mod.peque�o)+2*(length(mod.peque�o$coef)+1)
-2*logLik(mod.peque�o)+(length(mod.peque�o$coef)+1)*log(length(mod.peque�o$fitted))


###PRESS y Pcuadrado


###Primero con la funci�n de R

(press.grande=ols_press(modgrande))
(press.peque�o=ols_press(mod.peque�o))

(ag=anova(modgrande))  ##Suma de Cuadrados Total
(ap=anova(mod.peque�o))  ##Suma de Cuadrados Total

(psq.grande=1-press.grande/sum(ag[,2]))  ##F�rmula de c�lculo a mano
(psq.peque�o=1-press.peque�o/sum(ag[,2]))

PRESS(modgrande, verbose=FALSE)$stat  #Son subobjetos de un objeto PRESS
PRESS(modgrande, verbose=FALSE)$P.square

PRESS(mod.peque�o, verbose=FALSE)$stat
PRESS(mod.peque�o, verbose=FALSE)$P.square



###Calculo a mano del PRESS
###En lugar de hacer un ciclo, se usa la equivalencia a partir de los leverage

(press.grande.apie=sum((modgrande$residuals/(1-hatvalues(modgrande)))^2))

(press.peqe�o.apie=sum((mod.peque�o$residuals/(1-hatvalues(mod.peque�o)))^2))




##Juntando los indicadores del modelo peque�o y del modelo grande

tabla=matrix(rep(NA,10),ncol=2,nrow=5)

tabla[1,1]=summary(modgrande)$adj.r.squared
tabla[2,1]=psq.grande
tabla[3,1]=press.grande
tabla[4,1]=AIC(modgrande)
tabla[5,1]=BIC(modgrande)

tabla[1,2]=summary(mod.peque�o)$adj.r.squared
tabla[2,2]=psq.peque�o
tabla[3,2]=press.peque�o
tabla[4,2]=AIC(mod.peque�o)
tabla[5,2]=BIC(mod.peque�o)

rownames(tabla)=c("R2 ajust","P2","PRESS","AIC","BIC")
colnames(tabla)=c("Grande","Peque�o")

round(tabla,4)



####Pr�ctica para los estudiantes:
###Tome las variables del modelo peque�o: Population, Income y Illiteracy
###Estime todos los modelos de regresi�n lineal simple con esas variables
###Y todos los modelos con 2 variables (Population y Income, Population y Illiteracy, 
###Income y Illiteracy).  Para todos estos 6 modelos, calcule:
###el R cuadrado ajustado, el P-cuadrado, el PRESS, el AIC y el BIC.
###Haga un cuadro comparando dichos indicadores y diga si coinciden en el 
###modelo escogido.  
###Adem�s, haga un gr�fico de dispersi�n en el que compare el R2 ajustado con
###el P-cuadrado, y el PRESS con el AIC.

###Qu� concluye?

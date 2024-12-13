# XS-0223: MODELOS DE REGRESIÓN APLICADOS
# II SEMESTRE 2019

# LABORATORIO No.6a
# ANÁLISIS DE SUPUESTOS DEL MODELO LINEAL GAUSSIANO
# EJEMPLOS DE LAS PRESENTACIONES EN CLASE

##1. AUTOCORRELACIÓN


###EJEMPLO DEL GUJARATI FIGURA 12.7
##DATOS EN PÁG. 442
##Se trata de predecir el índice de producción por hora de la Fuerza laboral en EEUU entre 1960 y 1991
## con base en el índice de compensación real por hora, o sea, un índice del salario


indice.sal.hora=c(68.7,70.7,73.2,75,77.9,79.6,82.9,84.9,88.2,89.7,91.2,93,95.8,98,97,97.7,100.8,102.3,103.4,102,99.5,98.7,100,100.5,100.4,101.3,104.4,104.3,104.4,103,103.2,103.9)
indice.prod.hora=c(65.5,68,70.4,73.2,76.4,78.5,80.7,82.7,85.3,85.7,86.9,89.8,92.6,95,93.2,95.4,98.2,99.8,100.4,99.3,98.6,99.9,100,102,104.6,106.1,108.3,109.4,110.4,109.5,109.7,110.1)

anyo=(1960:1991)

cbind(anyo,indice.sal.hora,indice.prod.hora)

##Se debería de empezar por un análisis descriptivo
##graficando cada variable por el tiempo, y el gráfico de dispersión típico entre x y y

par(mfrow=c(1,3))
plot(anyo,indice.sal.hora)
plot(anyo,indice.prod.hora)

plot(indice.prod.hora~indice.sal.hora)



mod1=lm(indice.prod.hora~indice.sal.hora)
summary(mod1)

res=mod1$residuals
fit=mod1$fitted.values

##Diagnóstico gráfico
##Después de estimar y explorar el modelo se grafican los residuos por año

plot(anyo,res, ylab=("Residuos"),xlab=("Año"))
title("Residuos vs. año")
abline(h=0, col="red")

##Para los diagnósticos basados en pruebas de hipótesis
##Se usará la prueba de Durbin-Watson y la prueba de Geary (que es la prueba de rachas de Métodos).
library(car)
library(tseries)

durbinWatsonTest(mod1)


durbinWatsonTest(mod1, max.lag=2)

###Para poder hacer la prueba de rachas, hay que recodificar la variable en binaria,
###codificada en -1 y 1

res.factor=as.factor(recode(res, "lo:0=-1;0:hi=1") )


runs.test(res.factor)


##2. Heteroscedasticidad

#Primero se analizará el efecto de la heteroscedasticidad, y cómo afecta los errores estándar
#mediante simulaciones

###Simulación del beta1 y su error estándar en un modelo de regresión lineal simple

###Primero bajo condiciones de homoscedasticidad

  sim.beta=function(a,b,sig,n,min=1,max=10){
    x=runif(n,min,max)
    y=a+b*x+rnorm(n,0,sig)
    return(list("x"=x,"y"=y))
  }

  a=2; b=3; sig=5; n=10

  nrep=1000
  b1=numeric(nrep)
  se1=numeric(nrep)

  for(i in 1:nrep){
    s1=sim.beta(a,b,sig,n)
    m=lm(s1$y~s1$x)
    b1[i]=lm(s1$y~s1$x)$coef[2]
    se1[i]=sqrt(anova(m)[2,3]*summary(m)$cov[2,2])
  }


  sd(b1)           #Error estándar "verdadero"
  median(se1)      #Error estándar estimado (mediana de los errores estándar)
  razon1=sd(b1)/median(se1)
  razon1

# Modelo heteroscedástico vs. comparación MCO


  sim.beta2=function(a,b,n,exp,min=1,max=10){
    x=runif(n,min,max)
    y=a+b*x+rnorm(n,0,(x^exp))
    return(list("x"=x,"y"=y))
  }

###Caso en el que los errores son función de x al cuadrado
  a=2; b=3; n=10; exp=2

  nrep=1000
  b2=numeric(nrep)
  se2=numeric(nrep)

  for(i in 1:nrep){
    s2=sim.beta2(a,b,n,exp)
    m=lm(s2$y~s2$x)
    b2[i]=lm(s2$y~s2$x)$coef[2]
    se2[i]=sqrt(anova(m)[2,3]*summary(m)$cov[2,2])
  }


  sd(b2)           #compare este valor con el error estándar anterior
  median(se2)

    razon2=sd(b2)/median(se2)

    razon2

###Caso en el que los errores son función de x al cubo
  a=2; b=3; n=10; exp=3

  nrep=1000
  b3=numeric(nrep)
  se3=numeric(nrep)

  for(i in 1:nrep){
    s3=sim.beta2(a,b,n,exp)
    m=lm(s3$y~s3$x)
    b3[i]=lm(s3$y~s3$x)$coef[2]
    se3[i]=sqrt(anova(m)[2,3]*summary(m)$cov[2,2])
  }


  sd(b3)           #compare este valor con el error estándar anterior
  median(se3)

  razon3=sd(b3)/median(se3)
  razon3


 ###Caso en el que los errores son función de x a la cuatro
  a=2; b=3; n=10; exp=4

  nrep=1000
  b4=numeric(nrep)
  se4=numeric(nrep)

  for(i in 1:nrep){
    s4=sim.beta2(a,b,n,exp)
    m=lm(s4$y~s4$x)
    b4[i]=lm(s4$y~s4$x)$coef[2]
    se4[i]=sqrt(anova(m)[2,3]*summary(m)$cov[2,2])
  }


  sd(b4)           #compare este valor con el error estándar anterior
  median(se4)
  
  razon4=sd(b4)/median(se4)
  razon4

  ###Se hace un cuadro resumen para representar todas las relaciones
  
  cuadro=matrix(rep(0,12),nrow=3,byrow=TRUE)
  
  cuadro[1,1]=sd(b1)
  cuadro[1,2]=sd(b2)
  cuadro[1,3]=sd(b3)
  cuadro[1,4]=sd(b4)
  cuadro[2,1]=median(se1)
  cuadro[2,2]=median(se2)
  cuadro[2,3]=median(se3)
  cuadro[2,4]=median(se4)
  cuadro[3,1]=razon1
  cuadro[3,2]=razon2
  cuadro[3,3]=razon3
  cuadro[3,4]=razon4

  ##colnames(cuadro, do.NULL = FALSE)
  ##rownames(cuadro, do.NULL = FALSE)
  colnames(cuadro)=c("Homosced","Al cuad", "Al cubo", "A la 4")
  rownames(cuadro)=c("Verdadero","Estimado","Verd/Estim")
  
  cuadro
  

#3. Se analizará el efecto de la multicolinealidad

###Se van a simular 2 variables x con la misma media pero distintos grados de correlación
###y con distintos tamaños de muestra
###Se va a explorar si el beta1 deja de ser significativamente distinto de cero


library(MASS)
library(car)

###Toda la información se va a meter a cuadro2

cuadro2=matrix(rep(0,16),nrow=4,byrow=TRUE)
cuadro3=matrix(rep(0,16),nrow=4,byrow=TRUE)

###Creando las matrices de correlación
###Con 0.10, 0.30, 0.70 y 0.95

Sigma.0.10 <- matrix(c(1,0.10,0.10,1),2,2)
Sigma.0.10

Sigma.0.30 <- matrix(c(1,0.30,0.30,1),2,2)
Sigma.0.30

Sigma.0.70 <- matrix(c(1,0.70,0.70,1),2,2)
Sigma.0.70

Sigma.0.95 <- matrix(c(1,0.95,0.95,1),2,2)
Sigma.0.95


####Con n=10

residuos=rnorm(10,mean=0,sd=4)

##La función mvrnorm permite generar números aleatorios de una distribución normal
##multivariada con matriz de variancia-covariancia igual a Sigma

x10=(mvrnorm(n=10, rep(0, 2), Sigma.0.10, empirical=TRUE))
x10.1=x10[,1]
x10.2=x10[,2]

cor(x10.1,x10.2)


x30=(mvrnorm(n=10, rep(0, 2), Sigma.0.30, empirical=TRUE))
x30.1=x30[,1]
x30.2=x30[,2]

cor(x30.1,x30.2)


x70=(mvrnorm(n=10, rep(0, 2), Sigma.0.70, empirical=TRUE))
x70.1=x70[,1]
x70.2=x70[,2]

cor(x70.1,x70.2)


x95=(mvrnorm(n=10, rep(0, 2), Sigma.0.95, empirical=TRUE))
x95.1=x95[,1]
x95.2=x95[,2]

cor(x95.1,x95.2)


y10=1+2*x10.1+4*x10.2+residuos
mod.10=(lm(y10~x10.1+x10.2))


y30=1+2*x30.1+4*x30.2+residuos
mod.30=(lm(y30~x30.1+x30.2))


y70=1+2*x70.1+4*x70.2+residuos
mod.70=(lm(y70~x70.1+x70.2))


y95=1+2*x95.1+4*x95.2+residuos
mod.95=(lm(y95~x95.1+x95.2))

cor(cbind(y95,x95.1,x95.2))



summary(mod.10)
summary(mod.30)
summary(mod.70)
summary(mod.95)

cuadro2[1,1]=summary(mod.10)$coef[2,1]
cuadro3[1,1]=(summary(mod.10)$coef[2,4]<0.05)

cuadro2[2,1]=summary(mod.30)$coef[2,1]
cuadro3[2,1]=(summary(mod.30)$coef[2,4]<0.05)

cuadro2[3,1]=summary(mod.70)$coef[2,1]
cuadro3[3,1]=(summary(mod.70)$coef[2,4]<0.05)

cuadro2[4,1]=summary(mod.95)$coef[2,1]
cuadro3[4,1]=(summary(mod.95)$coef[2,4]<0.05)



vif(mod.10)
vif(mod.30)
vif(mod.70)
vif(mod.95)



####Con n=30


Sigma.0.10 <- matrix(c(1,0.10,0.10,1),2,2)
Sigma.0.10

Sigma.0.30 <- matrix(c(1,0.30,0.30,1),2,2)
Sigma.0.30

Sigma.0.70 <- matrix(c(1,0.70,0.70,1),2,2)
Sigma.0.70

Sigma.0.95 <- matrix(c(1,0.95,0.95,1),2,2)
Sigma.0.95


residuos=rnorm(30,mean=0,sd=4)


x10=(mvrnorm(n=30, rep(0, 2), Sigma.0.10, empirical=TRUE))
x10.1=x10[,1]
x10.2=x10[,2]

cor(x10.1,x10.2)


x30=(mvrnorm(n=30, rep(0, 2), Sigma.0.30, empirical=TRUE))
x30.1=x30[,1]
x30.2=x30[,2]

cor(x30.1,x30.2)


x70=(mvrnorm(n=30, rep(0, 2), Sigma.0.70, empirical=TRUE))
x70.1=x70[,1]
x70.2=x70[,2]

cor(x70.1,x70.2)


x95=(mvrnorm(n=30, rep(0, 2), Sigma.0.95, empirical=TRUE))
x95.1=x95[,1]
x95.2=x95[,2]

cor(x95.1,x95.2)


y10=1+2*x10.1+4*x10.2+residuos
mod.10=(lm(y10~x10.1+x10.2))


y30=1+2*x30.1+4*x30.2+residuos
mod.30=(lm(y30~x30.1+x30.2))


y70=1+2*x70.1+4*x70.2+residuos
mod.70=(lm(y70~x70.1+x70.2))


y95=1+2*x95.1+4*x95.2+residuos
mod.95=(lm(y95~x95.1+x95.2))

cor(cbind(y95,x95.1,x95.2))


summary(mod.10)
summary(mod.30)
summary(mod.70)
summary(mod.95)

cuadro2[1,2]=summary(mod.10)$coef[2,1]
cuadro3[1,2]=(summary(mod.10)$coef[2,4]<0.05)

cuadro2[2,2]=summary(mod.30)$coef[2,1]
cuadro3[2,2]=(summary(mod.30)$coef[2,4]<0.05)

cuadro2[3,2]=summary(mod.70)$coef[2,1]
cuadro3[3,2]=(summary(mod.70)$coef[2,4]<0.05)

cuadro2[4,2]=summary(mod.95)$coef[2,1]
cuadro3[4,2]=(summary(mod.95)$coef[2,4]<0.05)



vif(mod.10)
vif(mod.30)
vif(mod.70)
vif(mod.95)



####Con n=60


Sigma.0.10 <- matrix(c(1,0.10,0.10,1),2,2)
Sigma.0.10

Sigma.0.30 <- matrix(c(1,0.30,0.30,1),2,2)
Sigma.0.30

Sigma.0.70 <- matrix(c(1,0.70,0.70,1),2,2)
Sigma.0.70

Sigma.0.95 <- matrix(c(1,0.95,0.95,1),2,2)
Sigma.0.95


residuos=rnorm(60,mean=0,sd=4)


x10=(mvrnorm(n=60, rep(0, 2), Sigma.0.10, empirical=TRUE))
x10.1=x10[,1]
x10.2=x10[,2]

cor(x10.1,x10.2)


x30=(mvrnorm(n=60, rep(0, 2), Sigma.0.30, empirical=TRUE))
x30.1=x30[,1]
x30.2=x30[,2]

cor(x30.1,x30.2)


x70=(mvrnorm(n=60, rep(0, 2), Sigma.0.70, empirical=TRUE))
x70.1=x70[,1]
x70.2=x70[,2]

cor(x70.1,x70.2)


x95=(mvrnorm(n=60, rep(0, 2), Sigma.0.95, empirical=TRUE))
x95.1=x95[,1]
x95.2=x95[,2]

cor(x95.1,x95.2)


y10=1+2*x10.1+4*x10.2+residuos
mod.10=(lm(y10~x10.1+x10.2))


y30=1+2*x30.1+4*x30.2+residuos
mod.30=(lm(y30~x30.1+x30.2))


y70=1+2*x70.1+4*x70.2+residuos
mod.70=(lm(y70~x70.1+x70.2))


y95=1+2*x95.1+4*x95.2+residuos
mod.95=(lm(y95~x95.1+x95.2))

cor(cbind(y95,x95.1,x95.2))


summary(mod.10)
summary(mod.30)
summary(mod.70)
summary(mod.95)

cuadro2[1,3]=summary(mod.10)$coef[2,1]
cuadro3[1,3]=(summary(mod.10)$coef[2,4]<0.05)

cuadro2[2,3]=summary(mod.30)$coef[2,1]
cuadro3[2,3]=(summary(mod.30)$coef[2,4]<0.05)

cuadro2[3,3]=summary(mod.70)$coef[2,1]
cuadro3[3,3]=(summary(mod.70)$coef[2,4]<0.05)

cuadro2[4,3]=summary(mod.95)$coef[2,1]
cuadro3[4,3]=(summary(mod.95)$coef[2,4]<0.05)



vif(mod.10)
vif(mod.30)
vif(mod.70)
vif(mod.95)


####Con n=100


Sigma.0.10 <- matrix(c(1,0.10,0.10,1),2,2)
Sigma.0.10

Sigma.0.30 <- matrix(c(1,0.30,0.30,1),2,2)
Sigma.0.30

Sigma.0.70 <- matrix(c(1,0.70,0.70,1),2,2)
Sigma.0.70

Sigma.0.95 <- matrix(c(1,0.95,0.95,1),2,2)
Sigma.0.95


residuos=rnorm(100,mean=0,sd=4)


x10=(mvrnorm(n=100, rep(0, 2), Sigma.0.10, empirical=TRUE))
x10.1=x10[,1]
x10.2=x10[,2]

cor(x10.1,x10.2)


x30=(mvrnorm(n=100, rep(0, 2), Sigma.0.30, empirical=TRUE))
x30.1=x30[,1]
x30.2=x30[,2]

cor(x30.1,x30.2)


x70=(mvrnorm(n=100, rep(0, 2), Sigma.0.70, empirical=TRUE))
x70.1=x70[,1]
x70.2=x70[,2]

cor(x70.1,x70.2)


x95=(mvrnorm(n=100, rep(0, 2), Sigma.0.95, empirical=TRUE))
x95.1=x95[,1]
x95.2=x95[,2]

cor(x95.1,x95.2)


y10=1+2*x10.1+4*x10.2+residuos
mod.10=(lm(y10~x10.1+x10.2))


y30=1+2*x30.1+4*x30.2+residuos
mod.30=(lm(y30~x30.1+x30.2))


y70=1+2*x70.1+4*x70.2+residuos
mod.70=(lm(y70~x70.1+x70.2))


y95=1+2*x95.1+4*x95.2+residuos
mod.95=(lm(y95~x95.1+x95.2))

cor(cbind(y95,x95.1,x95.2))


summary(mod.10)
summary(mod.30)
summary(mod.70)
summary(mod.95)

cuadro2[1,4]=summary(mod.10)$coef[2,1]
cuadro3[1,4]=(summary(mod.10)$coef[2,4]<0.05)

cuadro2[2,4]=summary(mod.30)$coef[2,1]
cuadro3[2,4]=(summary(mod.30)$coef[2,4]<0.05)

cuadro2[3,4]=summary(mod.70)$coef[2,1]
cuadro3[3,4]=(summary(mod.70)$coef[2,4]<0.05)

cuadro2[4,4]=summary(mod.95)$coef[2,1]
cuadro3[4,4]=(summary(mod.95)$coef[2,4]<0.05)



vif(mod.10)
vif(mod.30)
vif(mod.70)
vif(mod.95)

###Resultado final

cuadro2
cuadro3



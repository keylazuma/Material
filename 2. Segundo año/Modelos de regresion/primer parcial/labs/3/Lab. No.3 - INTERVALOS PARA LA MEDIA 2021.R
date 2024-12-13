
# XS-2130: MODELOS DE REGRESION APLICADOS
# II SEMESTRE 2022

# LABORATORIO No.3
# INTERVALOS PARA LA MEDIA

#===============================================================================
# 1. Leer datos y ajustar modelos

  setwd("C:/Users/Gilbert BC/Documents/Teletrabajo/regresion 2022/Labs/Lab 3/")  # CAMBIAR SEGUN SU CASO

  load("base2.Rdata")
  attach(base2)
  
  mod1 = lm(gastoseguros~ingresoenmiles)
  mod2 = lm(gastoseguros ~ ingresoenmiles+numvehiculos)
  library(car); scatterplot(gastoseguros~ingresoenmiles)
#===============================================================================
# 2. Intervalo de confianza para la media 

  x0=c(1,3143.63,2)
  y0=predict(mod2,data.frame(ingresoenmiles=3143.63,numvehiculos=2))
  y0
  
  cme = anova(mod2)[3,3]
  cme
  x = cbind(1,ingresoenmiles,numvehiculos)
  varb=cme * summary(mod2)$cov
  varb
  vcov(mod2)
  

  varb


  var0= t(x0) %*% varb %*% x0
  var0
  ee0=sqrt(var0)
  ee0

  ##Este es el error estándar para una predicción media
  ###O sea, para el gasto promedio en seguros
  ###para todos los hogares con ingresoenmiles=3143.63 y numvehiculos=2
  
  n=nrow(x)
  p=ncol(x)
  t=qt(.975,n-p)   ###Este es el t tabular con n-p gl
  ic0=c(y0-t*ee0,y0+t*ee0)
  ic0
  
  predict(mod2,data.frame(ingresoenmiles=3143.63,numvehiculos=2),interval="confidence")

#===============================================================================
# 3. Intervalo de confianza para valores individuales

  varind = var0+cme
  eeind = sqrt(varind)
  eeind
  
  ###Error estándar para predicción individual
  ###O sea, para un solo hogar con 
  ###ingresoenmiles=3143.63 y numvehiculos=2
  
  icind=c(y0-t*eeind,y0+t*eeind)
  icind
  predict(mod2,data.frame(ingresoenmiles=3143.63,numvehiculos=2),interval="prediction")
  ic0
#===============================================================================
# 4. Hacer un gráfico con los intervalos
  mnumvehiculos=mean(numvehiculos)
  mnumvehiculos

  summary(ingresoenmiles)  ##Int de confianza para distintos valores de ingresoenmiles
  m16=seq(0,4000,250)      ##manteniendo fijo el numeros de vehiculos en la media=1.25


  ic.mean=predict(mod2,data.frame(ingresoenmiles=m16,numvehiculos=mnumvehiculos),interval="confidence")
  ic.mean

  ic.ind =predict(mod2,data.frame(ingresoenmiles=m16,numvehiculos=mnumvehiculos),interval="prediction")
  
  matplot(m16,ic.ind[,2:3],xlab="Ingreso (en miles de colones)",ylab="Gasto en seguros (en colones)",
          main="Intervalos con numvehiculos=1.25",lty=2,col="blue",type="l",ylim=c(0,25000))
  matlines(m16,ic.mean[,2:3],col="red",lty=1)
  lines(m16,ic.mean[,1])
  legend(2500,12000,c("Respuesta media","Respuesta individual"),col=c("red","blue"),
         lty=c(1,2),bty="n",cex=0.5)

#===============================================================================
# Coeficiente de determinación múltiple
  summary(mod1)
  anova(mod1)
  sce1=anova(mod1)[2,2]
  sctot1=sum(anova(mod1)[,2])
  sctot1
  var(gastoseguros)*(n-1)
  1-sce1/sctot1
  
  summary(mod1)
  names(summary(mod1))
  summary(mod1)$r.sq

  r=cor(gastoseguros,ingresoenmiles)
  r**2


  anova(mod2)
  sce2=anova(mod2)[3,2]
  sctot2=sum(anova(mod2)[,2])
  1-sce2/sctot2
  summary(mod2)
  summary(mod2)$r.sq
  summary(mod1)$r.sq
#===============================================================================
# Coeficiente de determinación parcial

  anova(mod1)
  anova(mod2)

  SCReg.numvehiculos.ingresoenmiles=181834903-155148505
  SCReg.numvehiculos.ingresoenmiles
  
  ry.21=26686398/181834903
  ry.21

  1-155148505/181834903

  mod3=lm(gastoseguros ~ numvehiculos)
  mod4=lm(gastoseguros ~ numvehiculos+ingresoenmiles)
  anova(mod3)
  anova(mod4)
  (SCReg.ingresoenmiles.numvehiculos=508186927-155148505) 
  
  ry.12=353038422/508186927
  ry.12
  1-155148505/508186927

#===============================================================================
# Comparación de modelos



  anova(mod1,mod2)
  
  ###H0: Omega.minuscula: Modelo reducido
  ###H1: Omega.mayuscula: Modelo más amplio
  
  ###H0: Beta2=0
  ###H1: Beta2<>0
  ##Fc=((181834903-155148505)/1) / (155148505/40)
  
  summary(mod2)

  mod5=lm(gastoseguros~1)   ###Modelo nulo
  
  ###H0: Beta1=Beta2=0
  ###H1: Beta1<>0 o Beta2<>0
  
  anova(mod5,mod2)

  rm(list=ls())
  
    ##setwd("D:/regresion 2018/Laboratorios/lab 3/")  

  load("ahorro.Rdata")  
  moda=lm(sr~pop15+pop75+dpi+ddpi,ahorro)
  summary(moda)
  modb=lm(sr~I(pop15+pop75)+dpi+ddpi,ahorro)
  summary(modb)
  
  ###H0: Beta1=Beta2
  ###H1: Beta1<>Beta2

  anova(modb,moda)

  modc=lm(sr~dpi+ddpi,ahorro)
  
  ###H0: Beta1=Beta2=0
  ###H1: Beta1<>0 o Beta2<>0
  
  anova(modc,moda)
#===============================================================================
   ###offset: Beta_J==1
  
  ###H0: Beta4=0.5
  ###H1: Beta4<>0.5
  
  ahorro$nddpi=0.5*ahorro$ddpi
  
  modd=lm(sr~pop15+pop75+dpi+offset(nddpi), ahorro)
  
  anova(modd,moda)

  
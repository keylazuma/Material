
# XS-2130: MODELOS DE REGRESION APLICADOS
# II SEMESTRE 2022

# LABORATORIO No.2
# INTERVALOS DE CONFIANZA

#===============================================================================
# 1. Leer datos y ajustar modelos

  load("base2.Rdata")
  attach(base2)
  
  mod1 = lm(gastoseguros~ingresoenmiles)
  mod2 = lm(gastoseguros ~ ingresoenmiles+numvehiculos)

  
  hist(gastoseguros)
  
#===============================================================================
# 2. Valores ajustados y residuales

  # Obtener un valor ajustado 
  predict(mod1, data.frame(ingresoenmiles=3143.63))  
  
  # Obtener un valor ajustado en el modelo con las dos variables
  predict(mod2, data.frame(ingresoenmiles=3143.63,numvehiculos=2))
  gastoseguros[1]
  
  # Obtener el vector de valores ajustados
  predict(mod1) 
  fitted(mod1)
  mod1$fit
  fit=mod1$fit
  
  mod1$fit[1]
  fit[1]
  fit[c(1,5,8)]
  
  b=mod1$coefficients
  b
  
  X=cbind(1,ingresoenmiles)
  dim(X)
  
  X%*%(b)
  
  # Obtener el vector de residuales
  gastoseguros-fit
  mod1$res
  residuals(mod1)
  res=mod1$res

  par(mfrow=c(1,2))  ##Matriz de gr?ficos
  plot(ingresoenmiles,gastoseguros,ylim=c(0,22000)) ##Gr?fico de x contra y
  abline(mod1,col=4)
  plot(ingresoenmiles,res,ylim=c(-8000,8000)) ###Gr?fico de x contra los residuos
  abline(h=0,col=2)
  


#===============================================================================
# 3. Cuadrado medio de error
 
  SCRes=sum(res**2)
  SCRes
  anova(mod1)
  anova(mod1)[2,2]
  
  CME=SCRes/(length(res)-length(mod1$coefficients))
  CME

#===============================================================================
# 4. Intervalos de confianza para los betas

# Obtener la matriz de varianza-covarianza de los betas
  anova(mod2)
  cme = anova(mod2)[3,3]
  x = cbind(1,ingresoenmiles,numvehiculos)
  varb=cme * solve(t(x) %*% x)
  varb

# Obtener los errores estándar de los betas
  sqrt(diag(varb))
  summary(mod2)            
  summary(mod2)$coef
  summary(mod2)$coef[,2]
  ee=summary(mod2)$coef[,2]

# Obtener los intervalos de confianza para los betas
  glib=length(base2$gastoseguros)-length(mod2$coef)
  t=qt(.975,glib)
  b=mod2$coef
  round(cbind(b-t*ee,b+t*ee),2)
  confint(mod2)
#===============================================================================


# 4. Introducción básica a las pruebas de hipótesis para los betas usando t

##H0: Beta_i=0
##H1: Beta_i<>0

tc=b/ee
tc

gl=dim(base2)[1]-length(tc)

pvalue=1-pt(abs(tc),gl)+pt(-abs(tc),gl)
round(pvalue,4)


cbind(b,ee,tc,round(pvalue,4))
summary(mod2)$coef






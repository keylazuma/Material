# XS-0223: MODELOS DE REGRESI?N APLICADOS
# II SEMESTRE 2020

# LABORATORIO No.4
# PREDICTORES CATEGORICOS 

setwd("~/Teletrabajo/regresion 2022/Labs/Lab 4")  # CAMBIAR SEGUN SU CASO

base4=read.table("base4.csv", header=TRUE, sep=",")
names(base4)

##Se crea una nueva variable factor que es igual a "Sin casa" si la variable casapropia==0
##Es igual a "Casa propia" si casapropia==1

base4$casa=ifelse(base4$casapropia==0,"Sin casa","Casa propia")

#===============================================================================
# 1. Gráficos por categorías
  attach(base4)

###El objeto kol es para ponerle color a los puntos.  Azul corresponde al valor 2
###Y rojo al valor 4.

  kol=2*(casa=="Sin casa")+4*(casa=="Casa propia")

  plot(ingresoenmiles,gastoseguros,col=kol,xlab="Ingreso (en miles)",ylab="Gasto en seguros",pch=18)
  mod1a=lm(gastoseguros~ingresoenmiles,base4[casa=="Sin casa",])
  mod1b=lm(gastoseguros~ingresoenmiles,base4[casa=="Casa propia",])  
  abline(mod1a,col=2); abline(mod1b,col=4)
  legend(10000,150000,c("Sin casa","Casa propia"),bty="n",lty=1,col=c(2,4),cex = 0.5)
  summary(mod1a)$coef
  summary(mod1b)$coef
    
#===============================================================================
# 2. Modelo asumiendo pendientes iguales.

  mod2 = lm(gastoseguros ~ ingresoenmiles+casa)
  summary(mod2)$coef   #observe error est?ndar del coef de ingresoenmiles y comparelo con mod1 y mod2
  plot(ingresoenmiles,gastoseguros,col=kol,xlab="Ingreso (en miles de colones)",ylab="Gasto en seguros (en colones)",pch=18)
  beta=mod2$coef 
  abline(beta[1],beta[2],col=4)
  abline(beta[1]+beta[3],beta[2],col=2)
  legend(10000,200000,c("Sin casa","Casa propia"),bty="n",lty=1,col=c(2,4),cex=0.75)
  abline(mod1a,col=2,lty=2); abline(mod1b,col=4,lty=2)

###En líneas sólidas están las líneas paralelas
###En líneas con guiones están las líneas no paralelas
###Se nota que hay mejor ajuste con las líneas no paralelas
  
  mod2b = lm(gastoseguros ~ ingresoenmiles+casa)
  
  ###Tipo1
  ###unclass(Tipo1)
  casa=as.factor(casa)
  summary(mod2b)$coef

  ##contrasts(base$Tipo)
  ##contrasts(base$Tipo1)

  # Cambiar el nivel de referencia
  casa2=casa
  contrasts(casa2) = contr.treatment(levels(casa2),base=2)
  contrasts(casa2)
  contrasts(casa)
  mod2c = lm(gastoseguros ~ ingresoenmiles+casa2)
  summary(mod2c)$coef

  summary(mod2b)$coef

  ###Compare las intersecciones (Beta0) y las pendientes (Beta1)
  ###¿Qué se observa?
  
#===============================================================================
# 3. Modelo con interacción
  
  mod4 = lm(gastoseguros ~ ingresoenmiles+casa+ingresoenmiles:casa)
  summary(mod4)
	
  plot(ingresoenmiles,gastoseguros,col=kol,xlab="Ingreso (en miles de colones)",ylab="Gasto en seguros (en colones)",pch=18)
  beta=mod4$coef #compare el ee de tamano con el de mod3
  ###beta[1] es el intercepto y beta[2] es la pendiente cuando casa==0
  abline(beta[1],beta[2],col=4)
  ###beta[1]+beta[3] es el intercepto y beta[2]+beta[4] es la pendiente cuando casa==1
  abline(beta[1]+beta[3],beta[2]+beta[4],col=2)
  legend(10000,125000,c("Sin casa","Casa propia"),bty="n",lty=1,col=c(2,4),cex = 0.5)
  
#===============================================================================
# 4. Intervalo de confianza para la diferencia de respuesta promedio entre el tipo de hogar
  # En el modelo sin interacción
  beta=mod2$coef
  beta
  ee=summary(mod2)$coef[3,2]
  ee
  n=nrow(base4)
  p=length(beta)
  t=qt(0.975,n-p)
  beta[3]+c(-1,1)*t*ee
  
  # En el modelo con interacción (fijando el ingreso en 2 millones de colones, o sea, ingresoenmiles=2000)
  beta=mod4$coef
  beta
  n=nrow(base4)
  p=length(beta)
  (cme=anova(mod4)[p,3])
  (vb=cme * summary(mod4)$cov)
  sqrt(diag(vb)); summary(mod4)$coef
  vb
  x=2000
  ee=sqrt(vb[3,3]+x^2*vb[4,4]+2*x*vb[3,4])
  t=qt(0.975,n-p)
  (beta[3]-x*beta[4])
  (beta[3]-x*beta[4])+c(-1,1)*t*ee
  rm(list=ls())
  
#===============================================================================  
# 5. Modelo con variable con más de dos categorías.
  # Educacion - 1: Primaria incompleta
  #             2: Secundaria
  #             3: Universitaria
  
  
  
  
  
  rm(list=ls())
  
  base10=read.table("base10.csv", header=TRUE, sep=",")
  
  detach(base4)
  attach(base10)
  
  kol=c(1,2,4)
  kol1=kol[1]*(educacion==1)+kol[2]*(educacion==2)+kol[3]*(educacion==3)
  
  plot(ingresoenmiles,gastoseguros,col=kol1,xlab="Ingreso (en miles)",ylab="Gasto en seguros",cex.axis=0.8)
  for(i in 1:3) abline(lm((gastoseguros)~ingresoenmiles,base10[educacion==i,]),col=kol[i])
  legend(1000,35000,c("PRIMARIA","SECUNDARIA","UNIVERSITARIA"),bty="n",col=kol,lty=1,cex=0.7)

#===============================================================================  
  # Modelo sin interacción
  educ=as.factor(educ)
  class(educ)
  contrasts(educ)
  
  mod9 = lm(gastoseguros ~ ingresoenmiles+educ)
  summary(mod9)            #educ es factor
  
  # Significancia de educ
  
  ###H0: Omega.minuscula ==> H0: educ no contribuye a la variabilidad de gasto en seguros. ===> H0: Beta_2=0 y Beta_3==0
  ###H1: Omega.mayusucula==> H1: educ s? contribuye a la variabilidad del gasto en seguros. ==> H1: Beta_2<>0 o Beta_3<>0
  
  mod9a = lm(gastoseguros ~ ingresoenmiles)
  anova(mod9a,mod9)
  anova(mod9)   # la l?nea de educ es equivalente al comando anterior
  
  beta=mod9$coef
  beta
  
  ###El objeto int va a tener los siguientes valores: 0, 
  ###el coeficiente de la variable dummy de secundaria
  ###y el coeficiente de la variable dummy de universitaria
  ###Los valores de int se le suman al beta0 para tener los 3 interceptos.
  int=c(0,beta[3:4])
  int
  plot(ingresoenmiles,gastoseguros,col=kol1,xlab="Ingreso",ylab="Gasto")
  for(i in 1:3) abline(beta[1]+int[i],beta[2],col=kol[i])
  legend(1000,40000,c("Primaria", "Secundaria", "Universitaria"),bty="n",col=kol,lty=1,cex=0.7)
  
#===============================================================================  
  # Modelo con interacción
  mod10 = lm(gastoseguros ~ ingresoenmiles*educ)
  summary(mod10)
  mod10a = lm(gastoseguros~ingresoenmiles+educ+ingresoenmiles:educ)
  summary(mod10a)   #idéntico al mod10
  anova(mod9,mod10) ###Prueba para ver si la interacción sirve: H0: Beta4=Beta5=0
  anova(mod10)   #más conveniente
    

  
  rm(list=ls())

  
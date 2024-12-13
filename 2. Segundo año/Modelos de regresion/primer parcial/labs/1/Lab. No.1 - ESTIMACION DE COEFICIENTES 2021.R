# XS-2130: MODELOS DE REGRESION APLICADOS
# II SEMESTRE 2022

# LABORATORIO No.1
# ESTIMACION DE COEFICIENTES

#===============================================================================
# 0. Inicial

  # Limpiar - es equivalente a usar el boton "Clear" en "Environment"
  rm(list=ls(all=T))
  
  # Establecer directorio de trabajo - no es necesario si se abrió el 
  # programa directamente
  setwd("~/Teletrabajo/regresion 2022/Labs/Lab 1")  # CAMBIAR SEGUN SU CASO
  getwd()

#===============================================================================
# 1. Leyendo datos

  # Crear un marco de datos
  base2=read.table("base2.csv", header=TRUE, sep=",")
  
  # Crear el marco y agrega nombres a las columnas
  names(base2)
  nrow(base2)
  ncol(base2)
  dim(base2)
  
  
  base2<-base2[,-2]
#===============================================================================
# 2. Salvando y cargando un archivo

  # El archivo se salva en el directorio de trabajo con formato de R.
  save(base2,file="base2.Rdata")
  
  # Volvemos a empezar sin nada y cargamos de nuevo el archivo salvado
  rm(list=ls(all=T))
  load("base2.Rdata")

#===============================================================================
# 3. Análisis descriptivo.

  # Graficar la Y vs la X
  attach(base2)
  par(mfrow=c(1,3))
  plot(gastoseguros~ingresoenmiles)  # usar xlab, ylab, pch, cex  para mejorar el gráfico
  plot(gastoseguros~ingresoenmiles, xlab="Ingreso (en miles de colones)", ylab="Gasto en seguros (en colones)", main="Grafico 1a")
  plot(gastoseguros~numvehiculos, xlab="Número de vehículos", ylab="Gasto en seguros (en colones)", main="Grafico 1b")

  library(car)
  scatterplot(ingresoenmiles,gastoseguros)
  scatterplotMatrix(base2)

  # Obtener la correlación entre dos variables.
  cor(base2)
  cor(gastoseguros,ingresoenmiles) 
	
  # Obtener la matriz de correlación completa.
  cor(base2[,-1]) 
 
  # Graficar las correlaciones
  ##install.packages("PerformanceAnalytics")
  library(PerformanceAnalytics)
  chart.Correlation(base2,method="pearson")

#===============================================================================
# 4. Estimando los coeficientes usando las matrices X y Y.

  # Combinar un vector de "unos" y los predictores para crear la matriz X.
  x = cbind(1,ingresoenmiles,numvehiculos)
  x

  # Construir XTX.
  xtx = t(x) %*% x
  xtx
  
  # Invertir la matriz.
  (xtxi = solve(xtx))
  
  
  # Verificar la relacion entre XTX y su inversa
  xtx %*% xtxi
  round(xtx %*% xtxi,5)

  # Estimar los coeficientes.
	coef = xtxi %*% t(x) %*% gastoseguros
	coef
	
#===============================================================================
# 5. Construyendo una función para estimar los coeficientes
  beta = function(x,y)  {
    x = cbind(1,x)
    xtxi = solve(t(x) %*% x)
    coef = xtxi %*% t(x) %*% y
    print(coef)
    }
  
  # Estimar los coeficientes usando la funci?n
  x=cbind(ingresoenmiles,numvehiculos)
  beta(x,gastoseguros)
  
  # Estimar el coeficiente usando s?lo ingresoenmiles
  beta(ingresoenmiles,gastoseguros)
  b=beta(ingresoenmiles,gastoseguros)
  
#===============================================================================
# 6. Usando las funciones programadas en R
  
  # Especificar el modelo con varias variables
  mod = lm(gastoseguros ~ ingresoenmiles+numvehiculos)
  summary(mod)

  # Obtener el vector de coeficientes
  b=mod$coef
  mod$residuals
  mod$model
  
  # Especificar el modelo solo con ingresoenmiles
  mod1 = lm(gastoseguros~ingresoenmiles)
  b1=mod1$coef

  b1
  
  cbind(b[1:2],b1)

  # Agregar la línea de regresión en un gráfico
  par(mfrow=c(1,1))
  plot(gastoseguros~ingresoenmiles, col=3)
  abline(b1[1],b1[2],col="red")
  library(lattice)
  xyplot(gastoseguros~ingresoenmiles)
  
#===============================================================================
#7. Valores ajustados

  # Obtener un valor ajustado 
  predict(mod1, data.frame(ingresoenmiles=3143.63))  
  
  # Obtener un valor ajustado en el modelo con las dos variables
  predict(mod, data.frame(ingresoenmiles=3143.63,numvehiculos=2))
  gastoseguros[1]
  
  # Obtener el vector de valores ajustados
  predict(mod) 
  fitted(mod)
  mod$fitted.values

#===============================================================================
# Coeficientes estandarizados
# Se usan para que las interpretaciones sean comparables

  # 3. Coeficientes estandarizados
  
  base2
  
  
  ##Se calculan la matriz de correlaciones entre las 2 variables independientes.
  ##Se muestran 2 formas de extraer esta matriz de correlaci?n
  
  
  
  rxx=cor(base2[,c(2,3)])
  rxx
  
  rxx2=cor(base2[,-1])
  rxx2
  
  ##Se estima el vector de correlaciones entre Y y cada una de las 2 Xs
  
  cor(base2)
  rxy=cor(base2)[c(2,3),1]
  rxy
  
  beta.s=solve(rxx) %*% rxy
  
  beta.s
  
  
  
  mod.st = lm(scale(gastoseguros) ~ scale(ingresoenmiles)+scale(numvehiculos))
  summary(mod.st)

  # Comparar los vectores de coeficientes
  mod.st$coef
  cbind(mod$coef,mod.st$coef)
  cbind(mod$coef,round(mod.st$coef,5))

  
  
    
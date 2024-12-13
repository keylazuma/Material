###EJEMPLO DE BOOTSTRAP PASO A PASO CON R.

##Supongamos que tenemos una muestra de 5 personas que dan informaci??n sobre su ingreso mensual en miles de colones.

##Amaya: 343
##Bruno: 282
##Cleo:  588
##Dora:  837
##Ema:   275



##Se ingresan los datos para tenerlos en un dataframe

nombre<-c("Amaya","Bruno","Cleo","Dora","Ema")
ingreso<-c(343,282,588,837,275)

ingmensual<-data.frame(nombre,ingreso)

ingmensual

###En econom??a, un indicador de desigualdad econ??mica es la Variancia del logaritmo del ingreso.

###Primero se va a establecer una funci??n para dicho indicador.

varlog=function(ing) {
  
  var(  log(ing)  )
    
}


varlog(ingmensual$ingreso)



###Dado que esta es una muestra, se quiere saber el error est??ndar e intervalos de confianza para este indicador.
###Y existe la posibilidad de que no se conozca bien la distribuci??n muestral de este indicador.


###Por eso se usa bootstrap

###La t??cnica de bootstrap consiste en seleccionar una gran cantidad de muestras con reemplazo
###de la muestra original, tratando de "simular" la distribuci??n muestral del estimador.

###La desviaci??n est??ndar de esta distribuci??n muestral simulada es una estimaci??n del error est??ndar
###de la estimaci??n, porque la desviaci??n est??ndar de una distribuci??n muestral es un error est??ndar.

###Los percentiles 2.5 y 97.5 se podr??an considerar como los l??mites inferior y superior de 
###los intervalos de confianza de la estimaci??n, porque entre estos dos valores est?? el 95%
###de las estimaciones simuladas con bootstrap.

###Para efectos ilustrativos, vamos a seleccionar 6 remuestras con reemplazo de la base de datos original

###Para ello, utilizaremos la l??gica del indicador de posici??n de R.

###Con la siguiente funci??n, se crear??n 30 n??meros (6 remuestras de 5 n??meros)

seq(1:5)

set.seed(898)   ###Esta semilla es para que generemos los mismos valores

posicion<-sample(seq(1:5),replace=TRUE,size=5)
posicion

##ESta posicion se aplica a los datos.

ingmensual
ingmensual[posicion,]


###Noten como se repite al menos un caso.


###Para ilustrar, ahora se seleccionan 6 remuestras.

almacen.nombres=matrix(rep(as.character(NA),5*6),nrow=5)   ###5*6 porque son 6 remuestras de tama??o 5,
almacen.ingresos=matrix(rep(NA,5*6),nrow = 5)  ### como la muestra original

for (i in 1:6) {
  
  posicion<-sample(seq(1:5),replace=TRUE,size=5)
  almacen.nombres[,i]=as.character(ingmensual[posicion,1])
  almacen.ingresos[,i]=ingmensual[posicion,2]
  
}


###Cada columna de almacen.nombres y almacen.ingresos es una remuestra.

almacen.nombres   ###Note como se repite los nombres en cada columna

almacen.ingresos  ##Y como se repiten los ingresos, y c/ingreso corresponde a su nombre respectivo.


###A cada columna se le puede aplicar la funcion varlog programada antes
###Con el comando apply

(resumen.varlog<-apply(almacen.ingresos,2,FUN=varlog))

##resumen.varlog tiene una versi??n muy reducida de la dist. muestral simulada

###El error estandar de bootstrap ser??a la desviaci??n est??ndar de resumen.varlog

sd(resumen.varlog)

##El sesgo de bootstrap es la diferencia entre la media de resumen.varlog
##y la estimaci??n original

mean(resumen.varlog)-varlog(ingmensual$ingreso)
varlog(ingmensual$ingreso)
mean(resumen.varlog)

###En un procedimiento de bootstrap el ciclo se hace muchas veces, por ejemplo 10000.

almacen.nombres=matrix(rep(as.character(NA),5*10000),nrow=5)   ###5*10000 porque son 10000 remuestras de tama??o 5,
almacen.ingresos=matrix(rep(NA,5*10000),nrow = 5)  ### como la muestra original

for (i in 1:10000) {
  
  posicion<-sample(seq(1:5),replace=TRUE,size=5)
  almacen.nombres[,i]=as.character(ingmensual[posicion,1])
  almacen.ingresos[,i]=ingmensual[posicion,2]
  
}

resumen.varlog<-apply(almacen.ingresos,2,FUN=varlog)

hist(resumen.varlog)

mean(resumen.varlog)-varlog(ingmensual$ingreso)
varlog(ingmensual$ingreso)
mean(resumen.varlog)


###El histograma de resumen.varlog representa la distribuci??n muestral simulada.

###Noten como el histograma no parece normal.  
###La distribuci??n muestral no tiene una forma clara.

###Adem??s, si se calculan los percentiles 2.5 y 97.5, se encuentra un intervalo de confianza del 95%
###de tipo no param??trico.

varlog(ingmensual$ingreso)
quantile(resumen.varlog,c(0.025,0.975))





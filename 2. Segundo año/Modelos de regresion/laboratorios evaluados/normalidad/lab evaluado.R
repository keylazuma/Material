library(readxl)
BASE_COLEGIOS <- read_excel("C:/Users/Usuario/Downloads/BASE_COLEGIOS.xlsx")
attach(BASE_COLEGIOS)


BASE_COLEGIOS$porc_aprob_9 = as.numeric(BASE_COLEGIOS$porc_aprob_9)
BASE_COLEGIOS$porc_aprob_10 = as.numeric(BASE_COLEGIOS$porc_aprob_10)
BASE_COLEGIOS$porc_aprob_11 = as.numeric(BASE_COLEGIOS$porc_aprob_11)
BASE_COLEGIOS$porc_reprob_9 = as.numeric(BASE_COLEGIOS$porc_reprob_9)
BASE_COLEGIOS$porc_reprob_10 = as.numeric(BASE_COLEGIOS$porc_reprob_10)
BASE_COLEGIOS$porc_reprob_11 = as.numeric(BASE_COLEGIOS$porc_reprob_11)


lnporc_reprob_9 <- log(porc_reprob_9 + 1)

modelo <- lm(porc_aprob_9~lnporc_reprob_9+mat_noveno)




almacen.ynorm <- rep(NA,1000)
almacen.yt <- rep(NA,1000)
almacen.ycauchy <- rep(NA,1000)
almacen.yuniforme <- rep(NA,1000)

almacen.ygama1 <- rep(NA,1000)
almacen.ygama2 <- rep(NA,1000)
almacen.ygama3 <- rep(NA,1000)
almacen.ygama4 <- rep(NA,1000)

almacen.rechazo.ynorm <- rep(NA,1000)
almacen.rechazo.yt <- rep(NA,1000)
almacen.rechazo.ycauchy <- rep(NA,1000)
almacen.rechazo.yuniforme <- rep(NA,1000)

almacen.rechazo.ygama1 <- rep(NA,1000)
almacen.rechazo.ygama2 <- rep(NA,1000)
almacen.rechazo.ygama3 <- rep(NA,1000)
almacen.rechazo.ygama4 <- rep(NA,1000)

for (i in 1:1000){
  errornormal <- rnorm(947,0,9.039676)
  errort <- rt(947,2)
  errort <- scale(errort)*9.039676
  errorcauchy <- rcauchy(947,0,2)
  errorcauchy <- scale(errorcauchy)*9.039676
  erroruniforme  <- runif(947,-2,2)
  erroruniforme <-scale(erroruniforme)*9.039676
  
  
  errorgama1 <-rgamma(947,3,4)
  errorgama1 <- scale(errorgama1)*9.039676
  errorgama2 <- rgamma(947,2,5)
  errorgama2 <- scale(errorgama2)*9.039676
  errorgama3 <- rgamma(947,7,4)
  errorgama3 <- scale(errorgama3)*9.039676
  errorgama4 <- rgamma(947,5,6)
  errorgama4 <- scale(errorgama4)*9.039676
  
  
  ynorm <- 96.457312   - 1.576504*lnporc_reprob_9 - 0.016750*mat_noveno + errornormal
  yt <- 96.457312   - 1.576504*lnporc_reprob_9 - 0.016750*mat_noveno + errort
  ycauchy <- 96.457312   - 1.576504*lnporc_reprob_9 - 0.016750*mat_noveno + errorcauchy
  yuniforme <- 96.457312   - 1.576504*lnporc_reprob_9 - 0.016750*mat_noveno + erroruniforme
  
  ygama1 <- 96.457312   - 1.576504*lnporc_reprob_9 - 0.016750*mat_noveno + errorgama1
  ygama2 <- 96.457312   - 1.576504*lnporc_reprob_9 - 0.016750*mat_noveno + errorgama2
  ygama3 <- 96.457312   - 1.576504*lnporc_reprob_9 - 0.016750*mat_noveno + errorgama3
  ygama4 <- 96.457312   - 1.576504*lnporc_reprob_9 - 0.016750*mat_noveno + errorgama4
  
  
  modelonormal <- lm(ynorm ~ lnporc_reprob_9+mat_noveno)
  modelot <- lm(yt ~ lnporc_reprob_9+mat_noveno)
  modelocauchy <- lm(ycauchy ~ lnporc_reprob_9+mat_noveno)  
  modelouniforme <- lm(yuniforme ~ lnporc_reprob_9+mat_noveno)

  modelogama1 <- lm(ygama1 ~ lnporc_reprob_9+mat_noveno)
  modelogama2 <- lm(ygama2 ~ lnporc_reprob_9+mat_noveno)
  modelogama3 <- lm(ygama3 ~ lnporc_reprob_9+mat_noveno)
  modelogama4 <- lm(ygama4 ~ lnporc_reprob_9+mat_noveno)
  
  almacen.rechazo.ynorm[i] <- 1*(summary(modelonormal)$coef[2,4]<0.05)
  almacen.rechazo.yt[i] <- 1*(summary(modelot)$coef[2,4]<0.05)
  almacen.rechazo.ycauchy[i] <- 1*(summary(modelocauchy)$coef[2,4]<0.05)
  almacen.rechazo.yuniforme[i] <- 1*(summary(modelouniforme)$coef[2,4]<0.05)
  
  almacen.rechazo.ygama1 <- 1*(summary(modelogama1)$coef[2,4]<0.05)
  almacen.rechazo.ygama2 <- 1*(summary(modelogama2)$coef[2,4]<0.05)
  almacen.rechazo.ygama3 <- 1*(summary(modelogama3)$coef[2,4]<0.05)
  almacen.rechazo.ygama4 <- 1*(summary(modelogama4)$coef[2,4]<0.05)
  

  
  almacen.ynorm[i] <- 1*((confint(modelonormal)[2,1]<(-1.55906)) & (confint(modelonormal)[2,2]>(-1.55906))) 
  almacen.yt[i] <- 1*((confint(modelot)[2,1]<(-1.55906)) & (confint(modelot)[2,2]>(-1.55906))) 
  almacen.ycauchy[i] <- 1*((confint(modelocauchy)[2,1]<(-1.55906)) & (confint(modelocauchy)[2,2]>(-1.55906))) 
  almacen.yuniforme[i] <- 1*((confint(modelouniforme)[2,1]<(-1.55906)) & (confint(modelouniforme)[2,2]>(-1.55906))) 

  almacen.ygama1[i] <- 1*((confint(modelogama1)[2,1]<(-1.55906)) & (confint(modelogama1)[2,2]>(-1.55906))) 
  almacen.ygama2[i] <- 1*((confint(modelogama2)[2,1]<(-1.55906)) & (confint(modelogama2)[2,2]>(-1.55906))) 
  almacen.ygama3[i] <- 1*((confint(modelogama3)[2,1]<(-1.55906)) & (confint(modelogama3)[2,2]>(-1.55906))) 
  almacen.ygama4[i] <- 1*((confint(modelogama4)[2,1]<(-1.55906)) & (confint(modelogama4)[2,2]>(-1.55906))) 

}

#Potencias rechazo beta 1
mean(almacen.rechazo.ynorm)
mean(almacen.rechazo.yt)
mean(almacen.rechazo.ycauchy)
mean(almacen.rechazo.yuniforme)

mean(almacen.rechazo.ygama1)
mean(almacen.rechazo.ygama2)
mean(almacen.rechazo.ygama3)
mean(almacen.rechazo.ygama4)

#Potencias intervalo de confianza
mean(almacen.ynorm)
mean(almacen.yt)
mean(almacen.ycauchy)
mean(almacen.yuniforme)

mean(almacen.ygama1)
mean(almacen.ygama2)
mean(almacen.ygama3)
mean(almacen.ygama4)

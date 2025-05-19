################################################################################
# Regresión lineal simple ejemplo medidores sangre
# Autor: Jose Antonio Perusquia Cortes
# Afil : Facultad de Ciencias - UNAM
# Curso : Modelos no Paramétricos y de Regresión
################################################################################

################################################################################
# Librerías (se tienen que instalar)
library(ggplot2)          # Version 3.5.1
library(ggthemes)         # Version 5.0.0
library(nortest)          # Version 1.0-4
library(ellipse)          # Version 0.5.0
library(car)              # Version 3.1-2
library(lmtest)           # Version 0.9-40
library(MASS)             # Version 7.3-60
library(here)             # Version 1.0.1
library(GGally)           # Version 2.2.1
library(MPV)              # Version 1.64
library(dplyr)            # Version 1.1.4
################################################################################

################################################################################
# Importar funciones extras para grafica y analizar residuales
source(here('graficasRegresion.R'))
################################################################################

################################################################################
# Caso real de medidores de sangre
datos=read.table(here('../Datos/medidores.txt'),header = T)

# Se grafican los datos
p=ggplot(data=datos,aes(x=M2,y=M1))+
  geom_point()+
  theme_minimal()
p

# Se ajusta el modelo básico
res=lm(M1~M2,datos)
summary(res)

x = seq(min(datos$M2),max(datos$M2),by=.1)
y_hat=res$coefficients[1]+res$coefficients[2]*x
ajuste=data.frame(x,y_hat)

p+geom_line(data=ajuste,aes(x=x,y=y_hat),col='red')

# Análisis de residuales

# Normalidad (histograma, qqplot y pruebas)
hist_res(res$residuals)
qqnorm_res(res$residuals)

lillie.test(res$residuals)
ad.test(res$residuals)
cvm.test(res$residuals)

# Varianza constante gráfica y prueba
res_vs_yhat(res$residuals,res$fitted.values)
ncvTest(res)

# No correlación
dwtest(res)
################################################################################

################################################################################
# Transformación de Box-Cox
bc = boxcox(datos$M1 ~ datos$M2)
lambda = bc$x[which.max(bc$y)];lambda
M1T=((datos$M1^lambda)-1)/lambda

datosT=data.frame(M1T,datos$M2)
names(datosT)=c('M1T','M2')

# TransM1 against M2 
p=ggplot(datosT,aes(x=M2,y=M1T))+
  geom_point(col='black',size=1)+
  labs(x='M2',y='Transformed M1')+
  theme_minimal()
p

# Se ajusta el modelo básico
res=lm(M1T~M2,datosT)
summary(res)

x = seq(min(datosT$M2),max(datosT$M2),by=.1)
y_hat=res$coefficients[1]+res$coefficients[2]*x
ajuste=data.frame(x,y_hat)

p+geom_line(data=ajuste,aes(x=x,y=y_hat),col='red')

# Análisis de residuales

# Normalidad de los residuales (histograma, qqplot y pruebas)
hist_res(res$residuals)
qqnorm_res(res$residuals)

lillie.test(res$residuals)
ad.test(res$residuals)
cvm.test(res$residuals)

# Varianza constante gráfica y prueba
res_vs_yhat(res$residuals,res$fitted.values)
ncvTest(res)

# No correlación
dwtest(res)
################################################################################

################################################################################
# Regresión polinomial

# Se ajusta un modelo polinomial de orden 3
res=lm(datosT$M1T~poly(datosT$M2,3,raw=T),datosT)
summary(res)

x = seq(min(datosT$M2),max(datosT$M2),by=.1)
y_hat=res$coefficients[1]+res$coefficients[2]*x+
  res$coefficients[3]*x^2+res$coefficients[4]*x^3
ajuste=data.frame(x,y_hat)

p+geom_line(data=ajuste,aes(x=x,y=y_hat),col='red')

# Análisis de residuales

# Normalidad de los residuales (histograma, qqplot y pruebas)
hist_res(res$residuals)
qqnorm_res(res$residuals)

lillie.test(res$residuals)
ad.test(res$residuals)
cvm.test(res$residuals)

# Varianza constante gráfica y prueba
res_vs_yhat(res$residuals,res$fitted.values)
ncvTest(res)

# No correlación
dwtest(res)

# Intervalos para la respuesta media al 95%
MeanRespCI=predict(res,data.frame(datosT$M2),interval="confidence",level=.95)

plot_mean_pred(datosT$M2,datosT$M1T,datosT$M2,MeanRespCI[,1],
               MeanRespCI[,2],MeanRespCI[,3],xlab='M2',ylab='M1 transformado',
               Title='Intervalo de la respuesta media al 95%',
               col_pred='skyblue2',
               col_pred_int='darkblue')

# Intervalos de predicción al 95%
MeanRespPred=predict(res,data.frame(datosT$STC),interval="prediction",level=.95)

plot_mean_pred(datosT$M2,datosT$M1T,datosT$M2,MeanRespPred[,1],
               MeanRespPred[,2],MeanRespPred[,3],xlab='M2',
               ylab='M1 transformado',
               Title='Intervalo de predicción al 95%',
               col_pred='skyblue2',
               col_pred_int='darkblue')

# Intervalo de confianza para la respuesta media en los datos
# originales
MeanRespOriginalCI=(MeanRespCI*lambda+1)^(1/lambda)

plot_mean_pred(datos$M2,datos$M1,datos$M2,MeanRespOriginalCI[,1],
               MeanRespOriginalCI[,2],MeanRespOriginalCI[,3],xlab='M2',
               ylab='M1',
               Title='Intervalo de la respuesta media al 95%',
               col_pred='skyblue2',
               col_pred_int='darkblue')

# Intervalo de predicción al 95% para datos originales
MeanRespOriginalPred=(MeanRespPred*lambda+1)^(1/lambda)

plot_mean_pred(datos$M2,datos$M1,datos$M2,MeanRespOriginalPred[,1],
               MeanRespOriginalPred[,2],MeanRespOriginalPred[,3],
               xlab='M2',
               ylab='M1',
               Title='Intervalo de predicción al 95%',
               col_pred='skyblue2',
               col_pred_int='darkblue')
################################################################################




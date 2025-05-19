################################################################################
# Regresión lineal simple ejemplo sintético
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
# Ejemplo con datos simulados para el modelo y=beta0+beta1*x+error
beta0 = .2
beta1 = .3
sigma = .5

# Se obtienen los residuales normales
set.seed(314159)
errores = rnorm(30,sd=sigma)

# La variable predictora son los enteros del 1 al 30
x = c(1:30)

# La variable respuesta
y = beta0+beta1*x+errores;y

# Se genera data frame para guardar los datos y poder graficarlos
df = data.frame(x,y)

# Se grafican los puntos
p=ggplot(data=df,aes(x=x,y=y))+
  geom_point()+
  labs(x='',y='')+
  theme_minimal()
p

# Se añade la recta teórica
p=p+geom_abline(slope=beta1,intercept = beta0,col='red')
p
################################################################################

################################################################################
# Estimación

# Matriz de diseño
X=cbind(rep(1,length(x)),x);X

# Estimadores de los coeficientes
H = solve((t(X)%*%X))%*%t(X)
beta_hat = H%*%y;beta_hat

# Graficamos la recta estimada
y_hat=beta_hat[1]+beta_hat[2]*x^2
p=p+geom_abline(slope=beta_hat[2],intercept = beta_hat[1],col='skyblue')
p

# Residuales
y_hat = X%*%beta_hat
e = y-y_hat

# Estimador de la varianza
SSres = t(e)%*%e;SSres
MSres = SSres/(length(y)-2);MSres
sqrt(MSres)

# Error estándar de los coeficientes de la regresión
Sxx = sum((x-mean(x))^2);Sxx

se_beta1 = sqrt(MSres/Sxx);se_beta1
se_beta0 = sqrt(MSres*((1/length(y))+((mean(x)^2)/Sxx)));se_beta0

# Estadísticos t's para H0: beta_i=0 vs H1: beta_i !=0
t0 = beta_hat[1]/se_beta0;t0
t1 = beta_hat[2]/se_beta1;t1

# Cuantil de la distribución t para probar a un nivel alpha del 5%
# las hipótesis nulas mencionadas arriba. En este ejemplo no se rechaza
# que beta0 sea cero y sí rechazamos para beta1
qt(.975,length(y)-2)

# Prueba F
SSr = sum(((y_hat-mean(y))^2));SSr
F0 = SSr/MSres;F0

# Comparamos contra el cuantil de una distribución F de 1,n-2 grados de lib
# en este caso rechazamos la hipótesis nula de que beta1=0
qf(.95,1,length(y)-2)
################################################################################

################################################################################
# Otras propiedaes 

# 1. La suma de residuales es cero
sum(e)

# 2. La suma de los valores observados y los ajustados es igual
sum(y)
sum(y_hat)

# 3. La regresión pasa por el centroide
centroide = data.frame(x=mean(x),y=mean(y))
p+geom_point(data=centroide,aes(x=x,y=y),col='blue',shape=8,size=4)

# 4. La suma ponderada de las variables predictoras es cero
sum(x*e)

# 5. La suma ponderada de los valores ajustados es cero
sum(y_hat*e)
################################################################################

################################################################################
# Intervalos de confianza

# El intervalo de beta1 no contiene al cero
beta1_U = beta_hat[2]+se_beta1*qt(.975,length(y)-2);beta1_U
beta1_L = beta_hat[2]-se_beta1*qt(.975,length(y)-2);beta1_L

# El intervalo de beta0 contiene al cero
beta0_U = beta_hat[1]+se_beta0*qt(.975,length(y)-2);beta0_U
beta0_L = beta_hat[1]-se_beta0*qt(.975,length(y)-2);beta0_L

# Intervalo para sigma^2
sigma_U = SSres/qchisq(.025,length(y)-2);sigma_U
sigma_L = SSres/qchisq(.975,length(y)-2);sigma_L

# R^2
SSt = sum((y-mean(y))^2);SSt
R2 = 1-(SSres/SSt);R2
################################################################################

################################################################################
# En R la función lm ajusta el modelo de regresión
res=lm(y~x,data=df)
summary(res)
plot(res)

# Región de confianza
reg_con = as.data.frame(ellipse::ellipse(res,npoints=100))
names(reg_con)=c('beta0','beta1')

ggplot(reg_con,aes(x=beta0,y=beta1))+
  geom_point(size=.1)+
  theme_minimal()+
  labs(x=expression(beta[0]),y=expression(beta[1]))
################################################################################

################################################################################
# Estimación de la respuesta media
x0=seq(min(x),max(x),by=.1)
mu_hat = beta_hat[1]+x0*beta_hat[2]
mu_hat_U = mu_hat + qt(.975,length(y)-2)*sqrt(MSres*((1/length(y))+
                                                       (((x0-mean(x))^2)/Sxx)))
mu_hat_L = mu_hat - qt(.975,length(y)-2)*sqrt(MSres*((1/length(y))+
                                                       (((x0-mean(x))^2)/Sxx)))

plot_mean_pred(x,y,x0,mu_hat,mu_hat_U,mu_hat_L,
                        Title='Intervalo de la respuesta media al 95%',
                        col_pred='skyblue2',
                        col_pred_int='darkblue')
  
# Nuevas predicciones
x0=seq(min(x),max(x),by=.1)
y0_hat = beta_hat[1]+x0*beta_hat[2]
y0_hat_U = mu_hat + qt(.995,length(y)-2)*sqrt(MSres*(1+(1/length(y))+
                                                       (((x0-mean(x))^2)/Sxx)))
y0_hat_L = mu_hat - qt(.995,length(y)-2)*sqrt(MSres*(1+(1/length(y))+
                                                       (((x0-mean(x))^2)/Sxx)))

plot_mean_pred(x,y,x0,y0_hat,y0_hat_U,y0_hat_L,
               Title='Intervalo de predicción al 95%',
               col_pred='skyblue2',
               col_pred_int='darkblue')
################################################################################

################################################################################
# Ejemplo de como la R^2 no mide la adecuación del modelo
x=c(1:30)
y=(x^2)-(8*x)+10
df=data.frame(x,y)

p=ggplot(data=df,aes(x=x,y=y))+
  geom_point()+
  theme_minimal()
p

# Ajustamos la regresión lineal
res=lm(y~x,df)
summary(res)

# Se genera una R^2 de 0.8985 lo cual pareciera una buena medida
# pero claramente el ajuste no es bueno
p+geom_abline(slope=res$coefficients[2],
              intercept = res$coefficients[1],col='skyblue')

# Regresión lineal con únicamente el término cuadrático
res1=lm(y~I(x^2),df)
summary(res1)

df1=data.frame('x'=x,'y'=res1$coefficients[1]+res1$coefficients[2]*x^2)
p+geom_line(data=df1,aes(x=x,y=y),col='skyblue')
##########################################################################
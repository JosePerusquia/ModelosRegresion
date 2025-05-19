################################################################################
# Regresión lineal con variable categórica
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
# Regresión con variables categóricas
vinos=read.table(here('../Datos/calidadRioja.txt'),header = T)
vinos$Region=as.factor(vinos$Region)

# Graficamos por pares las variables continuas
ggpairs(vinos[,-5],upper = list(continuous = "points"),
        lower=list(continuous = wrap("cor")))

# Agregar la variable categórica de la región del vino
ggpairs(vinos, aes(color = Region, alpha = 0.5),
        upper = list(continuous = "points"),
        lower=list(continuous = wrap("cor")))

# Cambio en la pendiente para las diferentes regiones
# tomando el modelo que explica la calidad en términos del sabor
p=ggplot(data=vinos,aes(x=Sabor,y=Calidad,col=Region))+
  geom_point(show.legend = F)+
  theme_minimal()
p

# Regresión lineal de calidad en términos del sabor
res=lm(Calidad~Sabor,data=vinos)
summary(res)

# Recta ajustada
sop=seq(min(vinos$Sabor),max(vinos$Sabor),by=.01)
yhat=res$coefficients[1]+res$coefficients[2]*sop
df_adj = data.frame(sop,yhat)

ggplot(data=vinos,aes(x=Sabor,y=Calidad))+
  geom_point()+
  theme_minimal()+
  geom_line(data=df_adj,aes(x=sop,y=yhat),col='red')

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


# Regresión lineal de calidad en términos del sabor diferenciando
# por región
res=lm(Calidad~Sabor+Region,data=vinos)
summary(res)

# Recta región 1
yhat1=res$coefficients[1]+res$coefficients[2]*sop
col1=rep(1,411)

yhat2=res$coefficients[1]+res$coefficients[2]*sop+res$coefficients[3]
col2=rep(2,411)

yhat3=res$coefficients[1]+res$coefficients[2]*sop+res$coefficients[4]
col3=rep(3,411)

sops=rep(sop,3)
yhats=c(yhat1,yhat2,yhat3)
cols=c(col1,col2,col3)

df_res=data.frame(sops,yhats,Region=as.factor(cols))
p+geom_line(data=df_res,aes(x=sops,y=yhats),show.legend = F)

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
################################################################################
# Multicolinealidad
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
# Multicolinealidad
data(cement)

# Graficamos por pares
ggpairs(cement)

# VIF
X=cement%>%dplyr::select(x1,x2,x3,x4)
W=scale(X)
Z=t(W)%*%W/12
C=solve(Z);C
VIFs = diag(C);VIFs

res=lm(y~x1+x2+x3+x4,data=cement)
vif(res)

# Número de condición
evd = eigen(Z)
evd$values

kappa = max(evd$values)/min(evd$values);kappa
kappa_j = evd$values/min(evd$values);kappa_j

# Descomposición en valores singulares
SVD = svd(W/sqrt(12))
SVD$d

eta_j = max(SVD$d)/SVD$d;eta_j

# Posible solución eliminar variables

# Selección por segmentos (coincide también con los otros métodos)
res= lm(y~1,cement)
step.model = stepAIC(res, direction = "both",
                     scope=list(upper=~x1+x2+x3+x4))

# VIF
res=lm(y~x1+x2+x4,data=cement)
vif(res)

# Número de condición
X=cement%>%dplyr::select(x1,x2,x4)
W=scale(X)
Z=t(W)%*%W/12;Z

evd = eigen(Z)
evd$values

kappa = max(evd$values)/min(evd$values);kappa
kappa_j = evd$values/min(evd$values);kappa_j

# Descomposición en valores singulares
SVD = svd(W/sqrt(12))
SVD$d

eta_j = max(SVD$d)/SVD$d;eta_j

# Modelo
res=lm(y~x1+x2+x4,cement)
summary(res) # x4 no es significativa

res = lm(y~x1+x2,cement)
vif(res)
summary(res)

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
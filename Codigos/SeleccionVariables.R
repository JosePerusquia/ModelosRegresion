################################################################################
# Selección de variables
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
# Selección de variables 
data(swiss)

# Graficamos por pares
ggpairs(swiss)

# Selección hacia adelante no considera Examination
res= lm(Fertility~1,swiss)
summary(res)

step.model = stepAIC(res, direction = "forward",
                     scope=list(upper=~Agriculture+Education+Examination+
                                  Catholic+Infant.Mortality))

# Selección hacia atrás (coincide con el de selección hacia adelante)
res= lm(Fertility~.,swiss)
summary(res)

step.model = stepAIC(res, direction = "backward")

# Selección por segmentos (coincide también con los otros métodos)
res = lm(Fertility~1,swiss)
step.model = stepAIC(res, direction = "both",
                     scope=list(upper=~Agriculture+Education+Examination+
                                  Catholic+Infant.Mortality))

# El mejor modelo
res= lm(Fertility~Education+Catholic+Infant.Mortality+Agriculture,swiss)
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

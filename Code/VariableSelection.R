####################################################################
# Variable selection
# Author: Jose Antonio Perusquia Cortes
# Afil : Facultad de Ciencias - UNAM
# Module : Regression analysis
####################################################################

####################################################################
# Required libraries
library(ggplot2)          # Version 4.0.2
library(ggthemes)         # Version 5.2.0
library(nortest)          # Version 1.0-4
library(car)              # Version 3.1-5
library(lmtest)           # Version 0.9-40
library(MASS)             # Version 7.3-65
library(here)             # Version 1.0.2
library(GGally)           # Version 2.4.0
####################################################################

####################################################################
# Source auxiliary plotting functions
source(here('Code/plotsLM.R'))
####################################################################

####################################################################
# Swiss data
# Objective is to fit a linear regression that explains
# the fertility in terms of the rest of covariables
data(swiss)
ggpairs(swiss)
####################################################################

####################################################################
# Forward variable selection

# Base model
mod_fw = lm(Fertility~1,swiss)
summary(mod_fw)

# Forward method does not consider examination
step_fw = stepAIC(mod_fw, direction = "forward",
                     scope=list(upper=~Agriculture+
                                  Education+
                                  Examination+
                                  Catholic+
                                  Infant.Mortality))
####################################################################

####################################################################
# Backward variable selection

# Base model
mod_bw = lm(Fertility~.,swiss)
summary(mod_bw)

# Backward method does not consider examination as well
step_bw = stepAIC(mod_bw, direction = "backward")
####################################################################

####################################################################
# Stepwise variable selection

# Base model
mod_sw = lm(Fertility~1,swiss)

# Stepwise model coincides with both backward and forward
step_sw = stepAIC(mod_sw, direction = "both",
                     scope=list(upper=~Agriculture+
                                  Education+
                                  Examination+
                                  Catholic+
                                  Infant.Mortality))

# Compare the BIC
BIC(mod_bw,mod_fw,step_sw)
####################################################################

####################################################################
# The final model
mod_fin = lm(Fertility~Education+Catholic+
             Infant.Mortality+Agriculture,swiss)
summary(mod_fin)

# VIFs show no multicollinearity 
vif(mod_fin)

# Normality of residuals
hist_res(mod_fin,bins = 10)
qqnorm_res(mod_fin)
ad.test(mod_fin$residuals)
shapiro.test(mod_fin$residuals)

# Constant variance of residuals
res_vs_yhat(mod_fin)
ncvTest(mod_fin)

# No correlation of residuals
dwtest(mod_fin)
bgtest(mod_fin)
####################################################################
###########################################################
# Multicollinearity
# Author: Jose Antonio Perusquia Cortes
# Afil : Facultad de Ciencias - UNAM
# Module : Linear regression models
###########################################################

###########################################################
# Required libraries
library(ggplot2)          # Version 4.0.2
library(ggthemes)         # Version 5.2.0
library(nortest)          # Version 1.0-4
library(ellipse)          # Version 0.5.0
library(car)              # Version 3.1-5
library(lmtest)           # Version 0.9-40
library(MASS)             # Version 7.3-65
library(here)             # Version 1.0.2
library(GGally)           # Version 2.4.0
library(MPV)              # Version 2.0
library(dplyr)            # Version 1.2.1
###########################################################

###########################################################
# Source auxiliary plotting functions
source(here('plotsLM.R'))
###########################################################

###########################################################
# Cement data
data(cement)

# Pair-wise scatterplots
ggpairs(cement)
###########################################################

###########################################################
# Multicollinearity analysis

# Design matrix and scaled data matrix
X = cement%>%dplyr::select(x1,x2,x3,x4)
X_scaled = scale(X)

# VIFs obtained manually and with vif function
Z = cor(X)
C = solve(Z)
vifs = diag(C);vifs

mod_ols = lm(y~x1+x2+x3+x4,data=cement)
vif(mod_ols)

# Condition number of the correlation matrix
evd = eigen(Z)
evd$values
kappa = max(evd$values)/min(evd$values);kappa
kappa_j = evd$values/min(evd$values);kappa_j

# Condition number of the data matrix
svd_data = svd(X_scaled)
svd_data$d
eta_j = max(svd_data$d)/svd_data$d;eta_j
eta = max(eta_j);eta
###########################################################

###########################################################
# Variable selection may reduce multicollinearity
# by removing redundant predictors
res_vs = lm(y~1,cement)
step.model = stepAIC(res_vs, direction = "both",
                     scope=list(upper=~x1+x2+x3+x4))

# VIF
mod_var_sel=lm(y~x1+x2+x4,data=cement)
summary(mod_var_sel)
vif(mod_var_sel)

# Condition number of the correlation matrix
X_var_sel=cement%>%dplyr::select(x1,x2,x4)
X_var_sel_scaled = scale(X_var_sel)

Z_var_sel = cor(X_var_sel)
evd = eigen(Z_var_sel)
evd$values

kappa = max(evd$values)/min(evd$values);kappa
kappa_j = evd$values/min(evd$values);kappa_j

# Condition number of the data matrix
svd_data_var_sel = svd(X_var_sel_scaled)
svd_data_var_sel$d
eta_j = max(svd_data_var_sel$d)/svd_data_var_sel$d;eta_j
eta = max(eta_j);eta
###########################################################

###########################################################
# Model without x4 which is not significant
mod_var_sel2 = lm(y~x1+x2,cement)
anova(mod_var_sel,mod_var_sel2)
summary(mod_var_sel2)

# VIF and further diagnostics show no multicollinearity
vif(mod_var_sel2)

# Condition number of the correlation matrix
X_var_sel=cement%>%dplyr::select(x1,x2)
X_var_sel_scaled = scale(X_var_sel)

Z_var_sel = cor(X_var_sel)
evd = eigen(Z_var_sel)
evd$values

kappa = max(evd$values)/min(evd$values);kappa
kappa_j = evd$values/min(evd$values);kappa_j

# Condition number of the data matrix
svd_data_var_sel = svd(X_var_sel_scaled)
svd_data_var_sel$d
eta_j = max(svd_data_var_sel$d)/svd_data_var_sel$d;eta_j
eta = max(eta_j);eta

# Residuals analysis on this model

# Normality
hist_res(mod_var_sel2,bins=3)
qqnorm_res(mod_var_sel2)
ad.test(mod_var_sel2$residuals)
cvm.test(mod_var_sel2$residuals)
shapiro.test(mod_var_sel2$residuals)

# constant variance 
res_vs_yhat(mod_var_sel2)
ncvTest(mod_var_sel2)

# Autocorrelation
dwtest(mod_var_sel2)
bgtest(mod_var_sel2)
###########################################################
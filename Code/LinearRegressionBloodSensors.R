####################################################################
# Linear regression model for blood sensors
# Author : Jose Antonio Perusquia Cortes
# Afil   : Facultad de Ciencias - UNAM
# Module : Linear regression models
####################################################################

####################################################################
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
library(dplyr)            # Version 1.2.0
####################################################################

####################################################################
# Source auxiliary plotting functions
source(here('Code/plotsLM.R'))
####################################################################

####################################################################
# Blood sensors
data=read.table(here('Data/medidores.txt'),header = T)

# Scatterplot
p_r=ggplot(data=data,aes(x=M2,y=M1))+
    geom_point()+
    theme_minimal()
print(p_r)

# Fit a simple linear regression model
mod_r=lm(M1~M2,data)
summary(mod_r)

# Fitted model
x = seq(min(data$M2),max(data$M2),by=.1)
y_hat_r=mod_r$coefficients[1]+mod_r$coefficients[2]*x
fit_r=data.frame(x,y_hat_r)
p_r+geom_line(data=fit_r,aes(x=x,y=y_hat_r),col='red')

# Residuals analysis

# Normality plots and tests show that the residuals are not normal
hist_res(mod_r,bins=10)
qqnorm_res(mod_r)
ad.test(mod_r$residuals)
shapiro.test(mod_r$residuals)

# Constant variance plot and test show non-constant variance
res_vs_yhat(mod_r)
ncvTest(mod_r)

# Correlation test shows no correlation
dwtest(mod_r)
bgtest(mod_r)
####################################################################

####################################################################
# Box-Cox transformation
bc = boxcox(data$M1 ~ data$M2)
lambda = bc$x[which.max(bc$y)];lambda

# Transform the variables
if (abs(lambda) < 1e-6) {
  data$M1T <- log(data$M1)
} else {
  data$M1T <- (data$M1^lambda - 1) / lambda
}

# Scatterplot on transformed measurements  
p_bc=ggplot(data,aes(x=M2,y=M1T))+
     geom_point(col='black',size=1)+
     labs(x='M2',y='Transformed M1')+
     theme_minimal()
print(p_bc)

# Fit a simple linear regression
mod_bc=lm(M1T~M2,data)
summary(mod_bc)

# Fitted model
y_hat_bc=mod_bc$coefficients[1]+mod_bc$coefficients[2]*x
fit_bc=data.frame(x,y_hat_bc)
p_bc+geom_line(data=fit_bc,aes(x=x,y=y_hat_bc),col='red')

# Residuals analysis

# Normality graphs and tests
hist_res(mod_bc,bins=10)
qqnorm_res(mod_bc)
ad.test(mod_bc$residuals)
shapiro.test(mod_bc$residuals)

# Constant variance plot and graph
res_vs_yhat(mod_bc)
ncvTest(mod_bc)

# Correlation test
dwtest(mod_bc)
bgtest(mod_bc)
####################################################################

####################################################################
# Polynomial regression

# We fit a third order polynomial regression model
mod_pol=lm(data$M1T~poly(data$M2,3,raw=T),data)
summary(mod_pol)

# Fitted model
y_hat_pol=mod_pol$coefficients[1]+mod_pol$coefficients[2]*x+
  mod_pol$coefficients[3]*x^2+mod_pol$coefficients[4]*x^3
fit_pol=data.frame(x,y_hat_pol)
p_bc+geom_line(data=fit_pol,aes(x=x,y=y_hat_pol),col='red')

# Residuals analysis

# Normality
hist_res(mod_pol,bins = 12)
qqnorm_res(mod_pol)
ad.test(mod_pol$residuals)
shapiro.test(mod_pol$residuals)

# Constant variance 
res_vs_yhat(mod_pol)
ncvTest(mod_pol)

# Correlation
dwtest(mod_pol)
bgtest(mod_pol)

# Compare models
AIC(mod_r, mod_bc, mod_pol)
BIC(mod_r, mod_bc, mod_pol)

# Mean response and 95% CI
newdata = data.frame(M2 = data$M2)
MeanRespCI=predict(mod_pol,newdata,interval="confidence",
                   level=.95)
plot_mean_pred(data$M2,data$M1T,data$M2,MeanRespCI[,1],
               MeanRespCI[,2],MeanRespCI[,3],xlab='M2',
               ylab='Transformed M1',
               title='Mean response and 95% CI',
               col_pred='skyblue2',
               col_pred_int='darkblue')

# Prediction and 95% CI
MeanRespPred=predict(mod_pol,newdata,interval="prediction",
                     level=.95)
plot_mean_pred(data$M2,data$M1T,data$M2,MeanRespPred[,1],
               MeanRespPred[,2],MeanRespPred[,3],xlab='M2',
               ylab='Transformed M1',
               title='Prediction and 95% CI',
               col_pred='skyblue2',
               col_pred_int='darkblue')

# Mean response and 95% CI on original scale
MeanRespOriginalCI=(MeanRespCI*lambda+1)^(1/lambda)
plot_mean_pred(data$M2,data$M1,data$M2,
               MeanRespOriginalCI[,1],
               MeanRespOriginalCI[,2],
               MeanRespOriginalCI[,3],
               xlab='M2',
               ylab='M1',
               title='Mean response and 95% CI',
               col_pred='skyblue2',
               col_pred_int='darkblue')

# Prediction and 95% CI in original scale
MeanRespOriginalPred=(MeanRespPred*lambda+1)^(1/lambda)
plot_mean_pred(data$M2,data$M1,data$M2,
               MeanRespOriginalPred[,1],
               MeanRespOriginalPred[,2],
               MeanRespOriginalPred[,3],
               xlab='M2',
               ylab='M1',
               title='Prediction and 95% CI',
               col_pred='skyblue2',
               col_pred_int='darkblue')
####################################################################


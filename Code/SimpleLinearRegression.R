################################################################################
# Example for a simple linear regression model
# Author: Jose Antonio Perusquia Cortes
# Afil : Facultad de Ciencias - UNAM
# Module : Linear regression models
################################################################################

################################################################################
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
################################################################################

################################################################################
# Source auxiliary plotting functions
source(here('plotsLM.R'))
################################################################################

################################################################################
# Synthetic example with simulated data using as parameters
beta0 = .2
beta1 = .3
sigma = .5

# Sample some gaussian errors
set.seed(314159)
errors = rnorm(30,sd=sigma)

# Observed data
n = 30
k = 2
x = c(1:n)
y = beta0+beta1*x+errors
df_obs = data.frame(x,y)

# Scatterplot
p=ggplot(data=df_obs,aes(x=x,y=y))+
  geom_point()+
  labs(x='',y='')+
  theme_minimal()
print(p)

# Add the theoretical line
p=p+geom_abline(slope=beta1,intercept = beta0,col='red')
print(p)
################################################################################

################################################################################
# Least squares estimates

# Design matrix
X=cbind(rep(1,length(x)),x)

# Estimation of the coefficients
A = solve((t(X)%*%X))%*%t(X)
beta_hat = A%*%y;beta_hat

# Plot the estimated line and the centroid
y_hat=beta_hat[1]+beta_hat[2]*x
p=p+geom_abline(slope=beta_hat[2],intercept = beta_hat[1],
                col='skyblue')
print(p)

x_bar = mean(x)
y_bar = mean(y)
centroid = data.frame(x=x_bar,y=y_bar)
p+geom_point(data=centroid,aes(x=x,y=y),col='blue',shape=8,size=4)

# Residuals
y_hat = X%*%beta_hat
e = y-y_hat

# Variance estimate
SSres = t(e)%*%e;SSres
MSres = SSres/(n-k);MSres
sqrt(MSres)

# Standard error for the beta coefficients
Sxx = sum((x-x_bar)^2)
se_beta1 = sqrt(MSres/Sxx);se_beta1
se_beta0 = sqrt(MSres*((1/n)+((x_bar^2)/Sxx)));se_beta0

# test statistics for indiviual test H0:beta_i=0 vs H1:beta_i!=0
t0 = beta_hat[1]/se_beta0;t0
t1 = beta_hat[2]/se_beta1;t1

# Reference value for specified alpha significance.
# In this exmaple we do not reject that beta0 is cero whereas
# for beta1 we reject the null hypothesis
alpha = 0.05
confLev = 1-alpha/2
ref_val = qt(confLev,n-k)

# F test
SSr = sum(((y_hat-y_bar)^2))
F0 = (SSr / (k - 1)) / MSres

# Reference value against F distribution of k-1 and n-k degrees
# of freedom
confLevF = 1-alpha
ref_valF = qf(confLevF,k-1,n-k)

# R^2
SSt = sum((y-y_bar)^2)
R2 = 1-(SSres/SSt)

# We corroborate everything with the lm function
mod = lm(y~x,data=df_obs)
summary(mod)
################################################################################

################################################################################
# Confidence intervals

# The CI for beta1 does not contain zero
beta1_U = beta_hat[2]+se_beta1*qt(confLev,n-k);beta1_U
beta1_L = beta_hat[2]-se_beta1*qt(confLev,n-k);beta1_L

# The CI for beta0 contains zero
beta0_U = beta_hat[1]+se_beta0*qt(confLev,n-k);beta0_U
beta0_L = beta_hat[1]-se_beta0*qt(confLev,n-k);beta0_L

# CI for sigma squared
sigma_U = SSres/qchisq(1-confLev,n-k);sigma_U
sigma_L = SSres/qchisq(confLev,n-k);sigma_L

# Joint confidence region for the beta coefficients compared to
# the cartesian product of individual CI's
conReg = as.data.frame(ellipse::ellipse(mod,npoints=100))
names(conReg)=c('beta0','beta1')

ggplot(conReg,aes(x=beta0,y=beta1))+
  geom_path()+
  theme_minimal()+
  labs(x=expression(beta[0]),y=expression(beta[1]))+
  geom_segment(aes(x=beta0_L,xend=beta0_L,
                   y=beta1_L,yend=beta1_U),col='darkred')+
  geom_segment(aes(x=beta0_U,xend=beta0_U,
                   y=beta1_L,yend=beta1_U),col='darkred')+
  geom_segment(aes(x=beta0_L,xend=beta0_U,
                   y=beta1_L,yend=beta1_L),col='darkred')+
  geom_segment(aes(x=beta0_L,xend=beta0_U,
                   y=beta1_U,yend=beta1_U),col='darkred')
################################################################################

################################################################################
# Mean response estimate and CI
x0=seq(min(x),max(x),by=.1)
mu_hat = beta_hat[1]+x0*beta_hat[2]
sigma_hat = sqrt(MSres*((1/n)+(((x0-x_bar)^2)/Sxx)))

mu_hat_U = mu_hat + qt(confLev,n-k)*sigma_hat
mu_hat_L = mu_hat - qt(confLev,n-k)*sigma_hat

plot_mean_pred(x,y,x0,mu_hat,mu_hat_U,mu_hat_L,
               title='Mean response and 95% CI',
               col_pred='skyblue2',col_pred_int='darkblue')
  
# Predictions
y0_hat = beta_hat[1]+x0*beta_hat[2]
sigma_hat = sqrt(MSres*(1+(1/n)+(((x0-x_bar)^2)/Sxx)))

y0_hat_U = y0_hat + qt(confLev,n-k)*sigma_hat
y0_hat_L = y0_hat - qt(confLev,n-k)*sigma_hat

plot_mean_pred(x,y,x0,y0_hat,y0_hat_U,y0_hat_L,
               title='Prediction and 95% CI',
               col_pred='skyblue2',col_pred_int='darkblue')
################################################################################
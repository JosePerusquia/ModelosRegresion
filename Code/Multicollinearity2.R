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
library(fsemipar)         # Version 1.1.1
###########################################################

###########################################################
# Source auxiliary plotting functions
source(here('plotsLM.R'))
###########################################################

###########################################################
# Tecator data
data(Tecator)
X = Tecator$absor.spectra
y = Tecator$fat

# Split into training and test data
set.seed(123)
n = nrow(X)
train_idx = sample(1:n, size = round(0.7*n))
train = X[train_idx, ]
test  = X[-train_idx, ]

# Scale training set
train_means = apply(train,2,mean)
train_sds   = apply(train,2,sd)

train_scaled = as.data.frame(scale(train,
                                   center = train_means,
                                   scale  = train_sds))

# Scale test set using training statistics
test_scaled = as.data.frame(scale(test,
                                  center = train_means,
                                  scale  = train_sds))

# Complete datasets
train_scaled$y=y[train_idx]
test_scaled$y=y[-train_idx]
###########################################################

###########################################################
# Linear regression model on training data
mod_tec_train = lm(y ~ ., data = train_scaled)
summary(mod_tec_train)

# VIFs
vif(mod_tec_train)

# Condition number of correlation matrix
Z = cor(train_scaled[,-1])
evd = eigen(Z)
evd$values
kappa = max(evd$values)/min(evd$values);kappa

# Condition number of data matrix
svd_data = svd(train_scaled[,-1])
svd_data$d
eta = max(svd_data$d) / min(svd_data$d);eta

# Mean squared error
pred_tec = predict(mod_tec_train, newdata = test_scaled)
mse_tec = mean((test_scaled$y - pred_tec)^2);mse_tec
###########################################################

###########################################################
# PCR solves instability by discarding low-variance 
# directions, not merely by orthogonalizing predictors.

# PCA on training data
pca_train = prcomp(train_scaled[,-101],
                   center = FALSE,
                   scale. = FALSE)

# Linear regression using all the components (it will yield
# the same results as the original data)
X_pca = pca_train$x
tecator_pca = data.frame(y=train_scaled$y,X_pca)
mod_tec_pca = lm(y~.,data=tecator_pca)
summary(mod_tec_pca)

# VIFs show no multicollinearity since principal components
# are uncorrelated
vif(mod_tec_pca)

# Condition number of correlation matrix
Z = cor(X_pca)
evd = eigen(Z)
evd$values
kappa = max(evd$values)/min(evd$values);kappa

# Condition number of data matrix
svd_data_pca = svd(scale(X_pca))
svd_data_pca$d
eta = max(svd_data_pca$d) / min(svd_data_pca$d);eta

# Mean squared error using dimensionality reduction techniques
max_k = 20
test_mse = numeric(max_k)

for(k in 1:max_k){
  
  # Training PCs
  Z_train = pca_train$x[,1:k]
  Z_train = data.frame(y=train_scaled$y,Z_train)
  colnames(Z_train)[2:(k+1)] = colnames(tecator_pca)[2:(k+1)]
  
  # Fit regression
  fit = lm(y~.,data=Z_train)
  
  # Project test data
  Z_test = scale(test_scaled[,-101],
                 center = pca_train$center,
                 scale  = pca_train$scale) %*% pca_train$rotation[,1:k]
  Z_test = as.data.frame(Z_test)
  colnames(Z_test) = colnames(tecator_pca)[2:(k+1)]
  
  pred = predict(fit, newdata = Z_test)
  
  test_mse[k] = mean((test_scaled$y - pred)^2)
}

df = data.frame(k = 1:max_k, MSE = test_mse)

ggplot(df, aes(x = k, y = MSE)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = mse_tec, col = "red") +
  labs(title = "MSE: PCR vs OLS",
       x = "Number of components (k)",
       y = "Test MSE") +
  theme_minimal()
###########################################################
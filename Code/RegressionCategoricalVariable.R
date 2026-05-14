##########################################################
# Linear regression with categorical variables
# Author: Jose Antonio Perusquia Cortes
# Afil : Facultad de Ciencias - UNAM
# Module : Regression analysis
##########################################################

##########################################################
# Required libraries
library(ggplot2)          # Version 4.0.2
library(ggthemes)         # Version 5.2.0
library(nortest)          # Version 1.0-4
library(car)              # Version 3.1-5
library(lmtest)           # Version 0.9-40
library(MASS)             # Version 7.3-65
library(here)             # Version 1.0.2
library(GGally)           # Version 2.4.0
##########################################################

##########################################################
# Source auxiliary plotting functions
source(here('Code/plotsLM.R'))
##########################################################

##########################################################
# Wines data
wines=read.table(here('Data/calidadRioja.txt'),
                 header = T)
wines$Region=as.factor(wines$Region)

# Plot the scatterplots for numeric variables
ggpairs(wines[,-5],upper = list(continuous = "points"),
        lower=list(continuous = wrap("cor")))

# Color the plot with the region variable
ggpairs(wines, aes(color = Region, alpha = 0.5),
        upper = list(continuous = "points"),
        lower=list(continuous = wrap("cor")))
##########################################################

##########################################################
# Linear regression model that explains the quality
# of the wines in terms of the taste 
p=ggplot(data=wines,aes(x=Sabor,y=Calidad))+
    geom_point()+
    theme_minimal()
print(p)

# Linear regression model
mod=lm(Calidad~Sabor,data=wines)
summary(mod)

# Fitted model
newdat = expand.grid(Sabor = seq(min(wines$Sabor),
                max(wines$Sabor),length.out = 100))
newdat$fit = predict(mod, newdata = newdat)

ggplot(wines, aes(Sabor, Calidad)) +
  geom_point() +
  geom_line(data = newdat,aes(y = fit),col='red')+
  theme_minimal()

# Normality of residuals
hist_res(mod,bins=10)
qqnorm_res(mod)
ad.test(mod$residuals)
shapiro.test(mod$residuals)

# Constant variance
res_vs_yhat(mod)
ncvTest(mod)

# No correlation
dwtest(mod)
bgtest(mod)
##########################################################

##########################################################
# Correlation might be caused my model misspecification
# in this case the region might be an important variable
p1 = ggplot(data=wines,aes(x=Sabor,y=Calidad,col=Region))+
      geom_point()+
      theme_minimal()
print(p1)

# Linear regression with region included by considering 
# just different intercepts
mod_region = lm(Calidad~Sabor+Region,data=wines)
summary(mod_region)

# Plot the lines
newdat = expand.grid(Sabor = seq(min(wines$Sabor),
               max(wines$Sabor),length.out = 100),
               Region = levels(wines$Region))
newdat$fit = predict(mod_region, newdata = newdat)

ggplot(wines, aes(Sabor, Calidad, color = Region)) +
  geom_point() +
  geom_line(data = newdat,aes(y = fit),show.legend = F)+
  theme_minimal()

# Normality of residuals
hist_res(mod_region,bins=5)
qqnorm_res(mod_region)
ad.test(mod_region$residuals)
shapiro.test(mod_region$residuals)

# Constant variance
res_vs_yhat(mod_region)
ncvTest(mod_region)

# No correlation
dwtest(mod_region)
bgtest(mod_region)
##########################################################

##########################################################
# Model with common intercept but region-specific slopes
mod_region2 = lm(Calidad~Sabor+Sabor:Region,data=wines)
summary(mod_region2)

# Plot the lines
newdat = expand.grid(Sabor = seq(min(wines$Sabor),
              max(wines$Sabor),length.out = 100),
              Region = levels(wines$Region))

newdat$fit = predict(mod_region2, newdata = newdat)

ggplot(wines,aes(Sabor, Calidad, color = Region)) +
  geom_point() +
  geom_line(data = newdat,aes(y = fit),show.legend = F)+
  theme_minimal()

# Normality of residuals
hist_res(mod_region2,bins=10)
qqnorm_res(mod_region2)
ad.test(mod_region2$residuals)
shapiro.test(mod_region2$residuals)

# Constant variance
res_vs_yhat(mod_region2)
ncvTest(mod_region2)

# No correlation
dwtest(mod_region2)
bgtest(mod_region2)
##########################################################

##########################################################
# Model with region-specific regression lines
mod_region3 = lm(Calidad~Sabor*Region,data=wines)
summary(mod_region3)

# Plot the lines
newdat = expand.grid(Sabor = seq(min(wines$Sabor),
                max(wines$Sabor),length.out = 100),
                     Region = levels(wines$Region))

newdat$fit = predict(mod_region3, newdata = newdat)

ggplot(wines,aes(Sabor, Calidad, color = Region)) +
  geom_point() +
  geom_line(data = newdat,aes(y = fit),show.legend = F)+
  theme_minimal()

# Normality of residuals
hist_res(mod_region3,bins=10)
qqnorm_res(mod_region3)
ad.test(mod_region3$residuals)
shapiro.test(mod_region3$residuals)

# Constant variance
res_vs_yhat(mod_region3)
ncvTest(mod_region3)

# No correlation
dwtest(mod_region3)
bgtest(mod_region3)

# Compare models with:
# 1. common line
# 2. different intercepts
# 3. different slopes
# 4. different intercepts and slopes
AIC(mod,mod_region,mod_region2,mod_region3)
BIC(mod,mod_region,mod_region2,mod_region3)
##########################################################
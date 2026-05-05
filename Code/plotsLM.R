####################################################################
# Auxiliary plots for linear regression models
# Author: Jose Antonio Perusquia Cortes
# Afil : Facultad de Ciencias - UNAM
# Module : Linear regression models
####################################################################

####################################################################
# Histogram of model residuals
# Args:
#   model  : fitted lm object
#   fill   : fill color of the bars
#   color  : border color of the bars
#   method : "sturges" for Sturges rule, otherwise ggplot default binning
#   xlab   : x-axis label
#   ylab   : y-axis label
#   title  : plot title
hist_res=function(model, fill = "skyblue2",
                  color = "black", bins=30,
                  xlab = "", ylab = "", title = "") {
  
  df = data.frame(residuals = residuals(model))
  
  
  
  p = ggplot(df, aes(x = residuals)) +
    geom_histogram(bins = bins,
      colour = color,
      fill = fill
    ) +
    labs(x = xlab, y = ylab, title = title) +
    theme_minimal()
  
  p
}
####################################################################

####################################################################
# Normal Q-Q plot of residuals
# Args:
#   model : fitted lm object
#   xlab  : x-axis label
#   ylab  : y-axis label
#   title : plot title

qqnorm_res = function(model, xlab = "", ylab = "", title = "") {
  
  df = data.frame(x = residuals(model))
  
  p = ggplot(df, aes(sample = x)) +
    geom_qq() +
    geom_qq_line(color = "red") +
    labs(title = title, x = xlab, y = ylab) +
    theme_minimal()
  
  p
}
####################################################################

####################################################################
# Residuals vs fitted values plot
# Args:
#   model : fitted lm object
#   xlab  : x-axis label
#   ylab  : y-axis label
#   title : plot title

res_vs_yhat = function(model, xlab = "", ylab = "", title = "") {
  
  res = residuals(model)
  fit = fitted(model)
  
  df = data.frame(residuals = res, fitted = fit)
  
  p = ggplot(df, aes(x = fitted, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "red") +
    labs(x = xlab, y = ylab, title = title) +
    theme_minimal()
  
  p
}
####################################################################


####################################################################
# Scatter plot with fitted mean response and confidence interval
# Args:
#   x, y        : observed data
#   x0          : points where predictions are evaluated
#   mu_hat      : predicted mean response
#   mu_hat_U/L  : upper and lower confidence bounds
#   xlab, ylab  : axis labels
#   title       : plot title
#   col_pred    : color of fitted line
#   col_pred_int: color of confidence band
plot_mean_pred=function(x,y,x0,mu_hat,mu_hat_U,mu_hat_L,
                        xlab='',ylab='',title='',
                        col_pred='skyblue2',
                        col_pred_int='darkblue'){
  
  df=data.frame(x,y)
  df1=data.frame(x0,mu_hat,mu_hat_U,mu_hat_L)
  df1 <- df1[order(df1$x0), ]
  
  p=ggplot(data=df,aes(x=x,y=y))+
    geom_point()+
    geom_line(data=df1,aes(x=x0,y=mu_hat),col=col_pred)+
    geom_line(data=df1,aes(x=x0,y=mu_hat_U),col=col_pred_int)+
    geom_line(data=df1,aes(x=x0,y=mu_hat_L),col=col_pred_int)+
    labs(x=xlab,y=ylab,title=title)+
    theme_minimal()
  
  return(p)
  
}
####################################################################

################################################################################
# Gráficas para los modelos de regresión
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
# Histograma para los residuales recibe como parámetros
# 1. residuales - vector de residuales del modelo de regresión
# 2. col_fill - color para llenar las barras del histograma
# 3. col_border - el color del borde las barras del histograma
# 4. breaks - el tipo de breaks a utilizar, actualmente funciona
# con el método Sturges ('S') o el método original de ggplot ('O')
# 5. xlab - leyenda para el eje x
# 6. ylab - leyenda para el eje y
# 7. Title - título de la gráfica

hist_res=function(residuales,col_fill='skyblue2',
                  col_border='black',breaks='S',
                  xlab='',ylab='',Title=''){
  
  data=data.frame(residuales)
  
  # Si no hay breaks asignados se asignan los que se 
  # generan con la función hist de R a partir de Sturges
  if(breaks=='S'){
    p=ggplot(data=data,aes(x=residuales))+
      geom_histogram(breaks=hist(residuales,plot=F)$breaks,
                     col=col_border,
                     fill=col_fill)+
      labs(x=xlab,y=ylab,title=Title)+
      theme_minimal()
  }else{
    p=ggplot(data=data,aes(x=residuales))+
      geom_histogram(col=col_border,
                     fill=col_fill)+
      labs(x=xlab,y=ylab,title=Title)+
      theme_minimal()
  }
  
  plot(p)
  return(p)
  
}
################################################################################

################################################################################
# qqnorm para los residuales, recibe como parámetros
# 1. residuales - vector de residuales del modelo de regresión
# 2. xlab - leyenda para el eje x
# 3. ylab - leyenda para el eje y
# 4. Title - título de la gráfica

qqnorm_res=function(residuales,xlab='',ylab='',Title=''){
  
  emp_quantiles=quantile(residuales,
    probs=seq(0.01,.99,length.out = length(residuales)))
  
  teo_quantiles=qnorm(seq(.025,.975,
                          length.out = length(residuales)))
  
  x1=qnorm(.25)
  x2=qnorm(.75)
  y1=quantile(residuales,.25)
  y2=quantile(residuales,.75)
  
  m=(y2-y1)/(x2-x1)
  b=y1-m*x1
  
  quantiles=data.frame(emp_quantiles,teo_quantiles)
  names(quantiles)=c('Empirical','Theoretical')
  
  p=ggplot(quantiles,aes(x=Theoretical,y=Empirical))+
    geom_point(col='black',size=2)+
    geom_abline(intercept = b ,slope =m,col="red")+
    labs(title=Title,x=xlab,y=ylab)+
    theme_minimal()+
    theme(axis.title =element_text(size=8))
  
  plot(p)
  return(p)
}
################################################################################

################################################################################
# Residuales contra valores ajustados, recibe como parámetros
# 1. residuales - vector de residuales del modelo de regresión
# 2. ajustados - vector de valores ajustados
# 3. xlab - leyenda para el eje x
# 4. ylab - leyenda para el eje y
# 5. Title - título de la gráfica
res_vs_yhat=function(residuales,ajustados,xlab='',ylab='',
                     Title=''){
  
  res_df=data.frame(x=residuales,y=ajustados)
  
  p=ggplot(data=res_df,aes(x=y,y=x))+
    geom_point()+
    labs(x=xlab,y=ylab,title=Title)+
    geom_hline(yintercept=0,col='red')+
    theme_minimal()
  
  plot(p)
  return(p)
}
################################################################################


################################################################################
# Gráficas para la respuesta media o nuevas predicciones.
# 1. x - Variable regresora
# 2. y - Variable respuesta
# 3. x0 - Nuevos puntos para la respuesta media o predicciones
# 4. mu_hat - Respueta media estimada
# 5. mu_hat_U - Intervalo superior para respuesta media
# 6. mu_hat_L - Itervalo inferior para respuesta media
# 7. xlab - Leyenda del eje x
# 8. ylab - Leyenda del eje y
# 9. Title - título de la gráfica
# 10. col_pred - color para la respuesta media
# 11. col_pred_int - color para los intervalos
plot_mean_pred=function(x,y,x0,mu_hat,mu_hat_U,mu_hat_L,
                        xlab='',ylab='',Title='',
                        col_pred='skyblue2',
                        col_pred_int='darkblue'){
  
  df=data.frame(x,y)
  df1=data.frame(x0,mu_hat,mu_hat_U,mu_hat_L)
  
  p=ggplot(data=df,aes(x=x,y=y))+
    geom_point()+
    geom_line(data=df1,aes(x=x0,y=mu_hat),col=col_pred)+
    geom_line(data=df1,aes(x=x0,y=mu_hat_U),col=col_pred_int)+
    geom_line(data=df1,aes(x=x0,y=mu_hat_L),col=col_pred_int)+
    labs(x=xlab,y=ylab,title=Title)+
    theme_minimal()
  
  plot(p)
  return(p)
  
}
################################################################################

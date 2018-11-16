#1 Generar los valores de manera mensual

library(quantmod)
library(gridExtra)
library(ggplot2)
start <- as.Date("2000-01-01")
end <- as.Date("2018-08-01")

getSymbols("MSFT", src = "yahoo", from = start, to = end, periodicity = "monthly")
getSymbols("AAPL", src = "yahoo", from = start, to = end, periodicity = "monthly") 

#MODIFICAR EL NOMBRE DE LAS COLUMNAS PARA FACILITAR SU CALCULO

colnames(MSFT)[1] <- "Open"
colnames(MSFT)[2] <- "High"
colnames(MSFT)[3] <- "Low"
colnames(MSFT)[4] <- "Close"
colnames(MSFT)[5] <- "Volumen"
colnames(MSFT)[6] <- "Adjusted"

colnames(AAPL)[1] <- "Open"
colnames(AAPL)[2] <- "High"
colnames(AAPL)[3] <- "Low"
colnames(AAPL)[4] <- "Close"
colnames(AAPL)[5] <- "Volumen"
colnames(AAPL)[6] <- "Adjusted"

#2 

###############################
#Retorno

retorno <- function(data) {

  Adjusted<- data$Adjusted
  
  n <- length(Adjusted)
  
  log_Adjusted <- diff(log(Adjusted), lag=1)
  
  return(log_Adjusted)
}

retorno(MSFT)


#################################
#Graficar los retornos

plot(retorno(MSFT), col = "purple", lwd = 2, ylab = "Return",
     main = "Retornos")

##################################

Adjusted<- MSFT$Adjusted

n <- length(Adjusted)

log_Adjusted <- diff(log(Adjusted), lag=1)
log_Adjusted <- na.omit(log_Adjusted)

skewness = ((sum(log_Adjusted) - mean(log_Adjusted))^3)/length(log_Adjusted)/
  ((sum((log_Adjusted - mean(log_Adjusted))^2)/length(log_Adjusted)))^(3/2)

skewness

kurtosis = (sum((log_Adjusted - mean(log_Adjusted))^4)/length(log_Adjusted))/
  ((sum((log_Adjusted - mean(log_Adjusted))^2)/length(log_Adjusted)))^2


jb = n*((skewness/6)+((kurtosis-3)^2)/24)

jb

###################### FUNCIÓN ###########################

retorno <- function(data) {
  #Calculo del retorno sobre uno de los precios
  Adjusted<- data$Adjusted
  
  #Calcular el valor de n
  n <- length(Adjusted)
  #Calcular el retorno
  log_Adjusted <- diff(log(Adjusted), lag=1)
  log_Adjusted <- na.omit(log_Adjusted)
  #Mostrar el retorno
  print(log_Adjusted)
  
  #Grafico de los retornos
  grafico <- plot(log_Adjusted, col = "purple", lwd = 2, ylab = "Return",
       main = "Retornos")
  
  #Grafico de los retornos acumulados
  
  retorno_acumulado <- cumprod(1 + log_Adjusted)
  retorno_acumulado <- na.omit(retorno_acumulado)
  
  grafico2 <- plot(retorno_acumulado, col = "green", lwd = 2, ylab = "Return",
                  main = "Retornos Acumulados") 
  
  g <- list(grafico, grafico2)
  
  #Prueba JB
  
  log_Adjusted <- diff(log(Adjusted), lag=1)
  log_Adjusted <- na.omit(log_Adjusted)
  
  #Skewness
  skewness = ((sum(log_Adjusted) - mean(log_Adjusted))^3)/length(log_Adjusted)/
    ((sum((log_Adjusted - mean(log_Adjusted))^2)/length(log_Adjusted)))^(3/2)
  
  skewness
  
  #Kurtosis
  kurtosis = (sum((log_Adjusted - mean(log_Adjusted))^4)/length(log_Adjusted))/
    ((sum((log_Adjusted - mean(log_Adjusted))^2)/length(log_Adjusted)))^2
  
  #Jarque Bera
  jb = n*((skewness/6)+((kurtosis-3)^2)/24)
  
  print(jb)
  
  return(g)

}

retorno(AAPL)




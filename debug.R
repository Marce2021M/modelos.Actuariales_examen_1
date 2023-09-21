
# Cargamos la libreria
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)


# Supuestos principales
# 191654
# edad del asegurado
xEdad <- 16+30
#vigencia del seguro
n <- 19
#tasa de interes efectiva anual
itasaBase <- .05
#numero de primas anuales
mPrimas <- 4
# Sumas aseguradas
saMuerte <- 2000000
saSupervivencia <- 1000000

nCarteraAsegurados <- 100

# Leeremos la tabla de mortalidad

# Leemos la tabla de mortalidad
tablaMortalidad <- read_excel("Tabla_mortalidad.xlsx")

tablaMortalidad <- tablaMortalidad |> 
  mutate(dx= c(tablaMortalidad$lx[1:(length(tablaMortalidad$lx)-1)]-tablaMortalidad$lx[2:length(tablaMortalidad$lx)],NA))

qTablaMortalidad <- data.frame(px = numeric(length(tablaMortalidad$lx)-xEdad), kSuperv = numeric(length(tablaMortalidad$lx)-xEdad))


# Creamos la columna de supervivencia

for(i in (xEdad+1):length(tablaMortalidad$lx)){
  qTablaMortalidad$px[i-xEdad] <- tablaMortalidad$lx[i]/tablaMortalidad$lx[xEdad+1] 
  qTablaMortalidad$kSuperv[i-xEdad] <- i-xEdad
}

######-------------------------------------
#Gastos base

# Hacemos función de gastos extras

gastosExtrasPrimas <- function(anio){
  if(anio == 0){
    return(1000 + primaEmision*.4)
  }else if(anio %in% c(1,2)){
    return(500+.2*primaEmision)
  }else {
    return(100 + .05*primaEmision)
  }
}

gastosExtrasLiquidacion <- function(salida){
  if(salida == "muerte"){
    return(3000 + saMuerte*.003)
  }else{
    return(1000+saSupervivencia*.001)
}}

######-------------------------------------
#Regla de valores garantizados

# Hacemos función de valores garantizados

valoresGarantizados <- function(valorPoliza, anio){
  if(anio %in% c(1,2)){
    return(valorPoliza*.8)
  }else if (anio == 3){
    return(valorPoliza*.9)
  } else {
    return(0)
  }
}

######-------------------------------------
#Valores observados

valoresObservados_tasaInteres <- read_excel("Valores observados.xlsx", sheet="Observados Promedio", range="A3:B28")

valoresObservados_costosPromedios <- read_excel("Valores observados.xlsx", sheet="Observados Promedio", range="D3:G29")

valoresObservados_carteraObservados <- read_excel("Valores observados.xlsx", sheet="Cartera Observados", range="A1:C101")

# Funciones útiles para los siguientes cálculos

# Definición de más variables globales

v <- 1/(1+itasaBase)

# Cálculo de temporales

temporal <- function(edad, vigencia){
  resultado <- 0
  for (k in seq(0, vigencia-1, 1)){
    resultado <- resultado + v^(k+1)*(tablaMortalidad$dx[edad+k+1]/tablaMortalidad$lx[edad+1])
  }
  return(resultado)
}

anualidad <- function(edad, vigencia){
  resultado <- 0
  for (k in seq(0, vigencia-1, 1)){
    resultado <- resultado + v^(k)*(tablaMortalidad$lx[edad+k+1]/tablaMortalidad$lx[edad+1])
  }
  return(resultado)
}

dotal <- function(edad, vigencia){
  resultado <- v^(n)*tablaMortalidad$lx[xEdad+vigencia+1]/tablaMortalidad$lx[xEdad+1]
  return(resultado)
}

valorPolizaRecargado <- function(kTemp, primaRecargada){
  resultado <- 0
  if(kTemp == 1){
    resultado <- (dotal(xEdad,kTemp)^(-1))*(-1000+.6*primaRecargada-(3000+1.003*saMuerte)*temporal(xEdad, kTemp))
  } 
  else if(kTemp %in% c(2,3)){
    resultado <- (dotal(xEdad,kTemp)^(-1))*(-1000+.6*primaRecargada+(-500+.8*primaRecargada)*(anualidad(xEdad, kTemp)-1)-(3000+1.003*saMuerte)*temporal(xEdad, kTemp))
  }
  else if(kTemp >=4 && kTemp<=n){
    resultado <- (dotal(xEdad,kTemp)^(-1))*(-1000+.6*primaRecargada+(-500+.8*primaRecargada)*(anualidad(xEdad, 3)-1)+(-100+.95*primaRecargada)*anualidad(xEdad+3, mPrimas-3)*dotal(xEdad, 3)-(3000+1.003*saMuerte)*temporal(xEdad, kTemp))
  }
  else{
    resultado <- 0
  }
  return (resultado)
}

# Creamos la tabla de valores póliza asociados a la prima recargada

tablaValoresPolizaAsociadosRecargada <- data.frame(k = numeric(n+1), Vx = numeric(n+1))

# Llenamos la tabla

for (k in seq(0, n, 1)){
  tablaValoresPolizaAsociadosRecargada$Vx[k+1] <- valorPolizaRecargado(k, primaRecargada)
  tablaValoresPolizaAsociadosRecargada$k[k+1] <- k
}


iTasaEstimada <- .05
tablaMortalidadEstimada <- tablaMortalidad

gastoPropPrima <- function(kTemp){
  #Es el complemento del ejerccio por cómo está configurado
  if(kTemp==1){
    resultado <- .6
  } else if(kTemp %in% c(2,3)){
    resultado <- .8
  }else if(kTemp>=4){
    resultado <- .95
  }
  return(resultado)
}
gastoFijoPrima <- function(kTemp){
    if(kTemp==1){
    resultado <- 1000
  } else if(kTemp %in% c(2,3)){
    resultado <- 500
  }else if(kTemp>=4){
    resultado <- 100
  }
  return(resultado)
}

desembolsoProp <- function(salida="Muerte"){
  if(salida =="Muerte"){
    resultado <- 1.003
  } else {
    resultado <- 1.001
  }
  return(resultado)
}
desembolsoFijo <- function(salida="Muerte"){
  if(salida =="Muerte"){
    resultado <- 3000
  } else {
    resultado <- 1000
  }
  return(resultado)
}



flujos_utilidad <- function(kTemp){
  #ktemo solo está entre 1 a n
  if (kTemp %in% c(1,2,3,4)){
    ingreso <- (gastoPropPrima(kTemp)*primaRecargada - gastoFijoPrima(kTemp) + tablaValoresPolizaAsociadosRecargada$Vx[kTemp])*(1+iTasaEstimada)*tablaMortalidadEstimada$lx[xEdad+kTemp]} 
    else {
    ingreso <- tablaValoresPolizaAsociadosRecargada$Vx[kTemp]*(1+iTasaEstimada)*tablaMortalidadEstimada$lx[xEdad+kTemp]
    }
    egreso <-(desembolsoFijo()+desembolsoProp()*saMuerte)*tablaMortalidadEstimada$dx[xEdad+kTemp]
    reserva <- tablaValoresPolizaAsociadosRecargada$Vx[kTemp+1]*tablaMortalidadEstimada$lx[xEdad+kTemp+1]
    resultado <- 1000*(ingreso-egreso-reserva)/tablaMortalidadEstimada$lx[xEdad+1]
    return(resultado)

medidaVPN <- function(){
  v <- 1/(1+iTasaEstimada)
  resultado <- 0
  for (kTemp in seq(1,n,1)){
    resultado <- resultado + flujos_utilidad(kTemp)*v^(kTemp)
  }
  return(resultado)
}

medidaVPN()
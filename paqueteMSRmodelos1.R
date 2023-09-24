# Cargamos la libreria
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(truncnorm)
library(knitr)
library(kableExtra)

# Cargamos los datos

# LEEMOS LA TABLA DE MORTALIDAD
tablaMortalidad <- read_excel("Tabla_mortalidad.xlsx")

tablaMortalidad <- tablaMortalidad |> 
  mutate(dx= c(tablaMortalidad$lx[1:(length(tablaMortalidad$lx)-1)]-tablaMortalidad$lx[2:length(tablaMortalidad$lx)],NA))

#MODIFICAR O CHECAR PARA LA GRÁFICA DE SUPERVIVENCIA
qTablaMortalidadGraf <- data.frame(px = numeric(length(tablaMortalidad$lx)-xEdad), kSuperv = numeric(length(tablaMortalidad$lx)-xEdad))

for(i in (xEdad+1):length(tablaMortalidad$lx)){
  qTablaMortalidadGraf$px[i-xEdad] <- tablaMortalidad$lx[i]/tablaMortalidad$lx[xEdad+1] 
  qTablaMortalidadGraf$kSuperv[i-xEdad] <- i-xEdad-1
}

# FUNCIÓN DE GASTOS

funcPrimaCostosMSR <- function(kTemp, prima){
    #kTemp es >=0 y se considera año en curso anticipadamente
  if(kTemp==0){
    resultado <- .6*prima -1000
  } else if(kTemp %in% c(1,2)){
    resultado <- .8*prima - 500
    }
  else if(kTemp>=3 && kTemp<=mPrimas-1){
    resultado <- .95*prima - 100
  } else {
    resultado <- 0
  }
  return(resultado)
}

funcMuerteCostosMSR <- function(kTemp){
  #kTemp es >=1
  #Se da anticipadamente en el año en curso
  if(kTemp>=1 && kTemp<=n){
    resultado <- resultado <- 1.003*saMuerte + 3000
  } else {
    resultado <- 0
  }
 return(resultado)
}

indicadoraPrimaNeta <- function(kTemp){
  if(kTemp>=0 && kTemp<=mPrimas-1){
    resultado <- 1
  } else {
    resultado <- 0
  }
  return(resultado)
}

# Mortalidad

funcMortalidadMSRq <- function(kTemp){
  #kTemp es >=0
  #Es q_x+k
  resultado <- tablaMortalidad$dx[xEdad+kTemp+1]/tablaMortalidad$lx[xEdad+kTemp+1]
  return(resultado)
  
}

# FUNCION DE VALOR GARANTIZADO

valoresGarantizados <- function(valorPoliza, kTemp){
  #Estoy tomando kTemp de referencia para indicar el año en curso anticipadamente
  if(kTemp %in% c(1,2)){
    resultado <- valorPoliza*.8
  }else if (kTemp >= 3 && kTemp <= mPrimas-1){
    resultado <- valorPoliza*.9
  } else {
    resultado <- 0
  }
  return(resultado)
}

# FUNCIONES AUXILIARES DE TEMPORALES Y ANUALIDAD

temporal <- function(edad, vigencia, itasa=itasaBase){
  v <- 1/(1+itasa)  
  resultado <- 0
  if(vigencia==0){
    resultado <- 0
  } else{
  for (k in seq(0, vigencia-1, 1)){
    resultado <- resultado + v^(k+1)*(tablaMortalidad$dx[edad+k+1]/tablaMortalidad$lx[edad+1])
  }}
  return(resultado)
}

anualidad <- function(edad, vigencia, itasa=itasaBase){
    v <- 1/(1+itasa)
    resultado <- 0
    if(vigencia==0){
    resultado <- 0
  } else{
    for (k in seq(0, vigencia-1, 1)){
    resultado <- resultado + v^(k)*(tablaMortalidad$lx[edad+k+1]/tablaMortalidad$lx[edad+1])
  }}
  return(resultado)
}

dotal <- function(edad, vigencia, itasa=itasaBase){
    v <- 1/(1+itasa)
    resultado <- v^(vigencia)*tablaMortalidad$lx[xEdad+vigencia+1]/tablaMortalidad$lx[xEdad+1]
    return(resultado)
}

# FUNCIONES DE VALOR POLIZA

valorPoliza <- function(kTemp, prima, interes=itasaBase,  funcMortalidad=funcMortalidadMSRq ){
  if(kTemp>=1 && kTemp<=n){
    valorPolizaAnterior <- valorPoliza(kTemp-1,prima, interes, funcMortalidad)

    resultado <- ((valorPolizaAnterior+primaNeta*indicadoraPrimaNeta(kTemp-1))*(1+interes)-funcMortalidad(kTemp-1)*saMuerte)/(1-funcMortalidad(kTemp-1))
  }
  else{
    resultado <- 0
  }
  return(resultado)
}

valorPolizaRecargado <- function(kTemp, prima, interes=itasaBase, funcPrimaCostos=funcPrimaCostosMSR, funcMuerteCostos=funcMuerteCostosMSR,funcMortalidad=funcMortalidadMSRq ){
  if(kTemp>=1 && kTemp<=n){
    valorPolizaAnterior <- valorPolizaRecargado(kTemp-1,prima, interes, funcPrimaCostos, funcMuerteCostos,funcMortalidad)

    resultado <- ((valorPolizaAnterior+funcPrimaCostos(kTemp-1, prima))*(1+interes)-funcMortalidad(kTemp-1)*funcMuerteCostos(kTemp))/(1-funcMortalidad(kTemp-1))
  }
  else{
    resultado <- 0
  }
  return(resultado)
}

# CALCULOS DE PRIMAS

primaNeta <- (saMuerte*temporal(xEdad, n) + saSupervivencia*dotal(xEdad,n))/anualidad(xEdad, mPrimas)

primaRecargada <- ((3000+1.003*saMuerte)*temporal(xEdad, n) + (1000+1.001*saSupervivencia)*dotal(xEdad,n) + 1000 + 500*(anualidad(xEdad, 3)-1) + 100*anualidad(xEdad+3, mPrimas-3)*dotal(xEdad, 3))/(.6+.8*(anualidad(xEdad, 3)-1)+.95*anualidad(xEdad+3, mPrimas-3)*dotal(xEdad, 3))

# CÁLCULO DE FLUJOS 

flujos_utilidad <- function(kTemp, iTasa=iTasaEstimada, tabla_Mortalidad=tablaMortalidadEstimada){
  #ktemp solo está entre 1 a n
    ingreso <- (funcPrimaCostosMSR(kTemp -1, primaRecargada) + tablaValoresPolizaAsociadosRecargada$Vx[kTemp])*(1+iTasa)*(tabla_Mortalidad$lx[xEdad+kTemp]/tabla_Mortalidad$lx[xEdad+1])

    egreso <-(funcMuerteCostosMSR(kTemp ))*(tabla_Mortalidad$dx[xEdad+kTemp]/tabla_Mortalidad$lx[xEdad+1])

    reserva <- tablaValoresPolizaAsociadosRecargada$Vx[kTemp+1]*(tabla_Mortalidad$lx[xEdad+kTemp+1]/tabla_Mortalidad$lx[xEdad+1])

    resultado <- 100*round(ingreso-egreso-reserva, digits = 9)
    return(resultado)
}


#------ Tabla de Mortalidad Estimada
generate_mortality_table_gamma <- function(alpha, start_age = 46, l_start = 10000, max_age = 101, beta = 1/4) {
  # Vector de edades
  ages <- start_age:max_age
  
  # Calcula la función de supervivencia gamma para cada edad
  Sx <- 1 - pgamma(ages - start_age, shape = alpha, rate = beta)
  
  # Calcula lx basado en la función de supervivencia
  lx <- l_start * Sx
  
  # Calcula dx
  dx <- -c(diff(lx), NA) # Agregamos NA al final porque no hay dx para la última edad
  
  # Crear data.frame para la tabla de mortalidad
  mortality_table <- data.frame(Age = ages, lx = lx, dx = dx)
  
  # Agregar filas con NA hasta el inicio
  na_rows <- data.frame(Age = (0):(start_age-1), lx = rep(NA, start_age), dx = rep(NA, start_age))
  mortality_table <- rbind(na_rows, mortality_table)
  
  return(mortality_table)
}

# FUNCIONES DE MEDIDAS

medidaVPN <- function(rCC=.16, iTasa=iTasaEstimada, tabla_Mortalidad=tablaMortalidadEstimada){
   #ktemo solo está entre 1 a n
    v <- 1/(1+rCC)
    resultado <- 0
    for (kTemp in seq(1,n,1)){
        resultado <- resultado + flujos_utilidad(kTemp, iTasa, tabla_Mortalidad)*v^(kTemp)
    }
    return(resultado)
}

medidaMU <- function(rCC=.16, iTasa=iTasaEstimada, tabla_Mortalidad=tablaMortalidadEstimada){
  v <- 1/(1+rCC)
  resultado <- 0
  for (kTemp in seq(1,mPrimas,1)){
    resultado <- resultado + primaRecargada*v^(kTemp-1)
  }
  return(medidaVPN(rCC, iTasaEstimada, tablaMortalidadEstimada)/(resultado*100))
}


# FUNCIONES PARA EL ASSETSHARE

# Inicializar la función AssetShare
funcInteres <- function(kTemp){
  #kTemp solo puede ir de 0-24
  resultado <- valoresObservados_tasaInteres$'Tasa de interés efectiva anual'[kTemp+1]
  return(resultado)
}
primaPropCostos <- function(kTemp, prima){
  if(kTemp==0){
    resultado <- .6*prima
  } else if(kTemp %in% c(1,2)){
    resultado <- .8*prima}
  else if(kTemp>=3 && kTemp<=mPrimas-1){
    resultado <- .95*prima
  } else {
    resultado <- 0
  }
  return(resultado)
}
funcPrimaCostos <- function(kTemp, prima){
  if(kTemp<=mPrimas-1){
    resultado <- primaPropCostos(kTemp, prima)-valoresObservados_costosPromedios$'Costo Asociado a la Prima G'[kTemp+1]
  } else {
    resultado <- 0
  }
  return(resultado)
}

propLiquidacion <- function(kTemp){
  #kTemp es >=1
  #Se da vencidamente
  if(kTemp>=1 && kTemp<=n){
    resultado <- resultado <- 1.003*saMuerte
  } else {
    resultado <- 0
  }
 return(resultado)
}

funcMuerteCostos <- function(kTemp){
  #kTemp es >=1
  #Se da vencidamente
  if(kTemp>=1 && kTemp<=n){
    resultado <- propLiquidacion(kTemp)+valoresObservados_costosPromedios$'Costo Asociado a liquidación por Muerte'[kTemp+1]
  } else {
    resultado <- 0
  }
  return(resultado)
}

#EXTRAEMOS MÁS DATOS

######-------------------------------------
#Valores observados

valoresObservados_tasaInteres <- read_excel("Valores observados.xlsx", sheet="Observados Promedio", range="A3:B28")

valoresObservados_costosPromedios <- read_excel("Valores observados.xlsx", sheet="Observados Promedio", range="D3:G29")

valoresObservados_carteraObservados <- read_excel("Valores observados.xlsx", sheet="Cartera Observados", range="A1:B101")

valoresObservados_qxPromedios <- read_excel("Valores observados.xlsx", sheet="Observados Promedio", range="I2:J103")

#FUNCIONES DE MORTALIDAD 
#función 1
funcMortalidad <- function(kTemp){
  #kTemp es >=0
  resultado <- valoresObservados_qxPromedios$qx[xEdad+kTemp+1]
  return(resultado)
  
}
#funcion2

# Iniciar l_0
l_0 <- 100
lx <- numeric(max(valoresObservados_carteraObservados$'K(x)') + 1)
lx[1] <- l_0

# Calcular dx y lx
for (k in 0:max(valoresObservados_carteraObservados$'K(x)')) {
  dx <- sum(valoresObservados_carteraObservados$'K(x)' == k)
  if (k > 0) {
    lx[k+1] <- lx[k] - dx
  }
}

# Crear el dataframe final de lx y dx
tabla_mortalidad <- data.frame(x = 0:max(valoresObservados_carteraObservados$'K(x)'), lx = lx, dx = -c(diff(lx), NA))

# Calcular qx
tabla_mortalidad$qx <- tabla_mortalidad$dx / tabla_mortalidad$lx

funcMortalidad2 <- function(kTemp){
  #kTemp es >=0
  resultado <- tabla_mortalidad$qx[xEdad+kTemp+1]
  return(resultado)
  
}

#ASSET SHARE

assetShare <- function(kTemp, funcInteres, funcPrimaCostos, funcMuerteCostos,funcMortalidad, prima){
  if(kTemp>=1 && kTemp<=n){
    assetShareAnterior <- assetShare(kTemp-1, funcInteres, funcPrimaCostos, funcMuerteCostos,funcMortalidad, prima)

    resultado <- ((assetShareAnterior+funcPrimaCostos(kTemp-1, primaRecargada))*(1+funcInteres(kTemp-1))-funcMortalidad(kTemp-1)*funcMuerteCostos(kTemp))/(1-funcMortalidad(kTemp-1))
  }
  else{
    resultado <- 0
  }
  return(resultado)
}


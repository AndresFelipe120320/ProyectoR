
library(nycflights13)
library(tidyverse)
library(ggplot2)
#'Se asigna el dataframe a una nueva variable llamada Bd
Bd <- nycflights13::flights
#'Se crea una nueva funcion
#'con el llamado a la función se extrae todos los vuelos con 2 horas o mas de retrasos
#'@return Vuelos con 2 o mas horas de retraso
#'@examples R2 <- RetrasoMas2Horas()
RetrasoMas2Horas <- function(){
  RetrasoM2Horas <- Bd %>%
    filter(arr_delay>=120, dep_delay>=120)
  ggplot(data = RetrasoM2Horas, aes(x = carrier)) +
    geom_bar() +
    ggtitle("Aerolíneas con más vuelos retrasados") +
    xlab("Aerolínea") +
    ylab("Cantidad de vuelos retrasados")
}
#´Hago el llamado de la función
R2 <- RetrasoMas2Horas()
#'Se realiza una nueva función para conocer los vuelos que salierón a Houston
#'Con el llamado a la funcion VuelosHouston se extrae todos los vuelos que salieron a Houston
#'@examples Vh <- VuelosHouston()
#'@return retorna los vuelos a Houston
VuelosHouston <- function(){
  VueloHouston <- Bd %>%
    filter(dest=="IAH"| dest=="HOU")
}
Vh <- VuelosHouston()

#'Se realiza una nueva función para conocer los vuelos que fueron operados por United, American y Delta
#'Con el llamado a la funcion VuelosHouston se extrae todos los vuelos que fueron operados por United, American y Delta
#'@examples Op <- VoperadosUUAUDL()
#'@return retorna los vuelos que fueron operados por United, American y Delta
VoperadosUUAUDL <- function(){
  OperadosAAUADL <- Bd %>%
    filter(carrier=="AA" | carrier=="UA" | carrier=="DL")
}
Op <- VoperadosUUAUDL()


#'Se realiza una nueva función para conocer los vuelos que salierón en verano
#'Con el llamado a la funcion VuelosHouston se extrae todos los vuelos que salierón en verano
#'@examples Sv <- SalidaVerano()
#'@return retorna los vuelos que salierón en verano
SalidaVerano <- function(){
  SalidaVeranon <- Bd%>%
    filter(month==7 | month==8 | month==9)
}
Sv <- SalidaVerano()

#'Se realiza una nueva función para conocer los vuelos que se retrasaron al menos 1 hora
#'Con el llamado a la funcion VuelosHouston se extrae todos los vuelos que se retrasaron al menos 1 hora
#'@examples R1h <- RetrasoAlM1hora()
#'@return retorna los vuelos que se retrasaron al menos 1 hora
RetrasoAlM1hora <- function(){
  RetrasaronPr <- Bd %>%
    filter(dep_delay>=60)
}
R1h <- RetrasoAlM1hora()

#'Se realiza una nueva función para conocer los vuelos que salieron entre la media noche hasta las 6 de la mañana
#'Con el llamado a la funcion VuelosHouston se extrae todos los vuelos que salieron entre la media noche hasta las 6 de la mañana
#'@examples Ve12To6 <- Vuelos12To6()
#'@return retorna los vuelos que salieron entre la media noche hasta las 6 de la mañana
Vuelos12To6 <- function(){
  Salida12amto6am <- Bd %>%
    filter(hour>=0 & hour<=6)
}
Ve12To6 <- Vuelos12To6()

#---------------------------------------------------------------------------------------------
# segundo punto 5.2.4

Between1 <- Bd %>%
  filter(between(hour, 6, 8))

Between2 <- Bd%>%
  filter(between(month, 5, 6))

Between3 <- Bd %>%
  filter(between(arr_delay, -1, 0))

Between4 <- Bd %>%
  filter(between(dep_delay, 1, 30) & between(arr_delay, 31, Inf))

Between5 <- Bd %>%
  filter(between(dep_delay, 1, 30) & between(arr_delay, 31, Inf))

# Solución 4


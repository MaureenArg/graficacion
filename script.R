# Trabajo previo 


## carga de paquetes

library (ggplot2)
library (plotly)
library (dplyr)
library (graphics)


## carga de datos 

rios <-
  read.csv(file = 'https://raw.githubusercontent.com/tpb728O-programaciongeoespacialr/2021ii/main/datos/hidrologia/fdc/fdc.csv')

## 1. Gráfico de barras apiladas que muestre el caudal de los ríos Pandora y Bananito por cada uno de los 12 meses del año

## conversión de variables 

rios <-
  rios %>%
  rename(fecha = Tiempo,
         pandora = Pandora.mm..637km2.,
         banano = Banano.mm..90km2.) %>%
  mutate (fecha = as.Date(fecha, format = "%m/%d/%Y"))

## Manipulación de datos

rios_x_mes <-
  rios %>%
  select (fecha, pandora, banano) %>%
  mutate (fecha = as.Date(fecha, format = "%d/%m/%Y")) %>%
  group_by( mes = format(fecha, "%m")) %>%
  summarise(caudal_pandora = sum (pandora), caudal_banano= sum (banano))

rios_x_mes
  
## Graficación

ggplot () + geom_col (
  data = rios_x_mes, 
  aes (x = mes,
      y = caudal_banano,  fill = "Río Banano"), width = 0.5) +
  geom_col (
      data = rios_x_mes,
      aes (x = mes, y = caudal_pandora, fill = "Río Pandora"), width = 0.5)  + 
  ggtitle ("Caudal mensual (m3/s)  de los Ríos Pandora y Banano, Limón, Costa Rica") +
  xlab ("Mes") +
  ylab ("Caudal (m3/s)") +
  scale_fill_manual (values = c("#97FFFF", "#4169E1")) +
  theme (
    legend.title = element_blank(), legend.position = "left", plot.title = element_text (size = 18)                                                                                                                                                                                                                   )


## 2.  Histograma que muestre la distribución de los casos nuevos de covid en Costa Rica. Elija cuidadosamente el tamaño de los “bins”

## carga de datos 

covid <- 
  read.csv(
    file='https://raw.githubusercontent.com/tpb728O-programaciongeoespacialr/2021ii/main/datos/minsalud/covid/ultimafecha_CSV_GENERAL.csv', 
    sep = ","
  )

str (covid)


## conversion de las variables 

covid1 <-
  covid %>%
  select (FECHA, nue_posi) %>%
  rename(fecha = FECHA) %>%
  mutate (fecha = as.Date(fecha, format = "%d/%m/%Y"))

## Histograma 

ggplot (covid1, aes (x = fecha, y = nue_posi)) + geom_histogram(binwidth = 0.05, stat= 'identity', fill = "white", color= "red" )+
ggtitle("Distribución de los casos nuevos de Covid-19, Costa Rica (2020-2021)") +
  xlab("Fecha")+
  ylab("Total de casos nuevos") + 
  theme(plot.title = element_text(size = 18))





## 3. Gráfico de barras agrupadas que muestre la cantidad de hombres y mujeres fallecidos por covid en Costa Rica por cada uno de los 12 meses del año.


covid2 <-
  covid %>%
  select (FECHA, muj_fall, hom_fall) %>%
  rename(fecha = FECHA) %>%
  mutate (fecha = as.Date(fecha, format = "%d/%m/%Y")) %>%
  group_by(mes = format(fecha, "%m")) %>%
  summarise(sum_hom = sum(hom_fall),sum_muj = sum(muj_fall))

covid2


## graficación

ggplot() +
  geom_col(
    data = covid2,
    aes(x = mes, y = sum_muj, fill = "Mujeres"),
    width = 0.3,
    position = position_nudge(x = -0.15)
  ) +
  
  geom_col(
    data = covid2,
    aes(x = mes, y = sum_hom, fill = "Hombres"),
    width = 0.3,
    position = position_nudge(x = 0.15)
  ) +
  scale_fill_manual(values = c("#7FFFD4", "#458B00")) +
  ggtitle( "Cantidad de mujeres y hombres fallecidos por el Covid-19 en Costa Rica en 2020-2021") +
  xlab("Mes") +
  ylab("Cantidad de Fallecidos") +
  theme(
    legend.title = element_blank(),
    legend.position = "left",
    plot.title = element_text(size = 19, face = "bold")
  )

  
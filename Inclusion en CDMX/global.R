

library(shiny)
library(echarts4r)
library(dplyr)
library(leaflet)
library(data.table)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinyWidgets)
library(viridisLite)
#library(sever)


delegaciones <- c("Azcapotzalco", "Iztacalco", 
                  "Milpa Alta", "Álvaro Obregón", 
                  "Xochimilco", "Venustiano Carranza", 
                  "Coyoacán", "Tlalpan", "Benito Juárez",
                  "Cuajimalpa de Morelos", "Miguel Hidalgo",
                  "Gustavo A. Madero", "Cuauhtémoc", 
                  "Magdalena Contreras", "Iztapalapa",
                  "Tláhuac"
)

# Cargando los datos

desc <- unzip("datos/3516a236-cc06-4c2f-8752-2ef9343dd2e0.zip")

datos <- data.table::fread(desc) 
espaciales <- sf::read_sf("shapefiles/alcaldiascdmxwgs84.shp") |> 
  tibble::rownames_to_column("n") |> 
  mutate(Delegación = delegaciones) 

datos[is.na(datos)] <- 0
espaciales$n <- as.numeric(espaciales$n)


ingresos <- datos |> 
  group_by(nombre_alcaldia) |> 
  summarise(Ingresos = sum(monto_aprobado))


espaciales <- espaciales |> 
  left_join(ingresos, by = c("Delegación" = "nombre_alcaldia"))


mytext <- paste0(
  "Delegación: ", espaciales$Delegación,"<br/>", 
  "Monto total: ", "$ ", scales::comma(espaciales$Ingresos), "<br/>") |> 
  lapply(htmltools::HTML)


# Paleta de colores y tooltip
pal <- colorBin("Greys", domain = espaciales$Ingresos, bins = 5)


############### estratos ################


est_sociales <- read.csv("datos/f474b7ea-3870-4f4a-8033-a857e018f17f")
est_sociales$nomgeo <- gsub("La Magdalena Contreras", "Magdalena Contreras", est_sociales$nomgeo)


est_sociales_faltante <- est_sociales |> 
  filter(nomgeo == "Coyoacán")
est_sociales_faltante$nomgeo <- "Milpa Alta"
est_sociales_faltante$total <- 0

est_sociales <- est_sociales |> 
  bind_rows(est_sociales_faltante)



espaciales_estsociales <- sf::read_sf("shapefiles/alcaldiascdmxwgs84.shp") |> 
  tibble::rownames_to_column("n") |> 
  mutate(Delegación = delegaciones) 



### Selección de estrato social
estrato_select <- est_sociales |> 
  group_by(estratos) |> 
  summarise(n()) |> 
  select(estratos)



##### objetos para graficos finales

cant_pobreza <- est_sociales |> 
  group_by(nomgeo) |> 
  filter(estratos %in% c("Pobreza alta", "Pobreza muy alta", "Satisfaccion mínima") ) |> 
  summarise(total = sum(total)) 




cant_asign <- datos |> 
  group_by(nombre_alcaldia) |> 
  summarise(total_asig = sum(monto_aprobado)) 


relacion <- cant_asign |> 
  left_join(cant_pobreza, by = c("nombre_alcaldia" = "nomgeo"))


porcentaje_fin <- cant_asign |> 
  mutate(porcentaje = total_asig/sum(total_asig))

### datos costo suelo
data_suelo <- read.csv("datos/data_suelo.csv")
data_suelo <- data_suelo |> 
  select(Delegacion, total, Ingresos)
data_suelo$total <- as.numeric(data_suelo$total)
data_suelo$Ingresos <- as.numeric(data_suelo$Ingresos)

data_suelo <- data_suelo |> 
  mutate(Ingresos_mill = Ingresos/1000000)


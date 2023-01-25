library(data.table)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(sf)
library(sp)
# library(scales)
# library(rgdal)
# library(maptools)
library(tidyverse)
library(openrouteservice)
# library(mapview)
library(xlsx)
library(RColorBrewer)
library(geosphere)


# Estou a correr o ors localmente com o docker, entao esta neste url
options(openrouteservice.url = "http://localhost:8080/ors")


#Loading and cleaning data
Barb <- fread(".\\Barbara\\Codigo Postal - Barbara.csv")

Moradas <- fread(".\\Barbara\\pt_addresses.csv", encoding = "UTF-8") %>% 
  unique %>% 
  mutate_if(is.character, str_to_lower) -> Moradas

Resultados <- fread(".\\Barbara\\Resultados.csv",encoding = "UTF-8") %>% 
  unique %>% 
  dplyr::select(-"CÛd. postal")
Resultados <- Resultados[1:219,]
Resultados$`Tipo SMS` <- as.character(Resultados$`Tipo SMS`)
Resultados$`Tipo SMS` <- ifelse(is.na(Resultados$`Tipo SMS`), 
                             'n„o', Resultados$`Tipo SMS`)

Barb <- subset(Barb, select = -V3) %>% 
  unique %>% 
  rename(Cod_Postal =`Codigo postal`)

Casos <- Barb[1:148, ]
Controlo <- Barb[150:220, ]


#Grouping with coords (sem converter para xy)
Controlo_com_Moradas <- merge(Controlo, Moradas[ ,c("city", "postcode", "lon", "lat")],
                                      by.x = "Cod_Postal", 
                                      by.y = "postcode", 
                                      all.x = T) %>% 
  group_by(`ID Animal`) %>%
  unique


Casos_com_Moradas <- merge(Casos, Moradas[ , c("city", "postcode", "lon", "lat")], 
                                  by.x = "Cod_Postal", 
                                  by.y = "postcode", 
                                  all.x = T) %>% 
  group_by(`ID Animal`) %>%
  unique

# Calculo do centroide das coordenadas
CasosDT <-setDT(Casos_com_Moradas) %>% 
  group_by(`ID Animal`) %>% 
  filter(n()>= 3) %>% 
  ungroup()
CasosDT <-setDT(CasosDT)


findCentroid <- function(Lon, Lat, ...){
  centroid(cbind(Lon, Lat), ...)
}

CasosDT[, c("Cent_lon", "Cent_lat") := as.list(findCentroid(lon, lat)), by = `ID Animal`]

Casos_centroide <- as.data.frame(CasosDT) %>% 
  subset(select=c("Cod_Postal","ID Animal","city","Cent_lon","Cent_lat")) %>% 
  group_by(`ID Animal`) %>%
  unique


ControloDT <-setDT(Controlo_com_Moradas ) %>% 
  group_by(`ID Animal`) %>% 
  filter(n()>= 4) %>% 
  ungroup()
ControloDT <-setDT(ControloDT)


ControloDT[, c("Cent_lon", "Cent_lat") := as.list(findCentroid(lon, lat)), by = `ID Animal`]

Controlo_centroide <- as.data.frame(ControloDT) %>% 
  subset(select=c("Cod_Postal","ID Animal","city","Cent_lon","Cent_lat")) %>% 
  group_by(`ID Animal`) %>%
  unique

# Agr juntar as Df pq tenho de ir buscar os que faltam pq tinham <3 ou <4 de pontos
Coord_casos <- Casos_com_Moradas %>%
  group_by(`ID Animal`) %>% 
  summarise(across(.fns = mean)) %>%
  subset(select = -c(2:3)) %>%
  merge(Casos_centroide, by.x = "ID Animal", by.y="ID Animal", all.x=TRUE) %>% 
  merge(Casos, by.x = "ID Animal", by.y="ID Animal") %>% 
  subset(select=-c(4))
Coord_casos$Cent_lon <- ifelse(is.na(Coord_casos$Cent_lon), Coord_casos$lon, Coord_casos$Cent_lon) 
Coord_casos$Cent_lat <- ifelse(is.na(Coord_casos$Cent_lat), Coord_casos$lat, Coord_casos$Cent_lat) 

Coord_casos <- Coord_casos[,c(1,7,4:6)]
Coord_casos<- Coord_casos %>% 
  rename(lat=Cent_lat) %>% 
  rename(lon=Cent_lon)

Coord_controlo <- Controlo_com_Moradas  %>%
  summarise(across(.fns = mean)) %>%
  subset(select = -c(2:3)) %>%
  merge(Controlo_centroide, by.x = "ID Animal", by.y="ID Animal", all.x=TRUE) %>% 
  merge(Controlo, by.x = "ID Animal", by.y="ID Animal") %>% 
  subset(select=-c(4))

Coord_controlo$Cent_lon <- ifelse(is.na(Coord_controlo$Cent_lon), Coord_controlo$lon, Coord_controlo$Cent_lon) 
Coord_controlo$Cent_lat <- ifelse(is.na(Coord_controlo$Cent_lat), Coord_controlo$lat, Coord_controlo$Cent_lat) 
Coord_controlo <- Coord_controlo[,c(1,7,4:6)] 
Coord_controlo <- Coord_controlo %>% 
  rename(lat=Cent_lat) %>% 
  rename(lon=Cent_lon)


# Routing now
# Directions to FMV UL from controlo points
# Careful, coordinates must be in order, meaning i need to add to the dfs FMV's coords in order

controlo_para_dir <- Coord_controlo %>% 
  group_by(`ID Animal`) %>% 
  group_modify(~add_row
               (lon=-9.195503158186124, lat=38.7139285562482,.x)) %>% 
  ungroup() %>% 
  dplyr:: select(lon, lat)

controlos_dir <- ors_directions(controlo_para_dir)

# Directions to FMV UL from case points
# Ora aqui ha outro problema que e nao haver dire√ßoes obviamente do funchal para a fmv pronto 
# Consegui por a funcionarlocalmente, entao nao preciso de andar a dividir a DB, mas tenho de tirar a madeira da equacao
# pontos a tirar: 250 e 251 (251 √© madeira acho)

casos_para_dir <- Coord_casos %>% 
  group_by(`ID Animal`) %>% 
  drop_na() %>% 
  subset(city!="funchal") %>% #Retirar a madeira pq n d· rota nÈ
  group_modify(~add_row
               (lon=-9.195503158186124, lat=38.7139285562482,.x)) %>% 
  ungroup() %>%
  dplyr:: select(lon, lat)

casos_dir <- ors_directions(casos_para_dir)


### Mapa com routes e pontos
leaflet_map <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = Coord_casos,
                   lng = ~lon,
                   lat = ~lat,
                   popup = ~`ID Animal`,
                   label = ~`ID Animal`,
                   group = "Casos",
                   radius = 2.5,
                   color = "blue",
                   stroke = FALSE,
                   fillOpacity = 0.7) %>% 
  addCircleMarkers(data = Coord_controlo,
                   lng = ~lon,
                   lat = ~lat,
                   popup = ~`ID Animal`,
                   label = ~`ID Animal`,
                   group = "Controlos",
                   radius = 2.5,
                   color = "red",
                   stroke = FALSE,
                   fillOpacity = 0.7) %>% 
  addGeoJSON(casos_dir, 
             fill = FALSE, 
             group="Casos") %>% 
  addGeoJSON(controlos_dir, 
             fill = FALSE,
             color = "red",
             group="Controlos") %>% 
  addLayersControl(overlayGroups = c("Casos", "Controlos"),
                   options = layersControlOptions(collapsed = FALSE))

leaflet_map 

# Com ors_matrix consegues distancia e tempo para as rotas, e nao tens de ter por ordem
# posso pegar na DB original s√≥ dos casos/controlos, adicionar uma ultima linha com FMV
# Assim tenho distancias casos/controlo vs fmv numa unica coluna

Caso_FMV <- Coord_casos %>% 
  dplyr:: select(lon, lat) %>% 
  drop_na() %>% 
  add_row (lon=-9.195503158186124, lat=38.7139285562482)

matrix_casos <- ors_matrix(Caso_FMV, metrics = c("duration", "distance"), units = "km") 
(matrix_casos$durations/60) %>%  round(1)

Controlo_FMV <- Coord_controlo %>% 
  add_row (lon=-9.195503158186124, lat=38.7139285562482) %>% 
  dplyr:: select(lon, lat)

matrix_controlo <- ors_matrix(Controlo_FMV, metrics = c("duration", "distance"), units = "km") 
(matrix_controlo$durations/60) %>%  round(1)

# Selecionar so a ultima coluna e temos distancia ponto 1:n a fmv
# problema: tivemos de dar drop a NA's, o que sigfnifica que dar match vai ser uma tarefa interessante
# Isto e giro pq so me interessa a coluna final, portanto selecionamos so coluna 143
# Convem renomear a coluna e dar match das rows

casos_p_matrix <- Coord_casos %>% 
  dplyr:: select(lon, lat) %>% 
  drop_na()

controlo_p_matrix <- Coord_controlo

matrix_dist_carro_casos <- as.data.frame(matrix_casos$distances) %>% 
  subset(select = c(144)) %>%
  slice(-c(144)) %>% 
  rename(`Distancia (de carro, em km) a HEFMV` = V144) %>% 
  cbind(casos_p_matrix)

matrix_temp_carro_casos <- as.data.frame(matrix_casos$durations / 60) %>% 
  subset(select = c(144)) %>% 
  slice(-c(144)) %>% 
  rename(`Distancia (de carro, em min) a HEFMV` = V144) %>% 
  cbind(casos_p_matrix)

matrix_dist_carro_controlo <- as.data.frame(matrix_controlo$distances) %>% 
  subset(select = c(72)) %>%
  slice(-c(72)) %>% 
  rename(`Distancia (de carro,em km) a HEFMV` = V72) %>% 
  cbind(controlo_p_matrix)

matrix_temp_carro_controlo <- as.data.frame(matrix_controlo$durations / 60) %>% 
  subset(select = c(72)) %>% 
  slice(-c(72)) %>% 
  rename(`Distancia (de carro,em min) a HEFMV` = V72) %>% 
  cbind(controlo_p_matrix)


# Ligar DBs

Casos_com_dados <- Coord_casos %>% 
  left_join(matrix_dist_carro_casos) %>% 
  left_join(matrix_temp_carro_casos) %>% 
  merge(Resultados, by.x="ID Animal", by.y="ID animal") %>% 
  distinct()


Controlo_com_dados <- Coord_controlo %>% 
  left_join(matrix_dist_carro_controlo) %>% 
  left_join(matrix_temp_carro_controlo) %>% 
  merge(Resultados, by.x="ID Animal", by.y="ID animal") %>% 
  distinct()

# Exportar para excel
write.xlsx(Controlo_com_dados, file=".\\Barbara\\Barbara_resultados.xlsx", sheetName="Controlo", row.names=FALSE)
write.xlsx(Casos_com_dados, file=".\\Barbara\\Barbara_resultados.xlsx", sheetName="Casos", append=TRUE, row.names=FALSE)

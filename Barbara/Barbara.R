
library(data.table)
library(tidyverse)
library(leaflet)
library(rgeos)
library(ggplot2)
library(sf)
library(sp)
library(scales)
library(rgdal)
library(maptools)
library(tidyverse)
library(adehabitatHR)
library(openrouteservice)
library(mapview)

# Estou a correr o ors localmente com o docker, entao esta neste url
options(openrouteservice.url = "http://localhost:8080/ors")



#Loading data
Barb <- fread(".\\Barbara\\Codigo Postal - Barbara.csv")

Moradas <- fread(".\\Barbara\\pt_addresses.csv", encoding = "UTF-8") %>% 
  unique %>% 
  mutate_if(is.character, str_to_lower) -> Moradas
# Isto tem city associado a codigo postal as well
# O que quer dizer que consigo ir buscar a NUT atraves da cidade, somehow

NUTS <- fread(".\\Barbara\\NUTS.csv", encoding= "UTF-8") %>%
  subset(select = -c(1,10:14)) %>% 
  unique %>% 
  mutate_if(is.character, str_to_lower) -> NUTS

Corresponde <- fread("Barbara/Correspondecias.csv") %>%
  mutate_if(is.character, str_to_lower) -> Corresponde 

# Cleaning data
Barb <- subset(Barb, select = -V3) %>% 
    unique %>% 
  rename(Cod_Postal =`Codigo postal`)
  
Casos <- Barb[1:148, ]
Controlo <- Barb[150:220, ]


#Grouping with coords (sem converter para xy)
Conjuncture_Controlo_Moradas <- merge(Controlo, Moradas[ ,c("city", "postcode", "lon", "lat")],
                                        by.x = "Cod_Postal", 
                                        by.y = "postcode", 
                                        all.x = T) %>% 
  group_by(`ID Animal`) %>%
  unique

  
Conjuncture_Barb_Moradas <- merge(Casos, Moradas[ , c("city", "postcode", "lon", "lat")], 
                      by.x = "Cod_Postal", 
                      by.y = "postcode", 
                      all.x = T) %>% 
   group_by(`ID Animal`) %>%
  unique
  
# Kinda funciona, pq a lista dos municipios por NUT nao esta completa, entao ficam a faltar concelhos
# Mas os que estao na lista tao ok

#Plotting the minimum convex polygon, full guide here:
#"https://jamesepaterson.github.io/jamespatersonblog/03_trackingworkshop_homeranges#:~:text=The%20minimum%20convex%20polygon%20"
#Removing NA's
#Remover ID's que tenham menos de 5 obs, mcp nao funciona otherwise
Conjuncture_Controlo_wna <- Conjuncture_Controlo_Moradas[!is.na(Conjuncture_Controlo_Moradas$lon) & !is.na(Conjuncture_Controlo_Moradas$lat), ]%>% 
    group_by(`ID Animal`) %>% 
    filter(n()>= 5) %>% 
    ungroup()

Conjuncture_Barb_wna <- Conjuncture_Barb_Moradas[!is.na(Conjuncture_Barb_Moradas$lon) & !is.na(Conjuncture_Barb_Moradas$lat), ]%>% 
    group_by(`ID Animal`) %>% 
    filter(n()>= 5) %>% 
    ungroup()
  
#Simplifying the sets
Controlo.sp <- Conjuncture_Controlo_wna[, c("ID Animal", "lon", "lat")]  
  
Barb.sp <- Conjuncture_Barb_wna[, c("ID Animal", "lon", "lat")] 
  
#Attributting coords
#https://stackoverflow.com/questions/68157684/set-crs-for-latitude-longitude-point-data
coordinates(Controlo.sp) <- c("lon", "lat")
coordinates(Barb.sp) <- c("lon", "lat")
  
#WGS84 is referenced to The International Reference Meridian.  
#WGS84 lat/long coordinates are stationary with respect to the average of all 
#global tectonic motions
proj4string(Controlo.sp) <- CRS("+init=epsg:4326 +proj=longlat +ellips=WGS84  +no_defs")
  
  
proj4string(Barb.sp) <- CRS("+init=epsg:4326 +proj=longlat +ellips=WGS84 +no_defs")
  

Controlo.mcp <- mcp(Controlo.sp, percent = 95)  
Barb.mcp <- mcp(Barb.sp, percent = 95)
  
  
plot(Controlo.mcp, col = as.factor(Controlo.mcp@data$id), pch = 16)
plot(Barb.mcp, col = alpha(1:5, 0.5), add = T)

#Now that we have the polygons, we need to calculate the centroids
#https://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes
#Using rgeos pack and
#Using sp to convert into a spatialpointsdataframe so it can be plotted
Centroid_Barb <- gCentroid(Barb.mcp, byid = T) 
  
SPDF_Centroid_Barb  <- SpatialPointsDataFrame(Centroid_Barb, data.frame(row.names = row.names(Centroid_Barb))) 
    
Centroid_Controlo <- gCentroid(Controlo.mcp, byid = T)
  
SPDF_Centroid_Controlo  <- SpatialPointsDataFrame(Centroid_Controlo, data.frame(row.names = row.names(Centroid_Controlo))) 
  
#Stitching them together
Coord_Barb_Morad_Simp <- Conjuncture_Barb_Moradas %>%
                        summarise(across(.fns = mean)) %>%
                        subset(select = -c(2:3)) %>%
                        merge(Casos, by = "ID Animal")

Coord_Control_Morad_Simp <- Conjuncture_Controlo_Moradas %>%
                        summarise(across(.fns = mean)) %>%
                        subset(select = -c(2:3)) %>%
                        merge(Controlo, by = "ID Animal")
#Trying a plot
map <- leaflet() %>%
     addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
     addProviderTiles(provider = "Esri", group = "Light") %>%
    
     addCircleMarkers(data = Conjuncture_Barb_Moradas,
                     lng = ~lon,
                     lat = ~lat,
                     popup = ~`ID Animal`,
                     label = ~Cod_Postal,
                     group = "Cases",
                     clusterOptions = NULL,
                     radius = 2.5,
                     color = "red",
                     stroke = FALSE,
                     fillOpacity = 0.5) %>% 
  
    addCircleMarkers(data = Conjuncture_Controlo_Moradas,
                     lng = ~lon,
                     lat = ~lat,
                     popup = ~`ID Animal`,
                     label = ~Cod_Postal,
                     group = "Controls",
                     radius = 2.5,
                     color = "blue",
                     stroke = FALSE,
                     fillOpacity = 0.7) %>% 
    
    addCircleMarkers(data = SPDF_Centroid_Barb,
                     group = "Centroids",
                     radius = 2.5,
                     color = "orange",
                     stroke = FALSE,
                     fillOpacity = 0.7) %>%

    addCircles(data = SPDF_Centroid_Controlo,
                     group = "Centroids",
                     radius = 2.5,
                     color = "orange",
                     stroke = FALSE,
                     fillOpacity = 0.7) %>%
    
    addPolygons(data = Controlo.mcp,
                popup = ~id,
                color = "Darkblue",
                group = "Polygons") %>% 
    
    addPolygons(data = Barb.mcp,
                popup = ~id,
                color = "Yellow", 
                group = "Polygons") %>%

    addLayersControl(baseGroups = c("Dark", "Light"),
                     overlayGroups = c("Cases", "Centroids", "Controls", "Official", "Polygons"),
                     options = layersControlOptions(collapsed = FALSE))
  
    
map
  


# Adicionar correspondencia a nuts
Coord_Casos_Morad_NUTS <- Conjuncture_Barb_Moradas %>% 
  subset(select = c(1:3)) %>% 
  unique %>%
  merge(Corresponde,
        by.x="city",
        by.y="Localidades DB",
        all.x=T) %>% 
  subset(select = c(2:4)) %>% 
  merge(NUTS,
        by.x = "Correspondencia",
        by.y = "CONCELHO_DSG",
        all.x = T)

Coord_Controlo_Morad_NUTS <- Conjuncture_Controlo_Moradas %>% 
  subset(select = c(1:3)) %>% 
  unique %>%
  merge(Corresponde,
        by.x="city",
        by.y="Localidades DB",
        all.x=T) %>% 
  subset(select = c(2:4)) %>% 
  merge(NUTS,
        by.x = "Correspondencia",
        by.y = "CONCELHO_DSG",
        all.x = T)

# Isto e um caso controlo de compliance vacinacao
# Portanto pensar tipo.. habilitacoes literarias/rendimentos/???
# Uplaod de dados INE
# Habilitacoes (dados 2020-2021)
habilita_sec <- fread(".\\Barbara\\Proporcao ensino sec.csv", encoding = "UTF-8") %>% 
  mutate_if(is.character, str_to_lower) -> habilita_sec
habilita_sup <- fread(".\\Barbara\\Proporcao ensino sup.csv", encoding = "UTF-8") %>% 
  mutate_if(is.character, str_to_lower) -> habilita_sup


# Rendimentos
taxa_emp <- fread(".\\Barbara\\Taxa de emprego.csv", encoding = "UTF-8") %>% 
  mutate_if(is.character, str_to_lower) -> taxa_emp

# Ligar DB
Casos_com_dados <- Coord_Casos_Morad_NUTS %>% 
  merge(habilita_sec,
        by.x="Correspondencia",
        by.y="Local de residência (à data dos Censos 2021)") %>% 
  merge(habilita_sup,
        by.x="Correspondencia",
        by.y="Local de residência (à data dos Censos 2021)") %>% 
  merge(taxa_emp,
        by.x="Correspondencia",
        by.y="Local de residência (à data dos Censos 2021)")


# Mapping routes and such
# 1st isochrone map from FMV-UL
# 1- 20min distance; 2-40min; 3-60min (by car)


mapviewOptions(fgb = FALSE)
coordinates <- data.frame(lon = c(-9.195503158186124) , lat = c(38.71392855624822))
res <- ors_isochrones(coordinates, range = 4800, interval = 1200, output = "sf")
res

values <- levels(factor(res$value))
ranges <- split(res, values)
ranges <- ranges[rev(values)]

names(ranges) <- sprintf("%s min", as.numeric(names(ranges))/60)

mapview(ranges, alpha.regions = 0.2, homebutton = FALSE, legend = FALSE)


# Routing now

# Versao com o ORS
# Directions to FMV UL from controlo points
# Careful, coordinates must be in order, meaning i need to add to the dfs FMV's coords in order

FMV_coord <- c(-9.195503158186124, 38.7139285562482)

u <- Coord_Control_Morad_Simp %>% 
  group_by(`ID Animal`) %>% 
  group_modify(~add_row
                   (lon=-9.195503158186124, lat=38.7139285562482,.x)) %>% 
  ungroup()
u2 <- dplyr:: select(u, lon, lat)

x <- ors_directions(u2)

# Directions to FMV UL from case points
v <- Coord_Barb_Morad_Simp %>% 
  group_by(`ID Animal`) %>% 
  drop_na() %>% 
  group_modify(~add_row
               (lon=-9.195503158186124, lat=38.7139285562482,.x)) %>% 
  ungroup() %>% 
  dplyr:: select(lon, lat)


# Ora aqui ha outro problema que e nao haver direÃ§oes obviamente do funchal para a fmv pronto 
# Consegui por a funcionarlocalmente, entao nao preciso de andar a dividir a DB, mas tenho de tirar a madeira da equacao
# pontos a tirar: 250 e 251 (251 Ã© madeira acho)

xx <- ors_directions(v)

# Com ors_matrix consegues distancia e tempo para as rotas, e nao tens de ter por ordem
# Unico problema e que tens max de 3500 rotas e eu estou tipo deer in headlights
# va isso e ele da-te tempo e distancia entre todos os santos pontos
# da uma matrix, o que seria de esperar, agr temos de tirar a informacao que e relevante
# Portanto, converter em df e limpar
# ou melhor que isso, como tenho matrix, posso pegar na DB original sÃ³ dos casos/controlos, adicionar uma ultima linha com FMV
# Assim tenho distancias casos/controlo vs fmv numa unica coluna
v1 <- Coord_Barb_Morad_Simp %>% 
  add_row (lon=-9.195503158186124, lat=38.7139285562482) %>% 
  drop_na() %>% 
  dplyr:: select(lon, lat)

ccc <- ors_matrix(v1, metrics = c("duration", "distance"), units = "km") 
(ccc$durations/60) %>%  round(1)
# Selecionar so a ultima coluna e temos distancia ponto 1:n a fmv
# problema: tivemos de dar drop a NA's, o que sigfnifica que dar match vai ser uma tarefa interessante

# Mapa com rotas de carro
# Convem agrupar isto por Casos e Controlos para depois podermos brincar com o mapa
leaflet() %>%
  addTiles() %>%
  addGeoJSON(x, fill=FALSE) %>%
  addGeoJSON(xx, fill=FALSE, color = "Red") %>%
  fitBBox(x$bbox)

# Mapa com rotas a pe
# Fazer o mesmo que o acima, mas definir profile para walking
# Ir ao ficheiro configuracoes e definir parametros
x_foot <- ors_directions(u4,profile="foot-walking")
leaflet() %>% 
  addTiles() %>% 
  addGeoJSON(x_foot, fill=FALSE) %>% 
  fitBBox(x$bbox)


# Mapa com public transport
# So this is fun, o ORS nao tem isso
library(hereR)

set_key("6O70u-gneZ_0CbZj7hDQ0nX88HOHFSmkzgwC9DLGFQ0")

b <- st_as_sf(v5, coords=c(1:2)) %>% 
  st_set_crs(4326) %>% 
  st_transform(crs=32736)

c <- hereR:: connection(
  origin=b[5,],
  destination=b[6,],
  summary=TRUE
)
# Isto da um objeto sf e um df que tem 2 colunas interessantes: distancia (imagino que em m) e tempo (em s)

# Assim tens os diferentes caminhos de TP, mas isto vai ficar uma mess de dar plot para todas pq isto da as alternativas todas de TP
leaflet(c) %>%
  addTiles() %>%
  addPolylines(color= "Green") %>% 
  fitBBox(c$bbox)

# https://geocompr.robinlovelace.net/transport.html
# https://cran.r-project.org/web/packages/hereR/hereR.pdf
# https://developer.here.com/documentation/public-transit/dev_guide/routing/index.html
# https://transitfeeds.com/l/670-portugal
# https://rstudio-pubs-static.s3.amazonaws.com/234589_8188f8d6471f412b94dbc61f7b1aaa16.html
# https://cran.r-project.org/web/packages/tidytransit/vignettes/frequency.html
# https://github.com/ATFutures/gtfs-router
# https://cran.r-project.org/web/packages/gtfsrouter/gtfsrouter.pdf
# https://cran.r-project.org/web/packages/tidytransit/vignettes/introduction.html
# https://platform.here.com/


# Se bem me lembro... o prof queria isto carro/pe/transportes mas isso vai ser uma dor de cabeca
# Este mapa agr acho que e de carro mas opah nao confirmei ainda, estava so a testar


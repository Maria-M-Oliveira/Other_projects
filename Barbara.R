
library(data.table)
library(dplyr)
library(leaflet)
library(rgeos)
library(ggplot2)
library(sf)
library(sp)
library(rgdal)
library(maptools)
library(tidyverse)
  
#Loading and cleaning data
Barb <- fread("Código Postal - Bárbara.csv")
Barbclean <- subset(Barb, select = -c( V4 : V26 )) %>% 
    unique
  
Barbcleanclean <- Barbclean[1:148, ]
    
#Making sure we dont lose the control group, extracting and creating a new df
Controlo <- Barbclean[150:220, ] %>%
  subset(select = -V3) 
  
  
Moradas <- fread("pt_addresses.csv") %>% 
  unique
  
#Grouping and taking the average of the coords (without converting into xy)
Conjuncture_Controlo_Moradas <- merge(Controlo, Moradas[ ,c("city", "postcode", "lon", "lat")],
                                        by.x = "Código postal", 
                                        by.y = "postcode", 
                                        all.x = T) %>% 
  group_by(`ID Animal`) %>%
  unique
    
  
  
Conjuncture_Barb_Moradas <- merge(Barbcleanclean, Moradas[ , c("city", "postcode", "lon", "lat")], 
                      by.x = "Código postal", 
                      by.y = "postcode", 
                      all.x = T) %>% 
   group_by(`ID Animal`) %>%
  unique
  
  
#Plotting the minimum convex polygon, full guide here:
#"https://jamesepaterson.github.io/jamespatersonblog/03_trackingworkshop_homeranges#:~:text=The%20minimum%20convex%20polygon%20"
#Removing NA's
#Remover ID's que tenham menos de 5 obs, mcp não funciona otherwise
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
  
library(adehabitatHR)
  
Controlo.mcp <- mcp(Controlo.sp, percent = 95)  
Barb.mcp <- mcp(Barb.sp, percent = 95)
  
library(scales)  
  
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
                        subset(select = -c(2:4)) %>%
                        merge(Barbcleanclean, by = "ID Animal")

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
                     label = ~`Código postal`,
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
                     label = ~`Código postal`,
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
   
#Uploading and cleaning shape files
PT <-readOGR("Cont_AAD_CAOP2020") 
names(PT)
   
PT_Lisb <- PT[PT$Distrito == "Lisboa",]
PT_Sant <- PT[PT$Distrito == "Santarém",]
PT_Evor <- PT[PT$Distrito == "?vora",]
PT_Setu <- PT[PT$Distrito == "Setúbal",]
PT_Mad <- readOGR("ArqMadeira_AAd_CAOP2020") 
  
PT_Clean <- rbind(PT_Lisb, PT_Evor) %>% 
     rbind(PT_Sant) %>% 
     rbind(PT_Setu)
   

# Mapping routes and such
# 1st isochrone map from FMV-UL
# 1- 20min distance; 2-40min; 3-60min (by car)

library(openrouteservice)
library(mapview)

# Cada key e pessoal, ver se funciona no teu tb
ors_api_key("5b3ce3597851110001cf6248a219fc66105d4bc5bacf80a7fbb1aab0")


mapviewOptions(fgb = FALSE)
coordinates <- data.frame(lon = c(-9.195503158186124) , lat = c(38.71392855624822))
res <- ors_isochrones(coordinates, range = 3600, interval = 1200, output = "sf")
res

values <- levels(factor(res$value))
ranges <- split(res, values)
ranges <- ranges[rev(values)]

names(ranges) <- sprintf("%s min", as.numeric(names(ranges))/60)

mapview(ranges, alpha.regions = 0.2, homebutton = FALSE, legend = FALSE)


# Routing now
# ATENCAO QUE NADA ISTO ESTA A FUNCIONAR, E SO PARA TERES NOCAO DO QUE SE PASSA
# Directions to FMV UL from controlo points
# Careful, coordinates must be in order, meaning i need to add to the dfs FMV's coords in order
Coord_controlo_limpo <- Coord_Control_Morad_Simp %>% 
  dplyr:: select(lon, lat)

FMV_coord <- c(38.71392855624822, -9.195503158186124)

# Tentativas de adicionar as coord em ordem, desisti por agora
# for (row in 1:nrow(Coord_controlo_limpo)) {
#   Coord_controlo_limpo %>% add_row(lat=8.71392855624822, lon= -9.195503158186124, .after= n)
#   print(Coord_controlo_limpo)
# }
# Coord_controlo_limpo[nrow(Coord_controlo_limpo) + 1,] = list(-9.195503158186124, 38.71392855624822)


# Ok so para explicar a confusao
# Entao, com o ORS, tens ummaximo de 70 pedidos que podes fazer at once, entao tive de separar as DB
 
Coord_controlo_limpo_1 <- Coord_controlo_limpo [37:71,]
Coord_controlo_limpo_2 <- Coord_controlo_limpo [1:36,]

x <- ors_directions(Coord_controlo_limpo_2)
y <- ors_directions(Coord_controlo_limpo_1)

# Directions to FMV UL from case points
Coord_Barb_limpo <- select(Coord_Barb_Morad_Simp, lon, lat)
Coord_Barb_limpo1 <- Coord_Barb_limpo [1:70,]  %>% drop_na()
Coord_Barb_limpo2 <- Coord_Barb_limpo [71:140,] %>% drop_na()
Coord_Barb_limpo3 <- Coord_Barb_limpo [141:148,] %>% drop_na()

# Ora aqui ha outro problema que e nao haver direçoes obviamente do funchal para a fmv pronto
xx <- ors_directions(Coord_Barb_limpo1)
yy <- ors_directions(Coord_Barb_limpo2)
zz <- ors_directions(Coord_Barb_limpo3)

# O mapa e possivel fazer, mas o que te vai acontecer antes de se arrumar as coordenadas em ordem, e que vais ter o caminho de uns CP para outros
leaflet() %>%
  addTiles() %>%
  addGeoJSON(x, fill=FALSE) %>%
  addGeoJSON(y, fill=FALSE) %>%
  addGeoJSON(xx, fill=FALSE) %>%
  # addGeoJSON(yy, fill=FALSE) %>%
  addGeoJSON(zz, fill=FALSE) %>%
  fitBBox(x$bbox)

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
Barb <- fread(".\\Barbara\\Código Postal - Bárbara.csv")
Barbclean <- subset(Barb, select = -c( V4 : V26 )) %>% 
    unique
  
Barbcleanclean <- Barbclean[1:148, ]
    
#Making sure we dont lose the control group, extracting and creating a new df
Controlo <- Barbclean[150:220, ] %>%
  subset(select = -V3) 
  
  
Moradas <- fread(".\\Barbara\\pt_addresses.csv") %>% 
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
PT <-readOGR(".\\Barbara\\Cont_AAD_CAOP2020") 
names(PT)
   
PT_Lisb <- PT[PT$Distrito == "Lisboa",]
PT_Sant <- PT[PT$Distrito == "Santarém",]
PT_Evor <- PT[PT$Distrito == "?vora",]
PT_Setu <- PT[PT$Distrito == "Setúbal",]
PT_Mad <- readOGR(".\\Barbara\\ArqMadeira_AAd_CAOP2020") 
  
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

# Versao com o ORS
# ATENCAO QUE NADA ISTO ESTA A FUNCIONAR, E SO PARA TERES NOCAO DO QUE SE PASSA
# Directions to FMV UL from controlo points
# Careful, coordinates must be in order, meaning i need to add to the dfs FMV's coords in order

FMV_coord <- c(-9.195503158186124, 38.7139285562482)

u <- Coord_Control_Morad_Simp %>% 
  group_by(`ID Animal`) %>% 
  group_modify(~add_row
                   (lon=-9.195503158186124, lat=38.7139285562482,.x)) %>% 
  ungroup()
u2 <- dplyr:: select(u, lon, lat)

# Ok so para explicar a confusao
# Entao, com o ORS, tens ummaximo de 70 pedidos que podes fazer at once, entao tive de separar as DB

 
u3 <- u2 [71:140,]
u4 <- u2 [1:70,]
u5 <- u2 [141:142,]

x <- ors_directions(u4)
y <- ors_directions(u3)
z <- ors_directions(u5)

# Directions to FMV UL from case points
v <- Coord_Barb_Morad_Simp %>% 
  group_by(`ID Animal`) %>% 
  drop_na() %>% 
  group_modify(~add_row
               (lon=-9.195503158186124, lat=38.7139285562482,.x)) %>% 
  ungroup() %>% 
  dplyr:: select(lon, lat)

v1 <- v [1:70,]  
v2 <- v [71:140,] 
v3 <- v [141:210,] 
v4 <- v [211:280,]
v5 <- v [281:286,]

# Ora aqui ha outro problema que e nao haver direçoes obviamente do funchal para a fmv pronto (acho que é o yy)
xx <- ors_directions(v1)
yy <- ors_directions(v2)
zz <- ors_directions(v3)
xxx <- ors_directions(v4) #Este tem a madeira e ele obvio que nao consegue dar compute ne pois
yyy <- ors_directions(v5)

# Mapa com rotas de carro
# Convem agrupar isto por Casos e Controlos para depois podermos brincar com o mapa
leaflet() %>%
  addTiles() %>%
  addGeoJSON(x, fill=FALSE) %>%
  addGeoJSON(y, fill=FALSE) %>%
  addGeoJSON(z, fill=FALSE) %>%
  addGeoJSON(xx, fill=FALSE, color = "Red") %>%
  addGeoJSON(yy, fill=FALSE, color = "Red") %>%
  addGeoJSON(zz, fill=FALSE, color = "Red") %>%
  addGeoJSON(yyy, fill=FALSE, color = "Red") %>% 
  fitBBox(x$bbox)

# Mapa com rotas a pe
# Fazer o mesmo que o acima, mas definir profile para walking
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
  origin=b[1,],
  destination=b[2,],
  summary=TRUE
)

plot(c)

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





# Routing tentativa numero 2 
# Ha outros packages: r5r e stplanr; ambos com capacidade de representar graficamente os dados
# Ora agr vem os problemas:
# No r5r tens de alocar mais memoria (que eles explicam como), mas esta a dar-me dor de cabeca mexer com isto
# No stplanr tb e confuso como fazes multiplas rotas

# Tentar com o stplanr
# Isto nem e bem o stplanr pq estava a dar erro, so peguei mesmo no osrm ate agr pq e esperto enough para pegar na 1a linha da tabela
library(osrm)
library(stplanr)

trip <- osrmRoute(
  FMV_coord,
  dst= Coord_controlo_limpo[],
  returnclass = "sf"
)


mapview::mapview(trip)
mapview::mapview(trip2)
# Consegui fazer 1 rota certinha pelo menos, agr e tentar juntar todas as rotas no mesmo mapa
# https://github.com/ropensci/stplanr 
# https://cran.r-project.org/web/packages/stplanr/stplanr.pdf
# Se bem me lembro... o prof queria isto carro/pe/transportes mas isso vai ser uma dor de cabeca
# Este mapa agr acho que e de carro mas opah nao confirmei ainda, estava so a testar


library(tidyverse)
library(leaflet)
library(openrouteservice)
library(mapview)
library(sp)

API_Key <- ors_api_key("5b3ce3597851110001cf6248a219fc66105d4bc5bacf80a7fbb1aab0")


# Plotting an isochrone from ESHN 

mapviewOptions(fgb = FALSE)
ESHN<- data.frame(lon = c(-9.256104898339965) , lat = c(39.08983507024364))
res <- ors_isochrones(ESHN, range = 1500, interval = 300, output = "sf")
res

values <- levels(factor(res$value))
ranges <- split(res, values)
ranges <- ranges[rev(values)]

names(ranges) <- sprintf("%s min", as.numeric(names(ranges))/60)

ESHN_iso<- mapview(ranges, alpha.regions = 0.2, homebutton = FALSE, legend = FALSE)
ESHN_iso

# adding other relevant schools
Maxial <- data.frame(lon = c(-9.17126663153978) , lat = c(39.14211512622054))
Outeiro <- data.frame(lon = c(-9.180702720365444) , lat = c(39.19046046412656))
Monte_redondo <- data.frame(lon = c(-9.200147572174581) , lat = c(39.11771656538302))
Ereira <- data.frame(lon = c(-9.154956053996555) , lat = c(39.11972513696694))
Matacaes <- data.frame(lon = c(-9.212182112082875) , lat = c(39.092667568333056))
Ramalhal <- data.frame(lon = c(-9.233219816635028) , lat = c(39.14665975564143)) 
Aldeia_grande <-data.frame(lon = c(-9.15510577487542) , lat = c(39.153848821095515)) 
Ameal <- data.frame(lon = c(-9.23636962430571) , lat = c(39.1345848415169)) 
Abrunheira <- data.frame(lon = c(-9.207221648365467) , lat = c(39.13779854872996)) 

# Creating df for routing
schools <- rbind(ESHN,Maxial,ESHN, Outeiro, ESHN, Monte_redondo, ESHN, Ereira, ESHN, Matacaes,ESHN,Ramalhal, ESHN,Aldeia_grande,ESHN,Ameal,ESHN,Abrunheira)

 #Converting schools df so it is usable for mapview  
coordinates(schools) <- c("lon", "lat")
proj4string(schools) <- CRS("+init=epsg:4326")

test<- ors_directions(schools, output = "sf") #output sf so i can use it in mapview to combine iso and route on the same map

mapview(test) + ESHN_iso + mapview(schools)



library(tidyverse)
library(leaflet)
library(openrouteservice)
library(mapview)
library(sp)
library(RColorBrewer)

options(openrouteservice.url = "http://localhost:8080/ors")

# Plotting an isochrone from ESHN 

mapviewOptions(fgb = FALSE)
ESHN<- data.frame(lon = c(-9.256104898339965) , lat = c(39.08983507024364))
res <- ors_isochrones(ESHN, range = 1500, interval = 300, output = "sf")
res

values <- levels(factor(res$value))
ranges <- split(res, values)
ranges <- ranges[rev(values)]

names(ranges) <- sprintf("%s min", as.numeric(names(ranges))/60)

# adding other relevant schools
# Creating df for routing
Escolas <- as.data.frame (tribble(~name, ~lon, ~lat,
                    'ESHN',-9.256104898339965,39.08983507024364,
                    'Maxial',-9.17126663153978 ,39.14211512622054,
                    'ESHN',-9.256104898339965,39.08983507024364,
                    'Outeiro da Cabeça',-9.180702720365444 ,39.19046046412656,
                    'ESHN',-9.256104898339965,39.08983507024364,
                    'Monte Redondo',-9.200147572174581 ,39.11771656538302,
                    'ESHN',-9.256104898339965,39.08983507024364,
                    'Ereira',-9.154956053996555, 39.11972513696694,
                    'ESHN',-9.256104898339965,39.08983507024364,
                    'Matacães',-9.212182112082875 , 39.092667568333056,
                    'ESHN',-9.256104898339965,39.08983507024364,
                    'Ramalhal',-9.233219816635028, 39.14665975564143,
                    'ESHN',-9.256104898339965,39.08983507024364,
                    'Aldeia Grande',-9.15510577487542,39.153848821095515,
                    'ESHN',-9.256104898339965,39.08983507024364,
                    'Ameal', -9.23636962430571, 39.1345848415169,
                    'ESHN',-9.256104898339965,39.08983507024364,
                    'Abrunheira',-9.207221648365467, 39.13779854872996
                    ))
Escolas1 <- Escolas
 #Converting schools df so it is usable for mapview  
coordinates(Escolas1) <- c("lon", "lat")
proj4string(Escolas1) <- CRS("+init=epsg:4326")

Escolas2 <- select(Escolas, lon, lat)
Rotas<- ors_directions(Escolas2, output = "sf") 
#output sf so i can use it in mapview to combine iso and route on the same map

ESHN_iso<- mapview(ranges, alpha.regions = 0.2, homebutton = FALSE, legend = FALSE, col.regions = ("cornflowerblue"))
Map_Rotas <- mapview(Rotas)
Map_Pontos <- mapview(Escolas1, legend= FALSE)

Mapa_final <- ESHN_iso + Map_Rotas + Map_Pontos
Mapa_final@map %>%
  addStaticLabels(Escolas1, Escolas1$name,
                  noHide = TRUE,
                  direction = 'left',
                  textOnly = TRUE,
                  textsize = "15px",
                  style= list("font-weight"="bold"))


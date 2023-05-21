library(rgee)
library(sf)
library(tidyverse)


rgee::ee_Initialize()

temps_ar <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR") %>%
  ee$ImageCollection$filterDate("2017-01-02", "2022-12-31") %>%
  ee$ImageCollection$map(function(x) x$select("temperature_2m_min")) %>% # Select only temperature bands
  ee$ImageCollection$toBands() # from imagecollection to image


faro <- st_read("C:\\Users\\olive\\Documents\\GitHub\\Other_projects\\Cont_AAD_CAOP2021", geometry_column = "geometry")%>% 
  subset(Concelho %in% c("Faro"))
# Subset para os varios distritos ou ele vai rebentar
# Fazer join dos dados mortalidade com CAOP para ter geometria nos dados mortalidade
#mas isto deve rebentar na mesma.. Como dar a volta? 1 por concelho? mesmo assim parece lento

ee_nc_temp <- ee_extract(
  x = temps_ar,
  y = faro["geometry"], #Isto tem de ser alterado depois para o que quero mesmo
  scale = 250,
  fun = ee$Reducer$mean(),
  sf = TRUE
) 

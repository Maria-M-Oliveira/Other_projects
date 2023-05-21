library(rgee)
rgee::ee_Initialize()

temps_ar <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR") %>%
  ee$ImageCollection$filterDate("2017-01-02", "2022-12-31") %>%
  ee$ImageCollection$map(function(x) x$select("temperature_2m_min")) %>% # Select only temperature bands
  ee$ImageCollection$toBands() # from imagecollection to image

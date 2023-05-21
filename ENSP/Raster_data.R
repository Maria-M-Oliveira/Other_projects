library(ncdf4)
library(lubridate)
library(tidyverse)
library(processNC)
library(terra)
library(raster)


### DATA ###
nc_tmax <- nc_open(".\\ENSP\\tx_ens_mean_0.25deg_reg_2011-2022_v27.0e.nc")
teste <- nc_open(".\\ENSP\\temp_max_2017.nc")


### DATA CLEANING ###
dim_lon <- ncvar_get(nc_tmax, "longitude")
dim_lat <- ncvar_get(nc_tmax, "latitude")
dim_time <- ncvar_get(nc_tmax, "time")
dim_temp <- ncvar_get(nc_tmax, "tx", collapse_degen=FALSE)
fillvalue <- ncatt_get(nc_tmax, "tx", "_FillValue")
dim_temp[dim_temp==fillvalue$value] <- NA


t_units <- ncatt_get(nc_tmax, "time", "units") #days since 1950-01-01 00:00
t_ustr <- strsplit(t_units$value, " ")
t_dstr <- strsplit(unlist(t_ustr)[3], "-")
date <- ymd(t_dstr) + ddays(dim_time)
date
time_obs<- as.POSIXct(date, origin = "1950-01-01", tz="GMT")


lswt_slice <- dim_temp[ , , 21] 
lswt_slice <- dim_temp[ , , 25]

# and why not, draw it out:
image(dim_lon, dim_lat, lswt_slice)


lswt_vec_long <- as.vector(dim_temp)
gc()
lonlattime <- as.matrix(expand.grid(dim_lon,dim_lat,time_obs))
lswt_obs <- data.frame(cbind(lonlattime, lswt_vec_long))


colnames(lswt_obs) <- c("Long","Lat","Date","Temp_Cel")
head(lswt_obs)

lswt_final <- lswt_obs
lswt_final$Date <- as.Date(lswt_final$Date)
lswt_final$Temp_Cel<- as.double(lswt_final$Temp_Cel)


lswt_final

dim(lswt_final)
lswt_final <- lswt_final %>%
  group_by(Date)



### testes ###
library(sf)
library(leaflet)
library(rasterVis)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)

sst_single <- raster("C:\\Users\\olive\\Documents\\GitHub\\Other_projects\\ENSP\\temp_max_2017.nc")

plot(sst_single)

sst_multi <-brick("C:\\Users\\olive\\Documents\\GitHub\\Other_projects\\ENSP\\temp_max_2017.nc")

sst_mean <-calc(sst_multi, fun = mean)
sst_sd <-calc(sst_multi, fun = sd)
plot(sst_mean, main = "Average SST")

sst_df <-as.data.frame(sst_mean, xy=TRUE, na.rm=TRUE)

countries <-ne_countries(scale = "medium", returnclass = "sf")
ggplot()+# add raster layer
  geom_raster(aes(x=x, y=y, fill=layer), data=sst_df)+
  # define color palette of raster layer
  scale_fill_distiller(palette = "Spectral", name = "SST (ºC)")+
  # add countries layers
  geom_sf(fill=grey(0.9), color=grey(0.6), lwd = 0.2, data=countries)+
  # define spatial extent
  coord_sf(xlim =range(sst_df$x), ylim =range(sst_df$y), expand = F, ndiscr = 500)+
  # labels
  labs(title = "Sea Surface Temperature (SST)",subtitle = "Annual average estimated from monthly products for 2020",x = "Longitude",y = "Latitude")+
  # theme
  theme_bw()

date_sst <- sst_multi%>%
  # get time stamps from multi raster
  getZ()%>%
  # parse character to POSIXct class (time)
  parse_date_time("Ymd HMS")%>%
  # get the first day of each month
  floor_date("month")

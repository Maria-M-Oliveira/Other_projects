library(ncdf4)
library(lubridate)
library(tidyverse)
library(processNC)
library(terra)
library(raster)


### DATA ###
nc_tmax <- nc_open("./ENSP/tx_ens_mean_0.25deg_reg_2011-2022_v27.0e.nc")


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


lswt_slice <- dim_temp[ , , 2123] 
lswt_slice <- dim_temp[ , , 25]

# and why not, draw it out:
image(dim_lon, dim_lat, lswt_slice)


lswt_vec_long <- as.vector(dim_temp)
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
  group_by(Date) %>%
  summarize(Mean_K = mean(Temp_Cel))

write.csv(as.data.frame(lswt_final), "GloboLakes_Atitlan_TS_95_16.csv", row.names=T)



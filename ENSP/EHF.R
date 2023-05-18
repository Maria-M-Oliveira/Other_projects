### Libraries ###



### Data ###
# Climate data from 1971 to 2000 to establish 95th percentil of Daily Mean Temperature (DMT)
# clima <- rgee ERA5-Land Daily from 1971 to 2000; coordinates for  Continental Portugal
# Might be useful getting the national official map CAOP for coordinates 
# Temperature data for a given year to test EHF function (lets say 2019)


### Functions ###

# Starting with 95th percentil determination
# ninety <- quantile(clima, probs =  95)
# Would it be useful to determine for each concelho ? 
# if so, join gee data with CAOP and group by concelho. 95th with summarise function  
# summarise(percent95 = quantile(xxx, probs = .95))

# EHI sig next
# EHI sig = (ti+ ti1+ti2)/3 - t95
# This would probably be an iteration? Checking each date and calculating for each one its EHI sig
#
# for (i in seq_along(clima_19)) {
#   x <- clima_19$temp,
#   y <- ninety,
#   sig <- (x+ (x+1)+(x+2))/3 - y)
# }


# EHI accl next
# for (i in 1:ncol(clima_19)) {
#   x <- clima_19$temp,
#   y <- ninety,
#   accl<- ((x+ (x+1)+(x+2))/3 - y)) - (x[1:30]/30)
# }


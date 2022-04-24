library(raster)
library(rgdal)

## set working directory (in this case where I saved your dem, change this for your file system)
setwd("C:/Users/robcu/Documents/R/Rprojects/microclimate")

## load raster DEM - you only need the .tif file here
dem <- raster("./tif/PredBMarch2018DEM.tif")
plot(dem) ## check to see if DEM looks OK


### calculkate slope and aspect as rasters
slope <- terrain(dem,opt="slope", unit="degrees")
plot(slope) ##check to see if slope looks OK

aspect <- terrain(dem,opt="aspect", unit="degrees")
plot(aspect) ##check to see if aspect looks OK

### sample code for calculating clear-sky solar radiation on slopes
### or a solar index

### set startday and endday to define period of interest - 
### here it is the first week of May (days 122 to 128)

startday <- 80 #20th March
endday <- 90 #30th March

### set time of day to define period of interest
### here it is hourly from 5am to 10pm (local time)

starthour <- 10
endhour <- 16

### if you want to use local time, you can convert as below
### using midday, at the given latitude and longitude with summer time correction (1 hour)
### on day 180
# starthour <- solartime(localtime=12,lat=50,lon=-5,dst=1,day=180)

### you can drive the swrad function below with either a single value, a vector (column in dataframe) 
### or a raster map
### for altitude, slope and aspect - the output will match the input format (single value, list or raster map)
### here we're using a single point value, 30 degrees slope facing due south (180 deg), 50m above sea level

slope <- slope
aspect <- aspect
elev <- dem

### also needs a lat or long - again can be a point, a list, or a raster. Doesn't need to be super accurate.
latitude = 50
longitude = -5

### the loop below calculates total clear-sky radiation for the period given slope and aspect


rad <- 0
for (d in startday:endday) {
  for (h in starthour:endhour) {
    alt <- solalt(time=h,day=d,lat=latitude)
    if(alt>0) {
      beam <- beamrad(day=d,altitude=alt,elevation=elev)
      diffuse <- diffuserad(day=d,altitude=alt)
      rad <- rad + swrad(sw=beam+diffuse,slope=slope,aspect=aspect,lat=latitude,lon=longitude,localtime=h,day=d,elevation=elev,merid=0,dst=0)
    }
  }
}



plot(rad)

### to export output as GIS compatible tif:

writeRaster(rad,"output_rad.tif")
writeRaster(rad,"C:/SR.tif")

### the output 'rad' is the total clear-sky radiation for the point in the landscape

### for a simpler (and much quicker) index at a particular time of day/year, use the solarindex function
### eg. 3pm on the 9th May (129th day of the year if it's not a leap year)
### dst=1 indicates to correct by one hour for British Summertime (Daylight Saving)

soltime <- solartime(15,lat=50,lon=-5,day=129,dst=1)
alt <- solalt(time=15,day=129,lat=50)
azi <- solazi(time=15,day=129,lat=50)
index <- solarindex(slope,aspect,azimuth=azi,altitude=alt)

plot(index)

### the output 'index' represents a value between 0 (total shade) 
### and 1 (sunlight hitting the slope full on)

### to export output as GIS compatible tif:

writeRaster(index,"output_solindex.tif")







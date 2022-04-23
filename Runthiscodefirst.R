#library(gitcreds)
#gitcreds_set()


library(usethis)
use_git()

#Initial code to run
## Solar radiation functions for Pines project - Jon Bennie 5th July 2013

## solar index function - calculates the relative flux (between 0 and 1) on a given slope
## for a given solar altitude and azimuth (see below to calculate aximuth/altitude)
solarindex <- function(slope,aspect,azimuth,altitude)
{
  alt <- altitude * (pi/180)
  zen <- pi/2 - alt
  azi <- azimuth * (pi/180)
  s <- slope * (pi/180)
  a <- aspect * (pi/180)
  index <- cos(zen) * cos(s) + sin(zen) * sin(s) * cos(azi - a)
  index[alt<=0] <- 0
  index[index<=0] <- 0
  index
}

## beam radiation - calculates the clear-sky direct beam solar irradiance
## for a given solar irradiance and atmospheric condition (in Wm-2)

beamrad <- function(day,altitude,elevation=0,linke=3)
{
  solarconstant <- 1367
  dayangle <- (2*pi*day)/365.25
  e <- 1 + 0.03344 * cos(dayangle - 0.048869)
  ## G is extraterrestrial irradiance
  G <- e * solarconstant
  
  deltaalt <- 0.061359 * (0.1594 + 1.123 * altitude + 0.065656 * altitude^2) / (1+ 28.9344 * altitude + 277.3971 * altitude^2)
  href <- altitude + deltaalt
  ppo <- exp(-elevation/8434.5)
  
  ## m is relative optical airmass
  m <- ppo/(sin(href*(pi/180))+0.50572*(href+6.07995)^-1.6364)
  m[which(altitude<=1)] <- 999
  
  ## dr is Rayleigh optical thickness at air mass m
  dr <- 1/(6.6296 + 1.7513 * m - 0.1202 * m^2 + 0.0065 * m^3 - 0.00013 * m^4)
  dr[m<=20] <- 1/(10.4 + 0.718 * m[m<=20])
  
  
  ## B is beam irradiance normal to the solar beam
  B <- G * exp(-0.8662 * linke * m * dr)
  B[altitude<=1] <- 0
  B
}

## diffuse radiation - calculates the clear-sky diffuse beam solar irradiance
## for a given solar irradiance and atmospheric condition (in Wm-2)

diffuserad <- function(day,altitude,linke=3)
{
  solarconstant <- 1367
  dayangle <- (2*pi*day)/365.25
  e <- 1 + 0.03344 * cos(dayangle - 0.048869)
  ## G is extraterrestrial irradiance
  G <- e * solarconstant
  Tn <- -0.015843 + 0.030543 * linke + 0.0003797 * linke^2
  Ai <- 0.26463 - 0.061581 * linke + 0.0031408 * linke^2
  if (Ai < 0.022) Ai <- 0.0022/Tn
  Aii <- 2.04020 + 0.018945 * linke - 0.011161 * linke^2
  Aiii <- -1.3025 + 0.039231 * linke + 0.0085079 * linke^2
  Fd <- Ai + Aii * sin(altitude*pi/180) + Aiii * sin(altitude*pi/180)^2
  D <- G * Tn * Fd
  D[D<=0] <- 0
  D
}


## solar time function - calculates the "solar" time (ie. time as measured by a sundial)
## from local clock time, time zone meridion, summer time adjustment etc...
solartime <- function(localtime, lat, lon, merid=0, dst=0, day)
{
  Bn <- 2 * 3.141 * (day - 81) / 364
  eot <- 9.87 * sin(2 * Bn) - 7.53 * cos(Bn) - 1.5 * sin(Bn)
  solartime <- localtime + (4 / 60) * (2 * 3.141 * (merid - lon) / 360) + (eot / 60) - dst
  solartime
}

## solar altitude - calculates the angular altitude (in degrees) of the sun above the horizon for
## a given (solar) time, day of year (0 to 365) and latitude

solalt <- function(time, day, lat, obliquity=23.5)
{
  t <- 0.261799 * (time - 12)
  declin <- (pi * obliquity / 180) * cos(2 * pi * ((day - 171) / 365.25))
  Sinh = sin(declin) * sin(lat * pi / 180) + cos(declin) * cos(lat * 3.141 / 180) * cos(t)
  solalt = (180 * atan(Sinh / sqrt(1 - Sinh * Sinh))) / pi
  solalt
}

## solar altitude - calculates the angular azimuth (in degrees) of the sun for
## a given (solar) time, day of year (0 to 365) and latitude

solazi <- function(time, day, lat)
{
  t = 0.261799 * (time - 12)
  declin = (pi * 23.5 / 180) * cos(2 * pi * ((day - 171) / 365.25))
  Sinh = sin(declin) * sin(lat * pi / 180) + cos(declin) * cos(lat * pi / 180) * cos(t)
  h = (atan(Sinh / sqrt(1 - Sinh * Sinh)))
  Sinazi = cos(declin) * sin(t) / cos(h)
  cosazi = (sin(lat * pi / 180) * cos(declin) * cos(t) - cos(pi * lat / 180) * sin(declin)) / sqrt((cos(declin) * sin(t)) ^ 2 + (sin(pi * lat / 180) * cos(declin) * cos(t) - cos(pi * lat / 180) * sin(declin)) ^ 2)
  solazi = 180 + (180 * atan(Sinazi / sqrt(1 - Sinazi * Sinazi))) / pi
  solazi[which(cosazi<0&Sinazi<0)] <- 180-solazi[which(cosazi<0&Sinazi<0)]
  solazi[which(cosazi<0&Sinazi>=0)] <- 540-solazi[which(cosazi<0&Sinazi>=0)]
  solazi
}

## short wave radiation - estimates the short wave radiation on a slope
## given the measured short wave radiation on a horizontal surface

swrad <- function(sw,slope,aspect,lat,lon,localtime,day,elevation=0,merid=0,dst=0)
{
  soltime <- solartime(localtime,lat,lon,merid,dst,day)
  alt <- solalt(localtime,day,lat)
  azi <- solazi(localtime,day,lat)	
  beam <- beamrad(day,alt,elevation,linke=3)
  diff <- diffuserad(day,alt,linke=3)
  swflux <- beam * solarindex(0,0,azi,alt) + diff
  slopeflux <- beam * solarindex(slope,aspect,azi,alt) + diff
  clearsky <- (sw - 0.2 * diff) / (swflux - 0.2 * diff)
  swrad <- 0.2 * diff + clearsky * (slopeflux - 0.2 * diff)
  swrad
}

## horizon angle - calculates the tangent of the angle to the horizon for a given set of angles

horizon.angle <- function(dtm,segments=c(1:36)*10,min.dist=1,max.dist=64,base=2)
{
  elev <- array(values(dtm),c(dim(dtm)[2],dim(dtm)[1]))
  nsteps <- ceiling(log(max.dist)/log(base))+1
  steps <- base^c(0:nsteps)
  test.block1 <- array(0,c(dim(dtm)[2],dim(dtm)[1],nsteps))
  angle.block <- array(0,c(dim(dtm)[2],dim(dtm)[1],length(segments)))
  seg.rads <- 2*pi*(segments/360)
  count <- 0
  for(angle in seg.rads) {
    count <- count+1
    for(step in 1:nsteps) {
      xshift <- round(step*sin(angle))
      yshift <- -round(step*cos(angle))
      test.block2 <- array(0,c(dim(dtm)[2],dim(dtm)[1]))
      test.block2[max(1,1-xshift):min(dim(dtm)[2],dim(dtm)[2]-xshift),max(1,1-yshift):min(dim(dtm)[1],dim(dtm)[1]-yshift)] <- elev[max(1,1+xshift):min(dim(dtm)[2],dim(dtm)[2]+xshift),max(1,1+yshift):min(dim(dtm)[1],dim(dtm)[1]+yshift)]
      test.block1[,,step] <- (test.block2-elev)/(steps[step]*xres(dtm))
    }
    angle.block[,,count] <- apply(test.block1,c(1,2),max)
  }
  angle.block[angle.block<0] <- 0
  angle.block
}

## toposhadow  - calculates a mask of shadow given a solar azimuth and altitude

toposhadow <- function(hor.angle,sol.alt,sol.azi,segments=c(1:36)*10)
{
  diff.angle <- abs(sol.azi-segments)%%360
  diff.angle[diff.angle>180] <- 360-diff.angle[diff.angle>180]
  min1 <- min(diff.angle)
  min.idx1 <- which.min(diff.angle)
  diff.angle[min.idx1] <- Inf
  min2 <- min(diff.angle)
  min.idx2 <- which.min(diff.angle)
  angle <- (min1*atan(hor.angle[,,min.idx1])+min2*atan(hor.angle[,,min.idx2]))/(min1+min2)
  angle <- (360*angle)/(2*pi)
  shadow <- array(0,dim(hor.angle)[1:2])
  shadow[angle<sol.alt] <- 1
  shadow
}


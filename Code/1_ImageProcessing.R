#-------------------------------------------------------------------------------
#Load driver
source("0_Driver.R")
#-------------------------------------------------------------------------------
#Create empty lists
TAMSATMonth.raster <- list()
#-------------------------------------------------------------------------------
#Load TAMSAT Monthly Data
#-------------------------------------------------------------------------------
#Get variables 
lon <- ncvar_get(TAMSATMonth.nc, "X")
lat <- ncvar_get(TAMSATMonth.nc, "Y")
time <- ncvar_get(TAMSATMonth.nc, "T")
rainfall <- ncvar_get(TAMSATMonth.nc, "rfe")

tic("Loading all TAMSAT Months")
for(i in 1:480){
  TAMSATMonth.raster[[i]] <- raster(rainfall[, , i], xmn=min(lat), xmx=max(lat), 
                                    ymn=min(lon), ymx=max(lon), 
                                    crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  TAMSATMonth.raster[[i]] <- t(TAMSATMonth.raster[[i]])
  TAMSATMonth.raster[[i]] <- flip(TAMSATMonth.raster[[i]], 2)
}
toc()
#-------------------------------------------------------------------------------
#Crop Datasets
#-------------------------------------------------------------------------------
tic("Crop Raster to Ghana Extent")
TAMSATMonth.raster <- foreach(n = 1:length(TAMSATMonth.raster)) %dopar% CropRaster(TAMSATMonth.raster[[n]], Ghana)
toc()
#-------------------------------------------------------------------------------
#Part2: Raster Calculations
#-------------------------------------------------------------------------------
#Designate order of months with number
Jan <- 1
Feb <- 2
Mar <- 3
Apr <- 4
M <- 5
Jun <- 6
Jul <- 7
Aug <- 8
Sep <- 9
Oct <- 10
Nov <- 11
Dec <- 12
#-------------------------------------------------------------------------------
#Part2.2: Create mean monthly rainfall rasters
#-------------------------------------------------------------------------------
#Get sums of each months
JanuaryMean <- mean(TAMSATMonth.raster[[Jan+12]], TAMSATMonth.raster[[Jan+2*12]], TAMSATMonth.raster[[Jan+3*12]],
               TAMSATMonth.raster[[Jan+4*12]], TAMSATMonth.raster[[Jan+5*12]], TAMSATMonth.raster[[Jan+7*12]],
               TAMSATMonth.raster[[Jan+8*12]], TAMSATMonth.raster[[Jan+9*12]], TAMSATMonth.raster[[Jan+10*12]], TAMSATMonth.raster[[Jan+11*12]],
               TAMSATMonth.raster[[Jan+12*12]], TAMSATMonth.raster[[Jan+13*12]], TAMSATMonth.raster[[Jan+14*12]], TAMSATMonth.raster[[Jan+15*12]],
               TAMSATMonth.raster[[Jan+16*12]], TAMSATMonth.raster[[Jan+17*12]], TAMSATMonth.raster[[Jan+18*12]], TAMSATMonth.raster[[Jan+19*12]],
               TAMSATMonth.raster[[Jan+20*12]], TAMSATMonth.raster[[Jan+21*12]], TAMSATMonth.raster[[Jan+22*12]], TAMSATMonth.raster[[Jan+23*12]],
               TAMSATMonth.raster[[Jan+24*12]], TAMSATMonth.raster[[Jan+25*12]], TAMSATMonth.raster[[Jan+26*12]], TAMSATMonth.raster[[Jan+27*12]],
               TAMSATMonth.raster[[Jan+28*12]], TAMSATMonth.raster[[Jan+29*12]], TAMSATMonth.raster[[Jan+30*12]], TAMSATMonth.raster[[Jan+31*12]],
               TAMSATMonth.raster[[Jan+32*12]], TAMSATMonth.raster[[Jan+33*12]], TAMSATMonth.raster[[Jan+34*12]], TAMSATMonth.raster[[Jan+35*12]],
               TAMSATMonth.raster[[Jan+36*12]], TAMSATMonth.raster[[Jan+37*12]], TAMSATMonth.raster[[Jan+38*12]], TAMSATMonth.raster[[Jan+39*12]])

FebruaryMean <- mean(TAMSATMonth.raster[[Feb]], TAMSATMonth.raster[[Feb+12]], TAMSATMonth.raster[[Feb+3*12]],
               TAMSATMonth.raster[[Feb+4*12]], TAMSATMonth.raster[[Feb+5*12]], TAMSATMonth.raster[[Feb+6*12]], TAMSATMonth.raster[[Feb+7*12]],
               TAMSATMonth.raster[[Feb+8*12]], TAMSATMonth.raster[[Feb+10*12]], TAMSATMonth.raster[[Feb+11*12]],
               TAMSATMonth.raster[[Feb+12*12]], TAMSATMonth.raster[[Feb+13*12]], TAMSATMonth.raster[[Feb+14*12]], TAMSATMonth.raster[[Feb+15*12]],
               TAMSATMonth.raster[[Feb+16*12]], TAMSATMonth.raster[[Feb+17*12]], TAMSATMonth.raster[[Feb+18*12]], TAMSATMonth.raster[[Feb+19*12]],
               TAMSATMonth.raster[[Feb+20*12]], TAMSATMonth.raster[[Feb+21*12]], TAMSATMonth.raster[[Feb+22*12]], TAMSATMonth.raster[[Feb+23*12]],
               TAMSATMonth.raster[[Feb+24*12]], TAMSATMonth.raster[[Feb+25*12]], TAMSATMonth.raster[[Feb+26*12]], TAMSATMonth.raster[[Feb+27*12]],
               TAMSATMonth.raster[[Feb+28*12]], TAMSATMonth.raster[[Feb+29*12]], TAMSATMonth.raster[[Feb+30*12]], TAMSATMonth.raster[[Feb+31*12]],
               TAMSATMonth.raster[[Feb+32*12]], TAMSATMonth.raster[[Feb+33*12]], TAMSATMonth.raster[[Feb+34*12]], TAMSATMonth.raster[[Feb+35*12]],
               TAMSATMonth.raster[[Feb+36*12]], TAMSATMonth.raster[[Feb+37*12]], TAMSATMonth.raster[[Feb+38*12]], TAMSATMonth.raster[[Feb+39*12]]) 


MarchMean <- mean(TAMSATMonth.raster[[Mar]], TAMSATMonth.raster[[Mar+12]], TAMSATMonth.raster[[Mar+2*12]], TAMSATMonth.raster[[Mar+3*12]],
             TAMSATMonth.raster[[Mar+4*12]], TAMSATMonth.raster[[Mar+5*12]], TAMSATMonth.raster[[Mar+6*12]], TAMSATMonth.raster[[Mar+7*12]],
             TAMSATMonth.raster[[Mar+8*12]], TAMSATMonth.raster[[Mar+9*12]], TAMSATMonth.raster[[Mar+10*12]], TAMSATMonth.raster[[Mar+11*12]],
             TAMSATMonth.raster[[Mar+12*12]], TAMSATMonth.raster[[Mar+13*12]], TAMSATMonth.raster[[Mar+14*12]], TAMSATMonth.raster[[Mar+15*12]],
             TAMSATMonth.raster[[Mar+16*12]], TAMSATMonth.raster[[Mar+17*12]], TAMSATMonth.raster[[Mar+18*12]], TAMSATMonth.raster[[Mar+19*12]],
             TAMSATMonth.raster[[Mar+20*12]], TAMSATMonth.raster[[Mar+21*12]], TAMSATMonth.raster[[Mar+22*12]], TAMSATMonth.raster[[Mar+23*12]],
             TAMSATMonth.raster[[Mar+24*12]], TAMSATMonth.raster[[Mar+25*12]], TAMSATMonth.raster[[Mar+26*12]], TAMSATMonth.raster[[Mar+27*12]],
             TAMSATMonth.raster[[Mar+28*12]], TAMSATMonth.raster[[Mar+29*12]], TAMSATMonth.raster[[Mar+30*12]], TAMSATMonth.raster[[Mar+31*12]],
             TAMSATMonth.raster[[Mar+32*12]], TAMSATMonth.raster[[Mar+33*12]], TAMSATMonth.raster[[Mar+34*12]], TAMSATMonth.raster[[Mar+35*12]],
             TAMSATMonth.raster[[Mar+36*12]], TAMSATMonth.raster[[Mar+37*12]], TAMSATMonth.raster[[Mar+38*12]], TAMSATMonth.raster[[Mar+39*12]])

AprilMean <- mean(TAMSATMonth.raster[[Apr]], TAMSATMonth.raster[[Apr+12]], TAMSATMonth.raster[[Apr+2*12]], TAMSATMonth.raster[[Apr+3*12]],
             TAMSATMonth.raster[[Apr+4*12]], TAMSATMonth.raster[[Apr+5*12]], TAMSATMonth.raster[[Apr+6*12]], TAMSATMonth.raster[[Apr+7*12]],
             TAMSATMonth.raster[[Apr+8*12]], TAMSATMonth.raster[[Apr+9*12]], TAMSATMonth.raster[[Apr+10*12]], TAMSATMonth.raster[[Apr+11*12]],
             TAMSATMonth.raster[[Apr+12*12]], TAMSATMonth.raster[[Apr+13*12]], TAMSATMonth.raster[[Apr+14*12]], TAMSATMonth.raster[[Apr+15*12]],
             TAMSATMonth.raster[[Apr+16*12]], TAMSATMonth.raster[[Apr+17*12]], TAMSATMonth.raster[[Apr+18*12]], TAMSATMonth.raster[[Apr+19*12]],
             TAMSATMonth.raster[[Apr+20*12]], TAMSATMonth.raster[[Apr+21*12]], TAMSATMonth.raster[[Apr+22*12]], TAMSATMonth.raster[[Apr+23*12]],
             TAMSATMonth.raster[[Apr+24*12]], TAMSATMonth.raster[[Apr+25*12]], TAMSATMonth.raster[[Apr+26*12]], TAMSATMonth.raster[[Apr+27*12]],
             TAMSATMonth.raster[[Apr+28*12]], TAMSATMonth.raster[[Apr+29*12]], TAMSATMonth.raster[[Apr+30*12]], TAMSATMonth.raster[[Apr+31*12]],
             TAMSATMonth.raster[[Apr+32*12]], TAMSATMonth.raster[[Apr+33*12]], TAMSATMonth.raster[[Apr+34*12]], TAMSATMonth.raster[[Apr+35*12]],
             TAMSATMonth.raster[[Apr+36*12]], TAMSATMonth.raster[[Apr+37*12]], TAMSATMonth.raster[[Apr+38*12]], TAMSATMonth.raster[[Apr+39*12]]) 

MayMean <- mean(TAMSATMonth.raster[[M]], TAMSATMonth.raster[[M+12]], TAMSATMonth.raster[[M+2*12]], TAMSATMonth.raster[[M+3*12]],
           TAMSATMonth.raster[[M+4*12]], TAMSATMonth.raster[[M+5*12]], TAMSATMonth.raster[[M+6*12]], TAMSATMonth.raster[[M+7*12]],
           TAMSATMonth.raster[[M+8*12]], TAMSATMonth.raster[[M+9*12]], TAMSATMonth.raster[[M+10*12]], TAMSATMonth.raster[[M+11*12]],
           TAMSATMonth.raster[[M+12*12]], TAMSATMonth.raster[[M+13*12]], TAMSATMonth.raster[[M+14*12]], TAMSATMonth.raster[[M+15*12]],
           TAMSATMonth.raster[[M+16*12]], TAMSATMonth.raster[[M+17*12]], TAMSATMonth.raster[[M+18*12]], TAMSATMonth.raster[[M+19*12]],
           TAMSATMonth.raster[[M+20*12]], TAMSATMonth.raster[[M+21*12]], TAMSATMonth.raster[[M+22*12]], TAMSATMonth.raster[[M+23*12]],
           TAMSATMonth.raster[[M+24*12]], TAMSATMonth.raster[[M+25*12]], TAMSATMonth.raster[[M+26*12]], TAMSATMonth.raster[[M+27*12]],
           TAMSATMonth.raster[[M+28*12]], TAMSATMonth.raster[[M+29*12]], TAMSATMonth.raster[[M+30*12]], TAMSATMonth.raster[[M+31*12]],
           TAMSATMonth.raster[[M+32*12]], TAMSATMonth.raster[[M+33*12]], TAMSATMonth.raster[[M+34*12]], TAMSATMonth.raster[[M+35*12]],
           TAMSATMonth.raster[[M+36*12]], TAMSATMonth.raster[[M+37*12]], TAMSATMonth.raster[[M+38*12]], TAMSATMonth.raster[[M+39*12]])

JuneMean <- mean(TAMSATMonth.raster[[Jun]], TAMSATMonth.raster[[Jun+12]], TAMSATMonth.raster[[Jun+2*12]], TAMSATMonth.raster[[Jun+3*12]],
            TAMSATMonth.raster[[Jun+4*12]], TAMSATMonth.raster[[Jun+5*12]], TAMSATMonth.raster[[Jun+6*12]], TAMSATMonth.raster[[Jun+7*12]],
            TAMSATMonth.raster[[Jun+8*12]], TAMSATMonth.raster[[Jun+9*12]], TAMSATMonth.raster[[Jun+10*12]], TAMSATMonth.raster[[Jun+11*12]],
            TAMSATMonth.raster[[Jun+12*12]], TAMSATMonth.raster[[Jun+13*12]], TAMSATMonth.raster[[Jun+14*12]], TAMSATMonth.raster[[Jun+15*12]],
            TAMSATMonth.raster[[Jun+16*12]], TAMSATMonth.raster[[Jun+17*12]], TAMSATMonth.raster[[Jun+18*12]], TAMSATMonth.raster[[Jun+19*12]],
            TAMSATMonth.raster[[Jun+20*12]], TAMSATMonth.raster[[Jun+21*12]], TAMSATMonth.raster[[Jun+22*12]], TAMSATMonth.raster[[Jun+23*12]],
            TAMSATMonth.raster[[Jun+24*12]], TAMSATMonth.raster[[Jun+25*12]], TAMSATMonth.raster[[Jun+26*12]], TAMSATMonth.raster[[Jun+27*12]],
            TAMSATMonth.raster[[Jun+28*12]], TAMSATMonth.raster[[Jun+29*12]], TAMSATMonth.raster[[Jun+30*12]], TAMSATMonth.raster[[Jun+31*12]],
            TAMSATMonth.raster[[Jun+32*12]], TAMSATMonth.raster[[Jun+33*12]], TAMSATMonth.raster[[Jun+34*12]], TAMSATMonth.raster[[Jun+35*12]],
            TAMSATMonth.raster[[Jun+36*12]], TAMSATMonth.raster[[Jun+37*12]], TAMSATMonth.raster[[Jun+38*12]], TAMSATMonth.raster[[Jun+39*12]])

JulyMean <- mean(TAMSATMonth.raster[[Jul]], TAMSATMonth.raster[[Jul+12]], TAMSATMonth.raster[[Jul+2*12]], TAMSATMonth.raster[[Jul+3*12]],
            TAMSATMonth.raster[[Jul+4*12]], TAMSATMonth.raster[[Jul+5*12]], TAMSATMonth.raster[[Jul+6*12]], TAMSATMonth.raster[[Jul+7*12]],
            TAMSATMonth.raster[[Jul+8*12]], TAMSATMonth.raster[[Jul+9*12]], TAMSATMonth.raster[[Jul+10*12]], TAMSATMonth.raster[[Jul+11*12]],
            TAMSATMonth.raster[[Jul+12*12]], TAMSATMonth.raster[[Jul+13*12]], TAMSATMonth.raster[[Jul+14*12]], TAMSATMonth.raster[[Jul+15*12]],
            TAMSATMonth.raster[[Jul+16*12]], TAMSATMonth.raster[[Jul+17*12]], TAMSATMonth.raster[[Jul+18*12]], TAMSATMonth.raster[[Jul+19*12]],
            TAMSATMonth.raster[[Jul+20*12]], TAMSATMonth.raster[[Jul+21*12]], TAMSATMonth.raster[[Jul+22*12]], TAMSATMonth.raster[[Jul+23*12]],
            TAMSATMonth.raster[[Jul+24*12]], TAMSATMonth.raster[[Jul+25*12]], TAMSATMonth.raster[[Jul+26*12]], TAMSATMonth.raster[[Jul+27*12]],
            TAMSATMonth.raster[[Jul+28*12]], TAMSATMonth.raster[[Jul+29*12]], TAMSATMonth.raster[[Jul+30*12]], TAMSATMonth.raster[[Jul+31*12]],
            TAMSATMonth.raster[[Jul+32*12]], TAMSATMonth.raster[[Jul+33*12]], TAMSATMonth.raster[[Jul+34*12]], TAMSATMonth.raster[[Jul+35*12]],
            TAMSATMonth.raster[[Jul+36*12]], TAMSATMonth.raster[[Jul+37*12]], TAMSATMonth.raster[[Jul+38*12]], TAMSATMonth.raster[[Jul+39*12]])

AugustMean <- mean(TAMSATMonth.raster[[Aug]], TAMSATMonth.raster[[Aug+12]], TAMSATMonth.raster[[Aug+2*12]], TAMSATMonth.raster[[Aug+3*12]],
              TAMSATMonth.raster[[Aug+4*12]], TAMSATMonth.raster[[Aug+5*12]], TAMSATMonth.raster[[Jul+6*12]], TAMSATMonth.raster[[Aug+7*12]],
              TAMSATMonth.raster[[Aug+8*12]], TAMSATMonth.raster[[Aug+9*12]], TAMSATMonth.raster[[Jul+10*12]], TAMSATMonth.raster[[Aug+11*12]],
              TAMSATMonth.raster[[Aug+12*12]], TAMSATMonth.raster[[Aug+13*12]], TAMSATMonth.raster[[Jul+14*12]], TAMSATMonth.raster[[Aug+15*12]],
              TAMSATMonth.raster[[Aug+16*12]], TAMSATMonth.raster[[Aug+17*12]], TAMSATMonth.raster[[Jul+18*12]], TAMSATMonth.raster[[Aug+19*12]],
              TAMSATMonth.raster[[Aug+20*12]], TAMSATMonth.raster[[Aug+21*12]], TAMSATMonth.raster[[Jul+22*12]], TAMSATMonth.raster[[Aug+23*12]],
              TAMSATMonth.raster[[Aug+24*12]], TAMSATMonth.raster[[Aug+25*12]], TAMSATMonth.raster[[Jul+26*12]], TAMSATMonth.raster[[Aug+27*12]],
              TAMSATMonth.raster[[Aug+28*12]], TAMSATMonth.raster[[Aug+29*12]], TAMSATMonth.raster[[Jul+30*12]], TAMSATMonth.raster[[Aug+31*12]],
              TAMSATMonth.raster[[Aug+32*12]], TAMSATMonth.raster[[Aug+33*12]], TAMSATMonth.raster[[Jul+34*12]], TAMSATMonth.raster[[Aug+35*12]],
              TAMSATMonth.raster[[Aug+36*12]], TAMSATMonth.raster[[Aug+37*12]], TAMSATMonth.raster[[Jul+38*12]], TAMSATMonth.raster[[Aug+39*12]])


SeptemberMean <- mean(TAMSATMonth.raster[[Sep]], TAMSATMonth.raster[[Sep+12]], TAMSATMonth.raster[[Sep+2*12]], TAMSATMonth.raster[[Sep+3*12]],
                 TAMSATMonth.raster[[Sep+4*12]], TAMSATMonth.raster[[Sep+5*12]], TAMSATMonth.raster[[Sep+7*12]],
                 TAMSATMonth.raster[[Sep+8*12]], TAMSATMonth.raster[[Sep+9*12]], TAMSATMonth.raster[[Sep+10*12]], TAMSATMonth.raster[[Sep+11*12]],
                 TAMSATMonth.raster[[Sep+12*12]], TAMSATMonth.raster[[Sep+13*12]], TAMSATMonth.raster[[Sep+14*12]], TAMSATMonth.raster[[Sep+15*12]],
                 TAMSATMonth.raster[[Sep+16*12]], TAMSATMonth.raster[[Sep+17*12]], TAMSATMonth.raster[[Sep+18*12]], TAMSATMonth.raster[[Sep+19*12]],
                 TAMSATMonth.raster[[Sep+20*12]], TAMSATMonth.raster[[Sep+21*12]], TAMSATMonth.raster[[Sep+22*12]], TAMSATMonth.raster[[Sep+23*12]],
                 TAMSATMonth.raster[[Sep+24*12]], TAMSATMonth.raster[[Sep+25*12]], TAMSATMonth.raster[[Sep+26*12]], TAMSATMonth.raster[[Sep+27*12]],
                 TAMSATMonth.raster[[Sep+28*12]], TAMSATMonth.raster[[Sep+29*12]], TAMSATMonth.raster[[Sep+30*12]], TAMSATMonth.raster[[Sep+31*12]],
                 TAMSATMonth.raster[[Sep+32*12]], TAMSATMonth.raster[[Sep+33*12]], TAMSATMonth.raster[[Sep+34*12]], TAMSATMonth.raster[[Sep+35*12]],
                 TAMSATMonth.raster[[Sep+36*12]], TAMSATMonth.raster[[Sep+37*12]], TAMSATMonth.raster[[Sep+38*12]], TAMSATMonth.raster[[Sep+39*12]])

OctoberMean <- mean(TAMSATMonth.raster[[Oct]], TAMSATMonth.raster[[Oct+12]], TAMSATMonth.raster[[Oct+2*12]], TAMSATMonth.raster[[Oct+3*12]],
               TAMSATMonth.raster[[Oct+4*12]], TAMSATMonth.raster[[Oct+5*12]], TAMSATMonth.raster[[Oct+6*12]], TAMSATMonth.raster[[Oct+7*12]],
               TAMSATMonth.raster[[Oct+8*12]], TAMSATMonth.raster[[Oct+9*12]], TAMSATMonth.raster[[Oct+10*12]], TAMSATMonth.raster[[Oct+11*12]],
               TAMSATMonth.raster[[Oct+12*12]], TAMSATMonth.raster[[Oct+14*12]], TAMSATMonth.raster[[Oct+15*12]],
               TAMSATMonth.raster[[Oct+16*12]], TAMSATMonth.raster[[Oct+17*12]], TAMSATMonth.raster[[Oct+18*12]], TAMSATMonth.raster[[Oct+19*12]],
               TAMSATMonth.raster[[Oct+20*12]], TAMSATMonth.raster[[Oct+21*12]], TAMSATMonth.raster[[Oct+22*12]], TAMSATMonth.raster[[Oct+23*12]],
               TAMSATMonth.raster[[Oct+24*12]], TAMSATMonth.raster[[Oct+25*12]], TAMSATMonth.raster[[Oct+26*12]], TAMSATMonth.raster[[Oct+27*12]],
               TAMSATMonth.raster[[Oct+28*12]], TAMSATMonth.raster[[Oct+29*12]], TAMSATMonth.raster[[Oct+30*12]], TAMSATMonth.raster[[Oct+31*12]],
               TAMSATMonth.raster[[Oct+32*12]], TAMSATMonth.raster[[Oct+33*12]], TAMSATMonth.raster[[Oct+34*12]], TAMSATMonth.raster[[Oct+35*12]],
               TAMSATMonth.raster[[Oct+36*12]], TAMSATMonth.raster[[Oct+37*12]], TAMSATMonth.raster[[Oct+38*12]], TAMSATMonth.raster[[Oct+39*12]])

NovemberMean <- mean(TAMSATMonth.raster[[Nov]], TAMSATMonth.raster[[Nov+12]], TAMSATMonth.raster[[Nov+2*12]], TAMSATMonth.raster[[Nov+3*12]],
                TAMSATMonth.raster[[Nov+4*12]], TAMSATMonth.raster[[Nov+6*12]], TAMSATMonth.raster[[Nov+7*12]],
                TAMSATMonth.raster[[Nov+8*12]], TAMSATMonth.raster[[Nov+9*12]], TAMSATMonth.raster[[Nov+10*12]], TAMSATMonth.raster[[Nov+11*12]],
                TAMSATMonth.raster[[Nov+12*12]], TAMSATMonth.raster[[Nov+13*12]], TAMSATMonth.raster[[Nov+14*12]], TAMSATMonth.raster[[Nov+15*12]],
                TAMSATMonth.raster[[Nov+16*12]], TAMSATMonth.raster[[Nov+17*12]], TAMSATMonth.raster[[Nov+18*12]], TAMSATMonth.raster[[Nov+19*12]],
                TAMSATMonth.raster[[Nov+20*12]], TAMSATMonth.raster[[Nov+21*12]], TAMSATMonth.raster[[Nov+22*12]], TAMSATMonth.raster[[Nov+23*12]],
                TAMSATMonth.raster[[Nov+24*12]], TAMSATMonth.raster[[Nov+25*12]], TAMSATMonth.raster[[Nov+26*12]], TAMSATMonth.raster[[Nov+27*12]],
                TAMSATMonth.raster[[Nov+28*12]], TAMSATMonth.raster[[Nov+29*12]], TAMSATMonth.raster[[Nov+30*12]], TAMSATMonth.raster[[Nov+31*12]],
                TAMSATMonth.raster[[Nov+32*12]], TAMSATMonth.raster[[Nov+33*12]], TAMSATMonth.raster[[Nov+34*12]], TAMSATMonth.raster[[Nov+35*12]],
                TAMSATMonth.raster[[Nov+36*12]], TAMSATMonth.raster[[Nov+37*12]], TAMSATMonth.raster[[Nov+38*12]], TAMSATMonth.raster[[Nov+39*12]])

DecemberMean <- mean(TAMSATMonth.raster[[Dec]], TAMSATMonth.raster[[Dec+12]], TAMSATMonth.raster[[Dec+2*12]], TAMSATMonth.raster[[Dec+3*12]],
                TAMSATMonth.raster[[Dec+4*12]], TAMSATMonth.raster[[Dec+6*12]], TAMSATMonth.raster[[Dec+7*12]],
                TAMSATMonth.raster[[Dec+8*12]], TAMSATMonth.raster[[Dec+9*12]], TAMSATMonth.raster[[Dec+10*12]], TAMSATMonth.raster[[Dec+11*12]],
                TAMSATMonth.raster[[Dec+12*12]], TAMSATMonth.raster[[Dec+13*12]], TAMSATMonth.raster[[Dec+14*12]], TAMSATMonth.raster[[Dec+15*12]],
                TAMSATMonth.raster[[Dec+16*12]], TAMSATMonth.raster[[Dec+17*12]], TAMSATMonth.raster[[Dec+18*12]], TAMSATMonth.raster[[Dec+19*12]],
                TAMSATMonth.raster[[Dec+20*12]], TAMSATMonth.raster[[Dec+21*12]], TAMSATMonth.raster[[Dec+22*12]], TAMSATMonth.raster[[Dec+23*12]],
                TAMSATMonth.raster[[Dec+24*12]], TAMSATMonth.raster[[Dec+25*12]], TAMSATMonth.raster[[Dec+26*12]], TAMSATMonth.raster[[Dec+27*12]],
                TAMSATMonth.raster[[Dec+28*12]], TAMSATMonth.raster[[Dec+29*12]], TAMSATMonth.raster[[Dec+30*12]], TAMSATMonth.raster[[Dec+31*12]],
                TAMSATMonth.raster[[Dec+32*12]], TAMSATMonth.raster[[Dec+33*12]], TAMSATMonth.raster[[Dec+34*12]], TAMSATMonth.raster[[Dec+35*12]],
                TAMSATMonth.raster[[Dec+36*12]], TAMSATMonth.raster[[Dec+37*12]], TAMSATMonth.raster[[Dec+38*12]], TAMSATMonth.raster[[Dec+39*12]])
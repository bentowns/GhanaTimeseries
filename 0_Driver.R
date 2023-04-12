#-------------------------------------------------------------------------------
#Set Directory
#-------------------------------------------------------------------------------
#Needs to be run in GhanaTimeseries Project!
dir0_main <- getwd() #Level 0 folder
#-------------------------------------------------------------------------------
#Level 1 folders
#-------------------------------------------------------------------------------
dir1_code <- paste(dir0_main, "Code", sep="/") 
dir1_data <- paste(dir0_main, "Data", sep="/")
dir1_outputs <- paste(dir0_main, "Outputs", sep="/")
#-------------------------------------------------------------------------------
#Data files
#-------------------------------------------------------------------------------
dir1data_rainfall <- "../DataCentral/RainfallData"
dir1data_shapefiles <- "../DataCentral/Shapefiles"
dir2data_TAMSAT <- paste(dir1data_rainfall, "TAMSATMonthly", sep="/")
dir2data_Ghana <- paste(dir1data_shapefiles, "GhanaCountry", sep="/")
#-------------------------------------------------------------------------------
#Load required packages
#-------------------------------------------------------------------------------
library(raster) #For raster preprocessing
library(sf) #For extraction
library(ncdf4) #For open rainfall files
library(tidyverse) #For any necessary data transformations and plotting
library(exactextractr) #For extracting pixel values
library(parallel) #For parallel computing
library(foreach) #For parallel computing
library(doParallel) #For parallel computing
library(tictoc) #For parallel computing
library(magick) #For creating gifs
#-------------------------------------------------------------------------------
#Load parallel
ncores <- detectCores(logical = FALSE)
registerDoParallel(cores=ncores)
#-------------------------------------------------------------------------------
#Read in Datasets
#-------------------------------------------------------------------------------
#Load shapefile of ghana districts
loc_Ghana <- paste(dir2data_Ghana, "GHA_adm0.shp", sep = "/")
Ghana <- st_read(loc_Ghana)

#Load in Monthly TAMSAT
loc_CHIRPSMonth <- paste(dir2data_TAMSAT, "TAMSATMonths_Dec2022.nc", sep = "/")
TAMSATMonth.nc <- nc_open(loc_CHIRPSMonth)
#-------------------------------------------------------------------------------
#Function to crop rasters
#-------------------------------------------------------------------------------
CropRaster <- function(raster, boundary) {
  raster.crop <- crop(raster, extent(boundary))
  raster.mask <- mask(raster.crop, boundary)
  return(raster.mask)
}
#-------------------------------------------------------------------------------
#Load Processed Images
source("Code/1_ImageProcessing.R")
#-------------------------------------------------------------------------------
#Create plots of the data
#-------------------------------------------------------------------------------
#First, find the max rainfall value (for setting limits)

#probably in may, june or july
JanPlot <- plot(JanuaryMean, zlim = c(0,500))
FebPlot <- plot(FebruaryMean, zlim = c(0,500))
plot(MarchMean, zlim = c(0,500))
plot(AprilMean, zlim = c(0,500))
plot(MayMean, zlim = c(0,500))
plot(JuneMean, zlim = c(0,500))
plot(JulyMean, zlim = c(0,500))
plot(AugustMean, zlim = c(0,500))
plot(SeptemberMean, zlim = c(0,500))
plot(OctoberMean, zlim = c(0,500))
plot(NovemberMean, zlim = c(0,500))
plot(DecemberMean, zlim = c(0,500))

JanuaryMean.df <- RasterToDataframe(JanuaryMean)
FebruaryMean.df <- RasterToDataframe(FebruaryMean)
MarchMean.df <- RasterToDataframe(MarchMean)
AprilMean.df <- RasterToDataframe(AprilMean)
MayMean.df <- RasterToDataframe(MayMean)
JuneMean.df <- RasterToDataframe(JuneMean)
JulyMean.df <- RasterToDataframe(JulyMean)
AugustMean.df <- RasterToDataframe(AugustMean)
SeptemberMean.df <- RasterToDataframe(SeptemberMean)
OctoberMean.df <- RasterToDataframe(OctoberMean)
NovemberMean.df <- RasterToDataframe(NovemberMean)
DecemberMean.df <- RasterToDataframe(DecemberMean)

JanuaryMean.df$Month <- "January"
FebruaryMean.df$Month <- "February"
MarchMean.df$Month <- "March"
AprilMean.df$Month <- "April"
MayMean.df$Month <- "May"
JuneMean.df$Month <- "June"
JulyMean.df$Month <- "July"
AugustMean.df$Month <- "August"
SeptemberMean.df$Month <- "September"
OctoberMean.df$Month <- "October"
NovemberMean.df$Month <- "November"
DecemberMean.df$Month <- "December"


MonthlyMean <- rbind(JanuaryMean.df, FebruaryMean.df, MarchMean.df, AprilMean.df, 
                     MayMean.df, JuneMean.df, JulyMean.df, AugustMean.df,
                     SeptemberMean.df, OctoberMean.df, NovemberMean.df, DecemberMean.df)
MonthlyMean$Month <- factor(MonthlyMean$Month, 
                            levels = c("January", "February", "March", "April", "May", "June",
                                      "July", "August", "September", "October", "November", "December"))
#-------------------------------------------------------------------------------
#Create gif and export the gif
#-------------------------------------------------------------------------------
p <- ggplot() +
  geom_sf(data = Ghana) +
  geom_raster(data = MonthlyMean, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours=c("#001137", "#0aab1e", "#e7eb05", "#ff4a2d", "#e90000"),
                       name="Rainfall (mm)") +
  labs(title = 'Month: {closest_state}', x = "Longitude", y = "Latitude") +
  transition_states(Month, transition_length = 1, state_length = 2)
animate(p)

save_animation("MeanRainfallTAMSAT.gif", animation = last_animation())

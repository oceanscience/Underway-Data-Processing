##3TSGcontourplot.R

#**NOTE may need to remove data from the interpolated data file when TSG is turned on and off throughout mission

#***Desciption: Script to read interpolated data and plot map contour plots from underway flow 
# through system.  plots are saved in the \3code_plot_TSGdata folder
# The interpolated data file is output from the code 2TSG_interp.R and 
# saved in the \2code_interp_plot_TSGdata\hourly_TSG_dataplots with name HUDyyyyfff_TSG_hourly.csv

#***INPUT: 
# 1- The interpolated data file output from the code 2TSG_interp.R and 
# saved in the \2code_interp_plot_TSGdata\hourly_TSG_dataplots folder with name HUDyyyyfff_TSG_hourly.csv
# May need to delete bad lines from this file if the TSG was turned off and on during mission
# 2- Topographic data downloaded boundaries west = -75, east = -50, south = 38, north = 50,
# 3- The longitude and latitude limits for plots variable lonlim and latlim 
# 4- The data coastlineWorldFine downloaded from the ocedata package

# OUTPUT is in the 3code_plot_TSGdata and includes:
# - one plot per variable of hourly interpolated data, named TSG_variable.png
# - a NetCDF file containing Topographic data

#**Folder structure:
#C:\TSG_process - working dir
#\1code_readTSGdata - functions to read and cleanup TSG log files and calculate oxygen saturation called from the script "1readTSGdata.R"
#\2code_interp_plot_TSGdata - functions to interpolate the processed data over time called from the script "2TSG_interp.R"
#\2code_interp_plot_TSGdata\hourly_TSG_dataplots - plots and tables of interpolated data from the script "2TSG_interp.R"
#\3code_plot_TSGdata - plots of the interpolated data created from the script "3TSGcontourplot.R"
#\4comparesamples - functions to compare and plot TSG data with water samples and  CTD data  called from the script "4TSGcompare_ctd_samples.R"
#\processed - formatted log files from TSG with corrupted data removed created from the script "1readTSGdata.R"
#\raw - log files from TSG 
#\bottle - water sample data analysed on the ship typically salts, Chlorophyll and oxygen 
#\ctd- ctd files in ODF format processed on the ship
#\elog - log files from ELOG

# author Diana Cardoso
# Oct 2021
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

rm(list=ls()) #remove everything in the working environment.

#Install and load specific package versions
#install.packages("oce", "ocedata")
library(oce)
library(ocedata)

# this is the directory where R expects to find the local code
# e.g "1code_readTSGdata/readflowdata.R"
source_code_directory <- getwd()

# path to where we want the processed files to end up
pathprocessed <- Sys.getenv("Processed_Directory")

#Map boundary limits used by plots in the function plot.tsg
min_lat <- as.numeric(Sys.getenv("Min_Lat"))
min_lon <- as.numeric(Sys.getenv("Min_Lon"))
max_lat <- as.numeric(Sys.getenv("Max_Lat"))
max_lon <- as.numeric(Sys.getenv("Max_Lon"))

lonlim <- c(min_lon, max_lon)
latlim <- c(min_lat, max_lat)

# if the processed directory doesn't already exist create it
if(!dir.exists(file.path(pathprocessed))) {
  dir.create(file.path(pathprocessed))
}

# This directory should have been created in the 2TSG_interp.R script
hourly_processed_data <- file.path(pathprocessed, "2code_interp_plot_TSGdata", "hourly_TSG_dataplots")

# Create the directory where intermediate hourly plots and data will be stored 
plot_output <- file.path(pathprocessed, "3code_plot_TSGdata")
if(!dir.exists(plot_output)) {
  dir.create(plot_output)
}

data('coastlineWorldFine', package="ocedata")

#Topographic data downloaded from a data server that holds the ETOPO1 dataset 
#(Amante, C. and B.W. Eakins, 2009) and saved as a netCDF file 
topoFile <- download.topo(west = min_lon, east = max_lon,
                          south = min_lat, north = max_lat,
                          resolution = 1)
ocetopo <- read.topo(topoFile)

# Read the file containing all the variables interpolated hourly, named HUDyyyyfff_TSG_hourly.csv
fileinterp <- list.files(path=hourly_processed_data, pattern = '*_TSG_hourly.csv', full.names = TRUE)

###Lindsay: Cut off dates/times BEFORE the underway system was turned on:
d <- read.csv(fileinterp)

start_date <- Sys.getenv("Start_Date")
end_date <- Sys.getenv("End_Date")

# subset the data to remove bad head or tail data from the underway dataset
if(start_date != '') d <- subset(d, time > start_date)
if(end_date != '') d <- subset(d, time < end_date)

lon <- d[['longitude']] * -1
lat <- d[['latitude']]

conductivity <- "Conductivity_S_m"
fluorescence_uv <- "FluorescenceUV"
fluorescence <- 'Fluorescence'
pH <- 'pH'
temperature <- 'Temperature_TSG_ITS_90'
oxygen <- 'O2Concentration_ml_L'
salinity <- 'salinity_PSU'

variables <- c(conductivity, fluorescence_uv, pH, temperature, oxygen,
               fluorescence, salinity) #'CO2_ppm' removed from list

proj <- '+proj=merc'
fillcol <- 'lightgray'

for (var in variables){
  filename <- paste0('TSG_', var, '.png')
  file <- file.path(plot_output, filename)
  png(filename=file , width = 6, height = 4,
      units = 'in', res = 250, pointsize = 12)
  layout(matrix(1:2, nrow=1), widths=c(5, 0.3))
  par(mar = c(2, 3, 1, 1))
  cm <- colormap(z = d[[var]], col = oceColorsJet) # can change color scheme
  drawPalette(colormap = cm)
  par(new=TRUE)
  par(mar = c(2, 2, 1, 3.5))
  mapPlot(coastlineWorldFine, 
          longitudelim = lonlim,
          latitudelim = latlim,
          col = fillcol, 
          proj = proj,
          grid = c(2,1))
  bathylevels <- c(-3000, -2000, -1000, -200)
  bathycol <- 'lightgrey'
  mapContour(longitude = ocetopo[['longitude']],
             latitude = ocetopo[['latitude']],   
             z = ocetopo[['z']],
             levels = bathylevels,
             lwd = 0.8, col = bathycol)
  if(var==conductivity) mtext("Conductivity (S/m)", side=4, line=4, col="black")
  if(var==fluorescence_uv) mtext(expression(paste("CDOM ", "(", mu,"g/L)", sep="")), side=4, line=4, col="black")
  if(var==pH) mtext("pH", side=4, line=4, col="black")
  if(var==temperature) mtext(expression(paste("Temperature ","(",degree,"C)", sep="")), side=4, line=4, col="black")
  if(var==oxygen) mtext("Dissolved Oxygen (ml/L)", side=4, line=4, col="black")
  if(var==fluorescence) mtext(expression(paste("Chlorophyll ", "(", mu,"g/L)", sep="")), side=4, line=4, col="black")
  if(var==salinity) mtext("Salinity", side=4, line=4, col="black")
  mapPoints(lon, lat, pch = 20, col = cm$zcol)
  dev.off()
}

# Record session information
sink_dir <- file.path(pathprocessed, 'session_info3.txt')
sink(sink_dir)
sessionInfo()
sink()

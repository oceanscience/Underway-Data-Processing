##2TSG_interp.R
#***Desciption: Script to read,interpolate hourly, and plot all processed data from underway flow 
# through system. processed files are saved in the processed folder and  
# 2 code_interp_plot_TSGdata folder contains all functions to interpolate and plot

#***INPUT: 
# 1- processed files one per 24 hours in processed folder and are named:
# TSGposition_yyyymmdd.CSV, FLOWdata_yyyymmdd.CSV, TSGdata_yyyymmdd.CSV, PCO2data_yyyymmdd.CSV
# 2- cruise number missionnum <- "xxxyyyy000" second line of code
# 3- the longitude and latitude limits for plots variable lonlim and latlim third line of code
# 4- hourrate can be changed from hour to another frequency

# OUTPUT is in the \2code_interp_plot_TSGdata\hourly_TSG_dataplots folder and includes:

  # - one file per variable of hourly interpolated data, named TSG_xxxxdata_xxxx_HUDyyyyfff.csv 
  # (where xxxx is variable and HUDyyyyfff is the mission number)
  # - a file containing all the variables interpolated hourly, named HUDyyyyfff_TSG_hourly.csv
  # - a plot of each variable versus time and versus longitude, named TSG_xxxxdata_xxxx_HUDyyyyfff.jpeg and  TSG_xxxx_long.jpeg, 
  # - a map of station locations, named TSG_map.jpeg

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

# FUNCTIONS:

#   interp.tsghourly - filename interpTSGhourly.R
#   assemble.tsg - filename assemble_TSG.R
#   plot.tsg - filename plot_tsg.R

# author Diana Cardoso
# Oct 2021
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

rm(list=ls()) #remove everything in the working environment.

#enter cruise number
missionnum <- Sys.getenv("Cruise_Number")

#Map boundary limits used by plots in the function plot.tsg
min_lat <- as.numeric(Sys.getenv("Min_Lat")) 
min_lon <- as.numeric(Sys.getenv("Min_Lon"))
max_lat <- as.numeric(Sys.getenv("Max_Lat"))
max_lon <- as.numeric(Sys.getenv("Max_Lon"))

lonlim <- c(min_lon, max_lon)
latlim <- c(min_lat, max_lat)

#Install and load specific package versions
#install.packages("oce", "ocedata", "ggplot2")
library(oce)
library(ocedata)
library(ggplot2)
data("coastlineWorldFine")

# this is the directory where R expects to find the local code
# e.g "1code_readTSGdata/readflowdata.R"
source_code_directory <- getwd()

# path where raw TSG files Read exist
pathrawdata <- Sys.getenv("TSG_Input_Directory")

# path to where we want the processed files to end up
pathprocessed <- Sys.getenv("Processed_Directory")

# if the processed directory doesn't already exist create it
if(!dir.exists(file.path(pathprocessed))) {
  dir.create(file.path(pathprocessed))
}

# Create the directory where intermediate hourly plots and data will be stored 
hourly_processed_data <- file.path(pathprocessed, "2code_interp_plot_TSGdata", "hourly_TSG_dataplots")
if(!dir.exists(hourly_processed_data)) {
  dir.create(hourly_processed_data, recursive = TRUE)
}

# function file names
source("2code_interp_plot_TSGdata/interpTSGhourly.R")
source("2code_interp_plot_TSGdata/assemble_TSG.R")
source("2code_interp_plot_TSGdata/plot_tsg.R")

# list the processed log files from TSG including path from the processed folder
filesflow <- list.files(path= pathprocessed, pattern = 'FLOWdata.*\\.csv', full.names = TRUE)
filespco2 <- list.files(path= pathprocessed, pattern = 'PCO2data.*\\.csv', full.names = TRUE)
filestsg <- list.files(path= pathprocessed, pattern = 'TSGdata.*\\.csv', full.names = TRUE)
filesnmea <- list.files(path= pathprocessed, pattern = 'TSGposition.*\\.csv', full.names = TRUE)

max_date = "9999-12-31 23:59:59.999"

t_tgall = -1
t_tgalle = max_date

t_flowall = -1
t_flowalle = max_date

t_pco2all = -1
t_pco2alle = max_date

t_nmeaall = -1
t_nmeaalle = max_date

#determine the latest start date/time from all files
if(length(filestsg) > 0) {
  tsgall <- data.frame(read.csv(filestsg[1], header = TRUE))
  t_tgall = if(exists("tsgall")) tsgall$time[1]
  
  tsgalle <- data.frame(read.csv(filestsg[length(filestsg)], header = TRUE))
  t_tgalle = if(exists("tsgalle")) tsgalle$time[length(tsgalle$time)]
}

if(length(filesflow) > 0) {
  flowall <- data.frame(read.csv(filesflow[1], header = TRUE))
  t_flowall = if( exists("flowall")) flowall$time[1]
  
  flowalle <- data.frame(read.csv(filesflow[length(filesflow)], header = TRUE))
  t_flowalle = if( exists("flowalle")) flowalle$time[length(flowalle$time)]
}

if(length(filespco2) > 0) {
  pco2all <- data.frame(read.csv(filespco2[1], header = TRUE))
  t_pco2all = if( exists("pco2all")) pco2all$time[1]
  
  pco2alle <- data.frame(read.csv(filespco2[length(filespco2)], header = TRUE))
  t_pco2alle = if( exists("pco2alle")) pco2alle$time[length(pco2alle$time)]
}

if(length(filesnmea) > 0) {
  nmeaall <- data.frame(read.csv(filesnmea[1], header = TRUE))
  t_nmeaall = if( exists("nmeaall")) nmeaall$time[1]
  
  nmeaalle <- data.frame(read.csv(filesnmea[length(filesnmea)], header = TRUE))
  t_nmeaalle = if( exists("nmeaalle")) nmeaalle$time[length(nmeaalle$time)]
}

start_date <- max(t_tgall, t_flowall, t_pco2all, t_nmeaall)
start_date <- round(as.POSIXct(start_date,tz = 'UTC'),"hour") # round to nearest hour

#determine the earliest end date/time from all files
end_date <- min(t_tgalle, t_flowalle, t_pco2alle, t_nmeaalle)

# vector of time by hour between start and end time determined above
hourrate <- seq(from = as.POSIXct(start_date,tz = 'UTC'), to = as.POSIXct(end_date,tz = 'UTC'), by = "hour")

# read each processed file and interpolate each variable hourly and save in individual csv files and 
# plots time series of each variable using the function assemble.tsg
if(length(filestsg) > 0) assemble.TSG(hourly_processed_data, filestsg, hourrate, missionnum)
if(length(filesnmea) > 0) assemble.TSG(hourly_processed_data, filesnmea, hourrate, missionnum)
if(length(filesflow) > 0) assemble.TSG(hourly_processed_data, filesflow, hourrate, missionnum)
if(length(filespco2) > 0) assemble.TSG(hourly_processed_data, filespco2, hourrate, missionnum)

filesinterp <- list.files(path=hourly_processed_data, pattern = 'TSG_.*\\.csv', full.names = TRUE)
interpdataall <- data.frame(read.csv(filesinterp[1], header = TRUE))
filesinterp2 <- filesinterp[2:length(filesinterp)]

for (i in filesinterp2){
  tsginterpall <- data.frame(read.csv(i, header = TRUE))
  coln <- colnames(tsginterpall) #Get the column name
  cc <- coln[3]
  coln2 <- colnames(interpdataall) #Get the column name
  
  interpdataall <- cbind(interpdataall, tsginterpall[,3])# add new column
  colnames(interpdataall)<-c(coln2, cc) #adding name
}

interpdataall$Conductivity_S_m[interpdataall$Conductivity_S_m > 959]<- NA
interpdataall$Fluorescence[interpdataall$Fluorescence > 959]<- NA
interpdataall$salinity_PSU[interpdataall$salinity_PSU > 959]<- NA
interpdataall$O2Concentration_ml_L[interpdataall$O2Concentration_ml_L < 0.5]<- NA
interpdataall$O2Concentration_ml_L[interpdataall$O2Concentration_ml_L > 15]<- NA

#write a csv with all variables interpolated hourly
filename <- paste0(missionnum,"_TSG_hourly.csv")
output_file <- file.path(hourly_processed_data, filename)
write.csv(interpdataall, file=output_file, row.names = FALSE)

# plot each variable vs longitude, plot station locations and 
# longitude and latitude limits for plots (lonlim,latlim)

plot.tsg(hourly_processed_data, interpdataall, lonlim, latlim)

# Record session information
sink_dir <- file.path(hourly_processed_data, 'session_info2.txt')
sink(sink_dir)
sessionInfo()
sink()

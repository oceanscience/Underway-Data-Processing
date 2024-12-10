##1readTSGdata.R
#***Desciption:  Script to read all log files from underway flow through system 
#which include NMEA, flow, sensor and PCO2 data and clean it up.
#processed files output from the code are saved in the processed folder
# removes or replaces corrupted data and incomplete lines in log files with 999 or NaN
# and adds column headers. 
# calculates of Oxygen Concentration (ml/L) per log file

#***INPUT: Log files generated every 24 hours and are named:
# NMEA_yyyymmdd.CSV, FLOW_yyyymmdd.CSV, TSGOUT_yyyymmdd.CSV, PCO2_yyyymmdd.CSV

#***OUTPUT: writes one csv file per log file saved in the processed folder
# TSGpositionlog.txt - is a log of percent bad NMEA data per log file
# O2concTSGdata_yyyymmdd.csv - a table of calculations of Oxygen Concentration (ml/L) per log file

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
# Oct 2023
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

rm(list=ls()) #remove everything in the working environment.

# Install and load specific package versions
#install.packages("oce", "csasAtlPhys")
library(oce)
library(csasAtlPhys)

# this is the directory where R expects to find the local code
# e.g "1code_readTSGdata/readflowdata.R"
source_code_directory <- getwd()

# path where raw TSG files Read exist
pathrawdata <- Sys.getenv("TSG_Input_Directory")

# path to where we want the processed files to end up
pathprocesseddata <- Sys.getenv("Processed_Directory")

# if the processed directory doesn't already exist create it
if(!dir.exists(file.path(pathprocesseddata))) {
  dir.create(file.path(pathprocesseddata))
}

#List of Functions
source("1code_readTSGdata/readflowdata.R")
source("1code_readTSGdata/readFLOW.R")
source("1code_readTSGdata/readnmeadata.R")
source("1code_readTSGdata/readNMEA.R")
source("1code_readTSGdata/readpco2data.R")
source("1code_readTSGdata/readpco2_2.R")
source("1code_readTSGdata/readtsgdata.R")
source("1code_readTSGdata/readTSGout.R")
source("1code_readTSGdata/TSG_optode_SatO2_4.R")

# read and process data
read.flowdata(pathrawdata, pathprocesseddata)
read.nmeadata(pathrawdata, pathprocesseddata)
read.pco2data(pathrawdata, pathprocesseddata)
read.tsgdata(pathrawdata, pathprocesseddata)

# Record session information
sink_dir <- file.path(pathprocesseddata, 'session_info.txt')
sink(sink_dir)
sessionInfo()
sink()

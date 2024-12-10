##4TSGcompare_ctd_samples.R

#**NOTE  processed TSGdata files can not start with a time of 00:00:00

#***Desciption: Script to read processed and bottle (Chl, Oxy, Salts) data from underway system and read CTD and ELOG files
# to match the sample, CTD and TSG data creating tables and plots.  

#***INPUT: 
# 1- pathelog - directory of ELOG .log Flow-Through files 
# 2- pathbottle - directory of analysed Sample data taken from the TSG
# file names start with the variable type:     
    # - 'Oxy.*\\.csv  (convert .dat file to csv, 8 header lines)
    # - 'Chl.*\\.xlsx' (excel, 18 header lines)
    # - 'Salts.*\\.csv' (csv, 4 header lines)
#Note: salts data is generated as 1 files per run these need to be compiled into 1 file, use the file 
#with 1 calculated value per sample ID and not the multiple readings per run or multiple run values
# and a 4 line header is added: 
#   ShipsName=Teleost
#   CruiseNumber=TEL2024880
#   Analyst=Diana Cardoso
#   SalinometerSerialNumber=71418
  
# 3- pathctd - directory of CTD processed ODF files 
# 4- pathprocessed - directory of TSG processed files
# 5- CTDpres - CTD pressure for comparison with TSG intake depth, typically between 4 - 8 m


# OUTPUT is in the \4comparesamples folder and includes:
#read_elog_tsg3.R
# - Bottle_TSG_QAT.csv - table: TSG data average of a minute (12 values) matched with sample data 
# - TSG_bottletable2.csv - table: TSG data when the sample was taken, 12 readings for the minute time of sample 
# - TSG_samples.csv -  table: time, date, lat, lon, ID of each sample taken from TSG
# - chl2tsgplot.jpeg, oxy2tsgplot.jpeg, salt2sgplot.jpeg - plots of TSG vs sample data
#read_elog_tsg_pco2.R
# - Bottle_TSG_PCO2_QAT.csv - table: TSG data average of a minute (12 values) matched with sample data  including PCO2 data
# - TSG_PCO2_bottletable.csv - table: TSG data when the sample was taken, 12 readings for the minute time of sample including PCO2 data
# - TSG_PCO2_samples.csv-  table: time, date, lat, lon, ID of each sample taken from TSG
# - chl2tsgplot.jpeg, oxy2tsgplot.jpeg, salt2sgplot.jpeg - plots of TSG vs sample data
# addPCO2_2.R
# - TSG_PCO2_bottletable2.csv - table: TSG data when the sample was taken, 12 readings for the minute time of sample including PCO2 data
# tsg_ctd.R
# - CTD_table3.csv-  table:CTD data at a certain depth near surface per event with time and position 
# ctd_tsg_compare4.R
# - CTD_TSG_QAT.csv-  table:CTD data at a certain depth near surface per event with time and position matched with TSG data 
# - TSG_ctdtable.RData
# - XXX_TSGCTDplot.jpeg - plots TSG vs CTD data for each variable per event and per day

#**Folder structure:
#C:\TSG_process - working dir
#\1code_readTSGdata - dir for script "1readTSGdata.R" to read all log files and process
#\2code_interp_plot_TSGdata - interpolate the processed data over time
#\3code_plot_TSGdata - plot the interpolated data
#\4comparesamples -  compare TSG water samples with TSG data and compare CTD and TSG data
#\processed - formatted log files from TSG with corrupted data removed
#\raw - log files from TSG
#\bottle - water sample data from the TSG analysed on the ship typically salts, Chlorophyll and oxygen 
#\ctd- ctd files in ODF format processed on the ship
#\elog - TSG log files from ELOG


#** FUNCTIONS:

# read_elog_tsg3.R - read elog and processed tsg files, match the samples to the TSG data and create tables and plots
# read_elog_tsg_pco2.R - read elog and processed tsg files including PCO2, match the samples to the TSG data and create tables and plots
# addPCO2_2.R - add pco2 data only to the table created by the function read_elog_tsg3.R
# tsg_ctd.R - read ctd ODF data and match to TSG data and create a table 
# ctd_tsg_compare4.R - read table created by function tsg_ctd.R and plot TSG vs CTD data

# author Diana Cardoso
# Oct 2021
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

rm(list=ls()) #remove everything in the working environment.

#Install and load specific package versions
#install.packages("stringr", "lubridate","readxl", "readr", oce")
library(stringr)
library(lubridate)
library(readxl)
library(readr)
library(oce)

#CTD pressure for comparison with TSG intake depth, typically between 4 - 8 m
CTDpres <- 7

# this is the directory where R expects to find the local code
# e.g "1code_readTSGdata/readflowdata.R"
source_code_directory <- getwd()

# path to where we want the processed files to end up
pathprocessed <- Sys.getenv("Processed_Directory")

pathout = file.path(pathprocessed, "4comparesamples")
if(!dir.exists(file.path(pathout))) {
  dir.create(file.path(pathout), recursive = TRUE)
}

pathelog<- Sys.getenv("Elog_Directory")
pathctd <- Sys.getenv("ODF_Directory")
samples_directory <- Sys.getenv("ODF_Directory")

#List of Functions
source("4comparesamples/read_elog_tsg3.R")
source("4comparesamples/read_elog_tsg_pco2.R")
source("4comparesamples/addPCO2_2.R")
source("4comparesamples/tsg_ctd.R")
source("4comparesamples/ctd_tsg_compare4.R")

# read and process data
read.elog_tsg(pathout, pathelog, samples_directory)
read.elog_tsg_pco2(pathelog, pathprocessed, samples_directory)
read.addPCO2(pathprocessed) 
read.tsg_ctd(pathctd, CTDpres)
read.ctd_tsg_compare4(pathprocessed)

# Record session information
sink("session_info.txt")
sessionInfo()
sink()

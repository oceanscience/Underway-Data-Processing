#' @title Read pCO2 file from BIO underway flow through system
#' 
#' @description Reads in pCO2 output file from underway flow through system and 
#' writes one csv file per log file with the name PCO2data_yyyymmdd.csv 
#' assuming the log file name is PCO2_yyyymmdd.CSV which is saved once per day
#' 
#' @param pathrawdata the folder name with the raw data to be read
#' @param pathprocesseddata the folder name of with the output processed data
#'  
#' @output a csv data file with date, time, and data one per raw log file
#' 
#' @author Diana Cardoso
#' 
# Oct 2023
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

read.pco2data <- function(pathrawdata, pathprocesseddata){

  filespco2 <- list.files(path=pathrawdata, pattern='PCO2.*\\.CSV', full.names = TRUE) #list of log files with path
  
  for (i in filespco2) {
    
    #creates file name to save the nmea data
    filen <- unlist(strsplit(i, "_"))
    filen2 <- unlist(strsplit(filen[2], "\\."))
    filename <- paste0("PCO2data_",filen2[1],".csv")
    
    # extract the day from file name
    filedate <- unlist(strsplit(filen2[1],""))
    fileday <- as.numeric(paste0(filedate[7],filedate[8]))
    
    pco2 <- read.pco2_2(i,fileday) #call function to read each log file i and include the day number
    
    #write a csv with data
    pathprocessed <- file.path(pathprocesseddata, filename)
    write.csv(pco2, file = pathprocessed)
    rm(pco2)
  }
}

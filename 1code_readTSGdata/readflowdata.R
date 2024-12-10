#' @title Read flowout file from BIO underway flow through system
#' 
#' @description Reads in flow output file from underway flow through system and 
#' writes one csv file per log file with the name FLOWdata_yyyymmdd.csv 
#' assuming the log file name is FLOW_yyyymmdd.CSV which is saved once per day
#' 
#' @param pathrawdata the folder name with the raw data to be read
#' @param pathprocesseddata the folder name of with the output processed data
#'  
#' @output a csv data file with date, time intake, TSG and PCO2 flow rates 
#' 
#' @author Diana Cardoso
#' 
# Oct 2023
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

read.flowdata <- function(pathrawdata, pathprocesseddata){

  filesflow <- list.files(path=pathrawdata, pattern='FLOW.*\\.CSV', full.names = TRUE) #list of log files with path
  
  for (i in filesflow){
    #call function to read each log file i
    flow <- read.flow(i)
    s <- dim(flow)
    flow[1:s[1],2] <- 999
    
    #creates file name to save the nmea data
    filen <- unlist(strsplit(i, "_"))
    filen2 <- unlist(strsplit(filen[2], "\\."))
    filename <- paste0("FLOWdata_",filen2[1],".csv")
    
    # write a csv with data
    # R has a function to concatenate file paths which will add
    # slashes between directories and file names if a slash doesn't
    # already exist. In this case it's preferable to using the paste0
    # which will just mash strings together.
    pathprocessed <- file.path(pathprocesseddata, filename)
    write.csv(flow, file = pathprocessed) 
    rm(flow)
  }
}


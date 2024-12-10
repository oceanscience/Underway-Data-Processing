#' @title Read NMEA log file from BIO underway flow through system
#' 
#' @description Reads in nmea output file from underway flow through system and 
#' writes one csv file per log file with the name TSGposition_yyyymmdd.csv 
#' assuming the log file name is NMEA_yyyymmdd.CSV which is saved once per day
#' 
#' @param pathrawdata the folder name with the raw data to be read
#' @param pathprocesseddata the folder name of with the output processed data
#'  
#' @output a csv data file with date, time, latitude (decimal degrees),
#'  and longitude (decimal degrees) one per raw log file
#' #'a text log file "TSGpositionlog.txt" listing percernt bad data and percernt NaN data pr log file
#' 
#' @author Diana Cardoso
#' 
# Oct 2023
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

read.nmeadata <- function(pathrawdata, pathprocesseddata){

  files <- list.files(path= pathrawdata, pattern = 'NMEA.*\\.CSV', full.names = TRUE) #list of log files with path 
  
  for (i in files){
    #call function to read each log file i
    nmea <- read.nmea(pathprocesseddata, i)
  
    #check for NaN in nmea
    which(is.na(nmea$latitude))
    bad <- which(is.na(nmea$latitude))
    
    #nmea[bad,]
    #checks the percent of data with NaN and prints to a log file
    percentbad <- length(bad)/length(nmea$latitude)*100
    position_log_file <- file.path(pathprocesseddata, "TSGpositionlog.txt")
    cat(',  percernt NaN data = ', percentbad, "for file ",i , "\n") #prints the percent of the data that has NaN in terminal window
    cat(',  percernt NaN data = ', percentbad, "for file ",i , "\n",file = position_log_file, append = TRUE) #prints the percent of the data that has NaN to log file
    
    #creates file name to save the nmea data
    filen <- unlist(strsplit(i, "_"))
    filen2 <- unlist(strsplit(filen[2], "\\."))
    filename <- paste0("TSGposition_",filen2[1],".csv")
    
    #write a csv with data
    pathprocessed <- file.path(pathprocesseddata, filename)
    write.csv(nmea, file = pathprocessed)
    rm(nmea)
  }
}

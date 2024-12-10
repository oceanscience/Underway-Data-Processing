#' @title Read TSGout file from BIO underway flow through system
#' 
#' @description Reads in TSG output file from underway flow through system and 
#' writes one csv file per log file with the name TSGdata_yyyymmdd.csv 
#' assuming the log file name is TSGOUT_yyyymmdd.CSV which is saved once per day
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

read.tsgdata <- function(pathrawdata, pathprocesseddata){

  filesTSG <- list.files(path=pathrawdata, pattern='TSGOUT.*\\.CSV', full.names = TRUE) #list of log files with path

  # if the directory we'll write O2 Saturation files to doesn't exist
  # create it
  o2conc_out_path <- file.path(pathprocesseddata, "1code_readTSGdata")
  if(!dir.exists(file.path(o2conc_out_path))) {
    dir.create(file.path(o2conc_out_path))
  }
  
  o2conc_out_path <- file.path(o2conc_out_path, "O2conc")
  if(!dir.exists(file.path(o2conc_out_path))) {
    dir.create(file.path(o2conc_out_path))
  }
  
  for (i in filesTSG) {
    #call function to read each log file i
    tsgout <- read.tsgout(i)
    
    #check for NaN in pH
    # The Fluorescence was not always logged which caused the ph to log in the wrong column
    # this moves the pH values from the FluorescenceUV column and places NA in FluorescenceUV column
    bad <- which(is.na(tsgout$pH))
    tsgout$pH[bad] <- tsgout$FluorescenceUV[bad]
    tsgout$FluorescenceUV[bad] <-  NA
  
    s <- dim(tsgout)
    # tsgout[1:s[1],2] <- 999
    
    #creates file name to save the nmea data
    filen <- unlist(strsplit(i, "_"))
    filen2 <- unlist(strsplit(filen[2], "\\."))
    filename <- paste0("TSGdata_",filen2[1],".csv")
    
    O2_conc_sat_per <- read.tsgcalphase(tsgout) #call function to calculate O2 concentration from cal Phase

    # convert from uM to ml/L, 1 µmol O2= 0.022391 ml    0.02239195
    tsgout$calphase <- (O2_conc_sat_per$O2ConcentrationSV)*0.02239195
    colnames(tsgout)[8] <- "O2Concentration_ml_L"
    badO2 <- which(tsgout$O2Concentration_ml_L<0.5)
    badO2_2 <- which(tsgout$O2Concentration_ml_L>15)
    tsgout$O2Concentration_ml_L[badO2] <- NA #999
    tsgout$O2Concentration_ml_L[badO2_2] <-NA #999
  
    #write a csv with data
    pathprocessed <- file.path(pathprocesseddata, filename)
    write.csv(tsgout, file = pathprocessed)
    rm(tsgout)
    
    # writing the o2 saturation file
    o2conc_path <- file.path(o2conc_out_path, filename)
    write.csv(O2_conc_sat_per, file=o2conc_path)
    rm(O2_conc_sat_per)
  }
}  

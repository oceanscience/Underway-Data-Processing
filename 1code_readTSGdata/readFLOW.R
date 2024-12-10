#' @title Read flowout file from BIO underway flow through system
#' 
#' @description Reads in flow output file from underway flow through system
#' 
#' @param file the name of the file which the data will be read from
#' 
#' @output a data frame, of relevant data which has date, time intake, TSG and PCO2 flow rates 
#' 
#' @author Diana Cardoso
#' 
# Oct 2023
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

read.flow <- function(file){
  # read file in
  #   note that warn=FALSE, meaning it won't warn us of an
  #   incomplete final line, which is common
  d <- readLines(file, warn = FALSE)

  #remove first line, which is the column headers
  d <- d[2:length(d)]
  
  # split each string into a list
  ssflow <- strsplit(d, split = ',')
  
  # now start getting relevant items out of the lines
  # date time
  date <- unlist(lapply(ssflow, function(k) k[1]))
  
  # format it 
  time <- as.POSIXct(date, tz = 'UTC')
  # flow data
  F1 <- unlist(lapply(ssflow, function(k) k[4]))
  F2 <- unlist(lapply(ssflow, function(k) k[5]))
  F3 <- unlist(lapply(ssflow, function(k) k[6]))
  # Fin <- as.numeric(F1)
  # Fin2 <- replace(Fin, Fin == 0, 999)           # Replace values
  
  # put time and flow data together
  df <- data.frame(
    time = time,
    Flow_intake = as.numeric(F1),
    Flow_TSG = as.numeric(F2),
    Flow_PCO = as.numeric(F3)
    )
  
  df
  
}
#' @title Read TSGout file from BIO underway flow through system
#' 
#' @description Reads in TSG output file from underway flow through system
#' 
#' @param file the name of the file which the data will be read from
#' 
#' @output a data frame of relevant data
#' 
#' @author Diana Cardoso
# Oct 2023
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

read.tsgout <- function(file){
  # read file in
  #   note that warn=FALSE, meaning it won't warn us of an
  #   incomplete final line, which is common
  d <- readLines(file, 
                 warn = FALSE)
  d <- d[2:length(d)]
  # split each string into a list
  x=1
  ss <- strsplit(d, split = ',')
  sstsg <- strsplit(ss[[1]], split = "\\s+")
  for (i in ss){
  sstsg[x] <- strsplit(i, split = "\\s+") #  split each string in list, treat multiple spaces as one
  x=x+1
}
  # now start getting relevant items out of the lines
  # date time
  date <- unlist(lapply(sstsg, function(k) k[1]))
  time <- unlist(lapply(sstsg, function(k) k[2]))
  dateTime <- paste(date, time)
  # format it 
  time <- as.POSIXct(dateTime, tz = 'UTC')
  # TSG data
  T1 <- unlist(lapply(sstsg, function(k) k[6]))
  T2 <- unlist(lapply(sstsg, function(k) k[5]))
  cond <- unlist(lapply(sstsg, function(k) k[7]))
  fluo <- unlist(lapply(sstsg, function(k) k[8]))
  ph <- unlist(lapply(sstsg, function(k) k[9]))
  if (length(sstsg[[1]])==9){
    chl<- 999                 
    chl[1:length(date)] <- 999
    calphase <- 999
    calphase[1:length(date)] <- 999
    sal <- 999
    sal[1:length(date)] <- 999
 } 
  if (length(sstsg[[1]])==11) {
    chl <- unlist(lapply(sstsg, function(k) k[10]))
    calphase <- unlist(lapply(sstsg, function(k) k[11]))  
    sal <- 999
    sal[1:length(date)] <- 999
 }
 
   if (length(sstsg[[1]])==12) {
    chl <- unlist(lapply(sstsg, function(k) k[10]))
    calphase <- unlist(lapply(sstsg, function(k) k[11]))
    sal <- unlist(lapply(sstsg, function(k) k[12]))
    cond<- as.numeric(cond)*10
  }
  
  # put time and TSG data together
  df <- data.frame(time = time,
                   Temperature_intake_ITS_90 = as.numeric(T1),
                   Temperature_TSG_ITS_90 = as.numeric(T2),
                   Conductivity_S_m = as.numeric(cond)/10,
                   FluorescenceUV = as.numeric(fluo),
                   pH = as.numeric(ph), 
                   Fluorescence = as.numeric(chl),
                   calphase = as.numeric(calphase),
                   salinity_PSU = as.numeric(sal)
                   )
  df
  
}

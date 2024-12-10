#' @title Read NMEA file from BIO underway flow through system
#' 
#' @description Reads in NMEA output file from underway flow through system
#' 
#' @param file the name of the file which the data will be read from
#' 
#' @output a data frame, of relevant data from the GPGGA lines, which has date, time, latitude 
#' (decimal degrees), and longitude (decimal degrees)
#' 
#' @author Diana Cardoso
# Oct 2023
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

read.nmea <- function(output_dir, file){   
  # convert lat lon to decimal
    conv <- function(x) {
    res <- rep(NA, length(x))
    zeros <- x == "0"
    nas <- is.na(x)
    good <- !(zeros | nas)
    res[good] <- ifelse(substr(x[good], 1, 1) == "-", -1, 1)*
      ((abs(as.numeric(x[good])/100) - floor(abs(as.numeric(x[good])/100)))*100/60 
       + floor(abs(as.numeric(x[good])/100)))
    res[zeros] <- 0
    return(res)
  }

  # read file in
  # not the same number of columns in each row so have to use readLines
  #   note that warn=FALSE, meaning it won't warn us of an
  #   incomplete final line, which is common
  d <- readLines(file, warn = FALSE)
  
  # split each string into a list
  # get the nmeaName
  ss <- strsplit(d, split = ',')
  
  # it looks line nmeaName will always be the third column
  nmeaName <- unlist(lapply(ss, function(k) k[3]))
  
  # remove the '$' for ease
  nmeaName <- gsub('\\$(\\w+)', '\\1', nmeaName)
  
  # find which lines are gpgga
  okgpgga <- nmeaName == 'GPGGA'
  
  #remove na lines from gpgga
  nagpgga <- which(is.na(okgpgga))
  
  #okgpgga[nagpgga] <- FALSE
  # subset the data
  dg <- d[okgpgga]
  
  # split each string into a list
  ssg <- strsplit(dg, split = ',')
  
  # check that each row has the same number of columns
  ncol <- unlist(lapply(ssg, length))
  uncol <- unique(ncol)

  if(length(uncol) != 1){
    ok16less <- which(ncol < 17)
    
    #checks the percent of bad data - rows with 17 or less columns and prints to a log file
    percentbad <- length(ok16less)/length(ssg)*100
    position_log_file <- file.path(output_dir, "TSGpositionlog.txt")
    cat('percernt bad data = ', percentbad)
    cat('percernt bad data = ', percentbad, file = position_log_file, append = TRUE) 
    
    #If the percent is greater than 5 print error otherwise keep only good data remove bad.
    if (percentbad > 5) {
      cat('Error : the GPGGA number of lines that do not have same number of columns is greater than 5%')
    } else {
      keep <- which(ncol >= 17)
      ssg17 <- ssg[keep]
      
      # now start getting relevant items out of the lines
      # date time
      dateTime <- unlist(lapply(ssg17, function(k) k[1]))
      
      # format it 
      time <- as.POSIXct(dateTime, tz = 'UTC')
      
      # coordinates
      latitude <- unlist(lapply(ssg17, function(k) k[5]))
      lathemis <- unlist(lapply(ssg17, function(k) k[6]))
      longitude <- unlist(lapply(ssg17, function(k) k[7]))
      lonhemis <- unlist(lapply(ssg17, function(k) k[8]))
      
      # format it
      # currently in degrees decimal minutes
      # want it in decimal degrees
      lat <- conv(latitude)
      lon <- conv(longitude)
      # put time and coordinates together
      df <- data.frame(time = time,
                       latitude = lat,
                       longitude = lon)
      df
    }
  } else if(length(uncol)== 1) {
      #checks the percent of bad data - rows with 17 or less columns and prints to a log file
      cat('percent bad data = ', 0)
      # cat('percent bad data = ', 0, file = "TSGpositionlog.txt", append = TRUE) 
      
      ssg17 <- ssg
      # now start getting relevant items out of the lines
      # date time
      dateTime <- unlist(lapply(ssg17, function(k) k[1]))
      # format it 
      time <- as.POSIXct(dateTime, tz = 'UTC')
      # coordinates
      latitude <- unlist(lapply(ssg17, function(k) k[5]))
      lathemis <- unlist(lapply(ssg17, function(k) k[6]))
      longitude <- unlist(lapply(ssg17, function(k) k[7]))
      lonhemis <- unlist(lapply(ssg17, function(k) k[8]))
      
      # format it
      # currently in degrees decimal minutes
      # want it in decimal degrees
      lat <- conv(latitude)
      lon <- conv(longitude)
      
      # put time and coordinates together
      df <- data.frame(
        time = time,
        latitude = lat,
        longitude = lon
        )
      
      df
    }
}
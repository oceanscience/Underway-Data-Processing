#' @title read,interpolate and plot processed log files from underway flow through system
#' 
#' @description Read, interpolate and plot time series of each variable from processed files 
#' output from the code 1readTSGdata in the processed folder. 
#' 
#' @param list of processed file names with path from processed folder to read (files), 
#' rate used to interpolate (hourrate) and mission number (missionnum)
#' 
#' @output a data frame and csv file of each variable interpolated by the rate specified 
#' with the date and time and a jpeg of time series plot of each variable saved in \2code_interp_plot_TSGdata\hourly_TSG_dataplots folder 
#' 
#' @author Diana Cardoso
# Oct 2021
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

assemble.TSG <- function(output_dir, files, hourrate, missionnum){
  # read files in for each variable (normally one per day) and bind them all in one data frame/file  
  dataall <- data.frame(read.csv(files[1], header = TRUE))

  # Get the name of the first file and use it to determine the type of file
  # being processed, eg. FLOWdata, TSGdata, PCO2data
  # file names follow the format '[file_type]_[date].csv'
  #
  # R has built in functions for getting file names - basename(file)
  # as well as getting the directory of the file - dirname(file)
  filen2 <- unlist(strsplit(basename(files[1]), "_"))
  file_type <-filen2[1]
  
  # already read the first file, so strip it out from the file array
  files2 <- files[2:length(files)]
  for (i in files2){
    tsg <- data.frame(read.csv(i, header = TRUE))
    dataall <- rbind(dataall, tsg)
  }
  
  dataall$time <- as.POSIXct(dataall$time,tz = 'UTC') # format time
  datacolnames <- colnames(dataall)
  numcol <- length(datacolnames)
  startcol <- 3 # column in file with data 
  
  if (file_type == "PCO2data"){ # column in file with data for PCO2 data
    startcol <- 6 
    numcol  <-  6
  } else if (file_type == "TSGdata"){ # first column in file with data
    startcol <- 4
  }
  
  start_datetime = hourrate[1]
  end_datetime = hourrate[length(hourrate)]
  for (i in startcol:numcol){
  
    datacol <- data.frame(dataall$time, dataall[[i]])
    colnames(datacol) <- c("time", datacolnames[i])

    datacol$time <- as.POSIXct(datacol$time, tz = 'UTC') # format time
    datacol_hour <- interp.tsghourly(datacol, hourrate) # call function to interpolate 
    colnames(datacol_hour) <- c("time",datacolnames[i]) 
    
    filename <- paste0("TSG_",file_type,"_", datacolnames[i],"_",missionnum,".csv")
    plotname <- paste0("TSG_",file_type,"_", datacolnames[i],"_",missionnum,".jpeg")
    plotname2 <- paste0("TSG_nozero",file_type,"_", datacolnames[i],"_",missionnum,".jpeg")  
    
    # create time series plot and save as a jpeg
    
    # 1. Open jpeg file
    output_plot <- file.path(output_dir, plotname)
    jpeg(output_plot, width = 1150, height = 750)

    # 2. Create a plot time series
    plot(datacol_hour,  
         main= paste0("TSG ", file_type, " hourly"), 
         ylab=datacolnames[i], 
         xaxt="n", 
         xlab=" ") 
    title(xlab="Time (days)", mgp=c(4,1,0))
    axis.POSIXct(1, at = seq(start_datetime, end_datetime, by="day"), format = "%b-%d", las=2)

    # Close the pdf file
    dev.off()
    
    # create a second plot removing 0 values
    datacol_hour0 <- datacol_hour

    datacol_hour0[datacol_hour0 == 0] <- NA
    datacol_hour0[,2][datacol_hour0[,2] > 2500] <- NA

    if (file_type == "TSGdata"){
      datacol_hour0[,2][datacol_hour0[,2] < 0.5] <- NA
    } else if (file_type == "FLOWdata"){
      datacol_hour0[,2][datacol_hour0[,2] < 1] <- NA
      datacol_hour0[,2][datacol_hour0[,2] > 959] <- 0
    }

    datacol_hour0[,2][datacol_hour0[,2] > 959] <- NA

    # 1. Open jpeg file
    output_plot2 <- file.path(output_dir, plotname2)
    jpeg(output_plot2, width = 1150, height = 750)

    # Make sure there's actually data to plot or we'll get a
    # "no non-mission arguments to ..." error
    if(length(datacol_hour0[!is.na(datacol_hour0[2])]) > 0) {
      # 2. Create a plot
      plot(datacol_hour0,
           main=paste0("TSG ",file_type," hourly zeros removed"),
           xlab=" ", 
           ylab=datacolnames[i],
           xaxt="n")
        title(xlab="Time (days)", mgp=c(4,1,0))
        axis.POSIXct(1, at = seq(start_datetime, end_datetime, by="day"), format = "%b-%d", las=2)

        # write a csv with interpolated data and time, if there's no non-zero
        # data, it the variable won't be written
        # saved in hourly_TSG_dataplots folder
        output <- file.path(output_dir, filename)
        write.csv(datacol_hour, file = output)
    } else {
      message(paste0("No ", datacolnames[i], " data to plot"))
    }
    
    # Close the pdf file
    dev.off()
    
    rm(datacol_hour)
  
  }
}

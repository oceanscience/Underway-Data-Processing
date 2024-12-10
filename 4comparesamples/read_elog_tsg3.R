#' @title read elog Flow-Through files and processed data  from underway flow through system, 
#' match the samples from the TSG to the TSG data and create tables and plots
#' 
#' @description  read elog Flow-Through files and processed data from underway flow through system, 
#' match the samples from the TSG to the TSG data and create tables and plots.  
#' The TSG data is averaged per minute (12 values) and matched with sample data
#' Does not include PCO2 data
#' pathbottle - directory of analysed Sample data taken from the TSG 
# file names start with the variable type:     
# - 'Oxy.*\\.csv  (convert .dat file to csv, 8 header lines)
# - 'Chl.*\\.xlsx' (excel, 18 header lines)
# - 'Salts.*\\.csv' (csv, 4 header lines)
#' 
#' @param  ELOG TSG logs directory (pathrawdata) and bottle data (pathbottle) directory
#' 
#' @output is in the \4comparesamples folder and includes:
# - Bottle_TSG_QAT.csv - table: TSG data average of a minute (12 values) matched with sample data 
# - TSG_bottletable2.csv - table: TSG data when the sample was taken, 12 readings for the minute time of sample 
# - TSG_samples.csv -  table: time, date, lat, lon, ID of each sample 
# - chl2tsgplot.jpeg, oxy2tsgplot.jpeg, salt2sgplot.jpeg - plots of TSG vs sample data
#'
#' @author Diana Cardoso
# Oct 2021
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

read.elog_tsg <- function(output_dir, pathrawdata, pathbottle){
  
  fileslog <- list.files(path= pathrawdata, pattern = '*\\.log', full.names = TRUE) #list of log files with path
  TSG_samples <- data.frame(time = "NA",
                                 date = "NA",
                                 POSIXctdate = "NA",
                                 lat = 0,
                                 lon = 0, 
                                 sampleID = 0) 
  
  TSG_bottletable <- data.frame(timebottle = "NA",
                            timeTSG = "NA",
                            lat = 0,
                            lon = 0,
                            sampleID = 0,
                            TSGsal = 0,
                            TSGChl = 0,
                            TSGOxy = 0,
                            TSGtemp = 0,
                            TSGcon = 0,
                            TSGUV = 0,
                            TSGpH = 0
                            ) 
  #bottleposix <- data.frame(time = "NA")
  y=1
  x=1
  for (i in fileslog){
  d <- readLines(i, warn = FALSE)
  df <- data.frame(d)
  sampleID <- df$d[12]
  sampleID <- strsplit(sampleID, split = ': ')
  sampleID <- sampleID[[1]][2]
  ss <- df$d[15]
  ssp <- strsplit(ss, split = ':')
  gps <- ssp[[1]][2]
  sgps <- strsplit(gps, split = " | ")
  sdate <- sgps[[1]][2]
  stime <- sgps[[1]][4]
  xtime <- paste0(str_sub(stime, 1, 2), ":",str_sub(stime, 3, 4),":",str_sub(stime, 5, -1))
  xdatetime <- paste0(sdate, " ", xtime)
  #strptime(x, "%m/%d/%y %H:%M:%S")
  datepos <- as.POSIXct(xdatetime, tz = 'UTC')
  
  slatdeg <- as.numeric(sgps[[1]][6])
  slondeg <- as.numeric(sgps[[1]][10])
  slatdec <- as.numeric(sgps[[1]][7])/60
  slondec <- as.numeric(sgps[[1]][11])/60 
  slatdecdeg <- slatdec + slatdeg
  slondecdeg <- slondec + slondeg
  
  TSG_samples[y,1] <- xtime
  TSG_samples[y,2] <- sdate
  TSG_samples[y,3] <- xdatetime
  TSG_samples[y,4] <- slatdecdeg
  TSG_samples[y,5] <- slondecdeg
  TSG_samples[y,6] <- sampleID
  
  y=y+1
  
  }
  
  filestsg <- list.files(path= pathprocessed, pattern = 'TSGdata.*\\.csv', full.names = TRUE)
  filestsgpco2 <- list.files(path= pathprocessed, pattern = 'PCO2data.*\\.csv', full.names = TRUE)
  
  
  for (g in filestsg){
    tsgall <- data.frame(read.csv(g, header = TRUE))
    
  tsgposix  <- data.frame(time=as.POSIXct(tsgall$time, tz = 'UTC'))
  
  for (t in 1:length(TSG_samples$time)){
    
    z <- strptime(TSG_samples$POSIXctdate[t], format="%Y-%m-%d %H:%M:%OS", tz = 'UTC')
    op <- options(digits.secs = 0)
    
    for (h in 1:length(tsgposix$time)){
      
      ztsg <- strptime(tsgposix$time[h], format="%Y-%m-%d %H:%M", tz = 'UTC')
      z2 <- strptime(z, format="%Y-%m-%d %H:%M", tz = 'UTC')
      
      if (ztsg == z2){
  
        TSG_bottletable[x,1]  <- TSG_samples$POSIXctdate[t]
        TSG_bottletable[x,2]  <- tsgall$time[h]
        TSG_bottletable[x,3] <- TSG_samples$lat[t]
        TSG_bottletable[x,4] <- TSG_samples$lon[t]
        TSG_bottletable[x,5] <- TSG_samples$sampleID[t]
        TSG_bottletable[x,6] <- tsgall$salinity_PSU[h]
        TSG_bottletable[x,7] <- tsgall$Fluorescence[h]
        TSG_bottletable[x,8] <- tsgall$O2Concentration_ml_L[h]
        TSG_bottletable[x,9] <- tsgall$Temperature_TSG_ITS_90[h]
        TSG_bottletable[x,10] <- tsgall$Conductivity_S_m[h]
        TSG_bottletable[x,11] <- tsgall$FluorescenceUV[h]
        TSG_bottletable[x,12] <- tsgall$pH[h]
        # TSG_bottletable$timebottle[[x]]  <-  as.POSIXct(TSG_samples$POSIXctdate[t], tz = 'UTC')
        # TSG_bottletable$timeTSG[x]  <-  tsgposix$time[h]
        # TSG_bottletable$lat[x] <- TSG_samples$lat[t]
        # TSG_bottletable$lon[x] <- TSG_samples$lon[t]
        # TSG_bottletable$sampleID[x] <- TSG_samples$sampleID[t]
        # TSG_bottletable$TSGsal[x] <- tsgall$salinity_PSU[h]
        # TSG_bottletable$TSGChl[x] <- tsgall$Fluorescence[h]
        # TSG_bottletable$TSGOxy[x] <- tsgall$O2Concentration_ml_L[h]
        x=x+1
        
      }
    }
  }
  }
  
  tsg_sample_file <- file.path(output_dir, "TSG_samples.csv")
  write.csv(TSG_samples, file = tsg_sample_file)
  
  tsg_bottle_file <- file.path(output_dir, "TSG_bottletable2.csv")
  write.csv(TSG_bottletable, file = tsg_bottle_file)

  ####################
  fileOXY <- list.files(path= pathbottle, pattern = 'Oxy.*\\.csv', full.names = TRUE) #list of log files with path
  fileChl <- list.files(path= pathbottle, pattern = 'Chl.*\\.xlsx', full.names = TRUE) #list of log files with path
  fileSal <- list.files(path= pathbottle, pattern = 'Salts.*\\.csv', full.names = TRUE, all.files=FALSE) #list of log files with path
  
  
  OXY_samples <-  data.frame(read.csv(fileOXY, header = TRUE, sep = ",", skip = 8, stringsAsFactors = FALSE))
  Chl_samples <-  data.frame(read_excel(fileChl, col_names = TRUE, skip = 18))
  Sal_samples <-  data.frame(read.csv(fileSal, header = TRUE, sep = ",", skip = 4, stringsAsFactors = FALSE))
  #Sal_samples <- Sal_samples[-c(1), ]
  
  TSG_samples$oxy1 = c(1:length(TSG_samples$time))
  TSG_samples$oxy2 = c(1:length(TSG_samples$time))
  TSG_samples$chl1 = c(1:length(TSG_samples$time))
  TSG_samples$chl2 = c(1:length(TSG_samples$time))
  TSG_samples$sal  = c(1:length(TSG_samples$time))
  TSG_samples$oxyTSGmean  = c(1:length(TSG_samples$time))
  TSG_samples$chlTSGmean  = c(1:length(TSG_samples$time))
  TSG_samples$salTSGmean  = c(1:length(TSG_samples$time))
  TSG_samples$tempTSGmean  = c(1:length(TSG_samples$time))
  TSG_samples$conTSGmean  = c(1:length(TSG_samples$time))
  TSG_samples$UVTSGmean  = c(1:length(TSG_samples$time))
  TSG_samples$pHTSGmean  = c(1:length(TSG_samples$time))
                      
  for (k in 1:length(TSG_samples$time)){
    
    matchoxy1 <- match(paste0(TSG_samples$sampleID[k], "_1"), OXY_samples$Sample)
    matchoxy2 <- match(paste0(TSG_samples$sampleID[k], "_2"), OXY_samples$Sample)
    matchchl1 <- match(TSG_samples$sampleID[k], Chl_samples$I.D.)
    matchsal1 <- match(as.numeric(TSG_samples$sampleID[k]), as.numeric(Sal_samples$Bottle.Label))
   # matchTSG <-  match(TSG_bottletable$sampleID, as.numeric(TSG_samples$sampleID[k]))
    matchTSG <-  which(TSG_bottletable$sampleID==as.numeric(TSG_samples$sampleID[k]))
    
    TSG_oxymean <- mean(TSG_bottletable$TSGOxy[matchTSG], na.rm = TRUE)
    TSG_chlmean <- mean(TSG_bottletable$TSGChl[matchTSG], na.rm = TRUE)
    TSG_salmean <- mean(TSG_bottletable$TSGsal[matchTSG], na.rm = TRUE)
    TSG_tempmean <- mean(TSG_bottletable$TSGtemp[matchTSG], na.rm = TRUE)
    TSG_conmean <- mean(TSG_bottletable$TSGcon[matchTSG], na.rm = TRUE)
    TSG_UVmean <- mean(TSG_bottletable$TSGUV[matchTSG], na.rm = TRUE)
    TSG_pHmean <- mean(TSG_bottletable$TSGpH[matchTSG], na.rm = TRUE)
    
    # adding a new column to the data frame using $ symbol
    TSG_samples$oxy1[k] = OXY_samples$O2_Concentration.ml.l.[matchoxy1]
    TSG_samples$oxy2[k] = OXY_samples$O2_Concentration.ml.l.[matchoxy2]
    TSG_samples$chl1[k] = Chl_samples$CHL.[matchchl1]
    TSG_samples$chl2[k] = Chl_samples$CHL.[matchchl1+1]
    TSG_samples$sal[k] = Sal_samples$Calculated.Salinity[matchsal1]
    TSG_samples$oxyTSGmean[k]  = TSG_oxymean
    TSG_samples$chlTSGmean[k]  = TSG_chlmean
    TSG_samples$salTSGmean[k]  = TSG_salmean
    
    TSG_samples$tempTSGmean[k]  = TSG_tempmean
    TSG_samples$conTSGmean[k]  = TSG_conmean
    TSG_samples$UVTSGmean[k]  = TSG_UVmean
    TSG_samples$pHTSGmean[k]  = TSG_pHmean
    
  }
  
  write_delim(TSG_samples, file = "4comparesamples/Bottle_TSG_QAT", delim = ",")
  write.csv(TSG_samples, file = "4comparesamples/Bottle_TSG_QAT.csv")
  
  # 1. Open jpeg file, set the path, name and the image size
  jpeg(filename = paste0(getwd() ,"/4comparesamples/salt2sgplot.jpeg"), width = 1150, height = 750)
  # 2. Create a plot
  plot(TSG_samples$sampleID,TSG_samples$sal, col = "green", pch = 19, bg="green", cex=4, 
       ylim=c(round(min(as.numeric(TSG_samples$salTSGmean),na.rm = TRUE )),round(max(as.numeric(TSG_samples$salTSGmean),na.rm = TRUE ))),
       main="salinity, green=sample, red=TSG", ylab="salinity",xlab="sample ID ",xaxt="n")
  points(TSG_samples$sampleID,TSG_samples$salTSGmean, col = "dark red", pch = 19, bg="red", cex=2)
  axis(side = 1, at = round(min(as.numeric(TSG_samples$sampleID),na.rm = TRUE )):round(max(as.numeric(TSG_samples$sampleID),na.rm = TRUE )))
  # 3. Close the pdf file
  dev.off()
  
  # 1. Open jpeg file, set the path, name and the image size
  jpeg(paste0(getwd(),"/4comparesamples/oxy2tsgplot.jpeg"), width = 1150, height = 750)
  # 2. Create a plot
  plot(TSG_samples$sampleID,TSG_samples$oxy1, col = "green", pch = 19, bg="green", cex=3, ylim=c(4.5,round(max(as.numeric(TSG_samples$oxy1),na.rm = TRUE ))),
       main="oxygen, green/blue=sample, red=TSG", ylab="O2 Concentration (ml-L)",xlab="sample ID",xaxt="n")
  axis(1, at = round(min(as.numeric(TSG_samples$sampleID),na.rm = TRUE )):round(max(as.numeric(TSG_samples$sampleID),na.rm = TRUE )))
  points(TSG_samples$sampleID,TSG_samples$oxy2, col = "blue", pch = 19, bg="blue", cex=2)
  points(TSG_samples$sampleID,TSG_samples$oxyTSGmean, col = "dark red", pch = 19, bg="red", cex=2)
  # 3. Close the pdf file
  dev.off()
  
  # 1. Open jpeg file, set the path, name and the image size
  jpeg(paste0(getwd(),"/4comparesamples/chl2tsgplot.jpeg"), width = 1150, height = 750)
  # 2. Create a plot
  plot(TSG_samples$sampleID,TSG_samples$chl1, col = "green", pch = 19, bg="green", cex=3, ylim=c(0,round(max(as.numeric(TSG_samples$chlTSGmean),na.rm = TRUE ))+2),
       main="Chlorophyl, green/blue=sample, red=TSG", ylab="Chlorophyl",xlab="sample ID",xaxt="n")
  axis(1, at = round(min(as.numeric(TSG_samples$sampleID),na.rm = TRUE )):round(max(as.numeric(TSG_samples$sampleID),na.rm = TRUE )))
  points(TSG_samples$sampleID,TSG_samples$chl2, col = "blue", pch = 19, bg="blue", cex=2)
  points(TSG_samples$sampleID,TSG_samples$chlTSGmean, col = "dark red", pch = 19, bg="red", cex=2)
  
  # 3.Close the pdf file
  dev.off()

}
